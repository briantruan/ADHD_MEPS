#Type of visits associated with medical conditions

library(MEPS)
library(dplyr)
library(survey)
library(tidyverse)

all_years_data <- NULL

OB_function = function(year = year){
  
  # accounting for some variables that change their names in each years dataset
  
  wtvar <- paste0("PERWT", substr(year, 3, 4), "F")
  
  povcat_var <- paste0("POVCAT", substr(year,3,4))
  
  options(survey.lonely.psu='adjust')
  
  # Build weight variable name based on year (last 2 digits)
  wtvar <- paste0("PERWT", substr(year, 3, 4), "F")
  
  fyc = read_MEPS(year = year, type = "FYC") %>%
    select(DUPERSID, VARSTR, VARPSU, !!sym(wtvar), SEX, !!sym(povcat_var), RACETHX, AGELAST)
  print("READ FYC Successful")
  
  cond = read_MEPS(year = year, type = "COND") %>%
    select(DUPERSID, CONDIDX, ICD10CDX, AGEDIAG) %>%
    filter(ICD10CDX == "F90") %>%
    mutate(ADHD = 1)
  # need to keep all eve
  print("READ cond Successful")
  print(names(cond))
  
  cond_ADHD <- cond %>%
    filter(ADHD == 1) %>%
    select(DUPERSID, ADHD, AGEDIAG) %>%
    mutate(AGEDIAG_valid = ifelse(AGEDIAG >= 0 & AGEDIAG <= 85, AGEDIAG, NA)) %>% 
    group_by(DUPERSID) %>%
    summarize(
      ADHD = first(ADHD),                  # all 1
      AGEDIAG = ifelse(all(is.na(AGEDIAG_valid)), NA, min(AGEDIAG_valid, na.rm = TRUE)),
      .groups = "drop"
    )
  
  clnk = read_MEPS(year = year, type = "CLNK") %>%
    filter(EVENTYPE ==1)
  print("READ clnk Successful")
  print(names(clnk))
  
  ob = read_MEPS(year = year, type = "OB")
  print("READ OB Successful")
  print(names(ob))
  
  # joining clnk file with the condition file and keeping unique EVNTIDX
  cond_clnk = clnk %>% inner_join(cond, by = c("DUPERSID", "CONDIDX")) %>%
    distinct(PANEL, DUPERSID, EVNTIDX, ICD10CDX, EVENTYPE)
  print("join cond and clnk successful")
  print(names(cond_clnk))
  
  # joining clnk_cond to ob file
  cond_clnk_ob = cond_clnk %>% 
    inner_join(ob, by = c("DUPERSID", "EVNTIDX"))
  print("join cond_clnk with OB successful")
  
  
  # converting to person level
  # calculate total number of OB visit
  # calculate total number of OB visit which is telehealth
  # calculate total paid (OBXP23X) lets not do this because its a categorical value group
  pers_cond_clnk_ob = cond_clnk_ob %>%
    group_by(DUPERSID) %>%
    summarize(
      total_ob = n_distinct(EVNTIDX),
      # total_tele = sum(TELEHEALTHFLAG ==1, ra.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(ADHD_visit = 1)
  print("conversion to person level successful")
  #now joining it with fyc file and also the cond file. because there may be a cond file not associated with an OB visit (unlikely but possible)
  
  fyc_pers_cond_clnk_ob = fyc %>% 
    left_join(pers_cond_clnk_ob, by=c("DUPERSID")) %>%
    replace_na(list(ADHD_visit=0)) %>%
    left_join(cond_ADHD, by=c("DUPERSID")) %>%
    replace_na(list(ADHD = 0))
  print("join with fyc successful")
  
  print(names(fyc_pers_cond_clnk_ob))
  
  mepsdsgn = svydesign(
    id = ~VARPSU,
    strata = ~VARSTR,
    weights = as.formula(paste0("~", wtvar)),
    data = fyc_pers_cond_clnk_ob,
    nest = TRUE)
  
  mepsdesgn_adhd = subset(mepsdsgn, ADHD_visit==1)
  
  
  # proportion of people who had any ADHD OB visit
  print(svymean(~ADHD_visit
                , design = mepsdsgn))
  
  # Average number of visit for ADHD for people who had any visit
  print(svymean(~total_ob,
                 # total_tele, 
                design = mepsdesgn_adhd))
  
  # average number of visit for ADHD for people who had any visit by sex
  print(svyby(~total_ob,
                # total_tele,
              by=~SEX,
              FUN =svymean,
              design = mepsdesgn_adhd ))
  
  fyc_yr = fyc_pers_cond_clnk_ob %>%
    # rename(PERWT = !!sym(paste0("PERWT",substr(yr,3,4),"F"))) %>%
    mutate(year = year)
  
  
  return(fyc_yr)
}

mepsdsgn <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT20F,
  data = y,
  nest = TRUE
)

print(svymean(~AGEDIAG,
              # total_tele, 
              design = subset(mepsdsgn, ADHD==1)))
data = y %>% filter(ADHD==1) %>% filter(AGELAST>20)

table(data$AGEDIAG)

table(data$AGELAST)

y = OB_function(year = 2020)



for (yr in 2016:2022) {
  cat("===== Year:", yr, "=====\n")
  out = OB_function(year = yr)
  
  if(is.null(all_years_data)){
    all_years_data = out
  } else {
    all_years_data = bind_rows(all_years_data, out)
  }
  cat("\n")
  }


mepsdsgn <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT,
  data = all_years_data,
  nest = TRUE
)

print(names(all_years_data))
model_trend <- svyglm(ADHD_visit ~ as.numeric(year),
                      design = mepsdsgn,
                      family = quasibinomial())
summary(model_trend)

model_years <- svyglm(ADHD_visit ~ factor(year),
                      design = mepsdsgn,
                      family = quasibinomial())
summary(model_years)

# Wald test for overall year effect
regTermTest(model_years, ~factor(year))

pred <- svyby(~ADHD_visit, ~year + SEX, 
              design = mepsdsgn, 
              svymean, vartype = c("se", "ci"))
pred

library(ggplot2)

ggplot(pred, aes(x = year, y = ADHD_visit)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.2) +
  labs(x = "Year", y = "Proportion with any outpatient visit") +
  theme_minimal()

pred$SEX <- factor(pred$SEX, levels = c(1, 2), labels = c("Male", "Female"))

ggplot(pred, aes(x = year, y = ADHD_visit, color = SEX)) +
  geom_point(size = 3) +
  geom_line(aes(group = SEX), size = 1) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.2) +
  labs(
    x = "Year",
    y = "Proportion with ADHD visit",
    color = "Sex",
    title = "ADHD outpatient visits by year and sex"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "top"
  )


pred <- svyby(~ADHD, ~year + SEX, 
              design = mepsdsgn, 
              svymean, vartype = c("se", "ci"))
pred
pred$SEX <- factor(pred$SEX, levels = c(1, 2), labels = c("Male", "Female"))

ggplot(pred, aes(x = year, y = ADHD, color = SEX)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.2) +
  labs(x = "Year", y = "Proportion with any outpatient visit") +
  theme_minimal()

ggplot(pred, aes(x = year, y = ADHD, color = SEX)) +
  geom_point(size = 3) +
  geom_line(aes(group = SEX), size = 1) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.2) +
  labs(
    x = "Year",
    y = "Proportion with ADHD",
    color = "Sex",
    title = "ADHD Condition by year and sex"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "top"
  )


pred <- svyby(~AGEDIAG, ~year, 
              design = subset(mepsdsgn, ADHD==1), 
              svymean, vartype = c("se", "ci"))

pred

pred <- svyby(~AGEDIAG, ~year + SEX, 
              design = mepsdsgn, 
              svymean, vartype = c("se", "ci"))
pred$SEX <- factor(pred$SEX, levels = c(1, 2), labels = c("Male", "Female"))
pred


ggplot(pred, aes(x = year, y = ADHD, color = SEX)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.2) +
  labs(x = "Year", y = "Proportion with any outpatient visit") +
  theme_minimal()

ggplot(pred, aes(x = year, y = ADHD, color = SEX)) +
  geom_point(size = 3) +
  geom_line(aes(group = SEX), size = 1) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.2) +
  labs(
    x = "Year",
    y = "Proportion with ADHD",
    color = "Sex",
    title = "ADHD Condition by year and sex"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "top"
  )
