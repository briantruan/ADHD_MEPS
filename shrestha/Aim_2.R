# Aim 2 medication utilization 
# 
# Number of people with any medication for ADHD
# 
# Days supply of any medication for ADHD
# 
# Cost OOP and total for any medication for ADHD
# 
# total estimates as well as person level average estimates 
# 
# 

library(MEPS)
library(survey)
library(tidyverse)

meds_function = function(year = year){
  options(survey.lonely.psu='adjust')
  options(digits = 10)
  
  # var info for RX
  # RXREDICX:           ENVTIDX     RXDRGNAM      RXXPxxX:out of pocket RXSDxxX:sum of payments
  
  wtvar <- paste0("PERWT", substr(year, 3, 4), "F")
  
  rx_oop_var <- paste0("RXSF", substr(year, 3, 4), "X")
  
  rx_tot_var <- paste0("RXXP", substr(year, 3, 4), "X")
  
  povcat_var <- paste0("POVCAT", substr(year,3,4))
  
  
  pmed = read_MEPS(year=year, type = "RX") %>% 
    rename(EVNTIDX = LINKIDX) %>% 
    select(DUPERSID, RXRECIDX, EVNTIDX, RXDAYSUP, RXDRGNAM, TC1S1, !!sym(rx_oop_var), !!sym(rx_tot_var)) %>%
    filter(TC1S1 == 71) %>%
    mutate(daysup_imput = if_else(RXDAYSUP %in% c(-7,-8),
                                  mean(RXDAYSUP[!(RXDAYSUP %in% c(-7,-8))], ra.rm=TRUE),
                                  RXDAYSUP))
  
  cond = read_MEPS(year = year, type = "COND") %>%
    select(DUPERSID, CONDIDX, ICD10CDX) %>%
    filter(ICD10CDX =="F90")
  
  cond_flag = cond %>% 
    select(DUPERSID) %>%
    mutate(ADHD_flag = 1)
  
  fyc = read_MEPS(year = year, type ="FYC") %>%
    select(DUPERSID, AGELAST, SEX, RACETHX, !!sym(povcat_var), VARSTR, VARPSU, !!sym(wtvar))
  
  clnk = read_MEPS(year = year, type = "CLNK") 
  
  #merge condition with clink
  cond_clnk = cond %>% inner_join(clnk, by = c("DUPERSID", "CONDIDX")) %>% 
    distinct(PANEL, DUPERSID, EVNTIDX, ICD10CDX, EVENTYPE)
  
  #QC Check should be 0 duplicate EVNTIDX
  cond_clnk %>% pull(EVNTIDX) %>% duplicated() %>% sum
  
  #merge cond_clnk with PMED
  cond_pmed = cond_clnk %>% inner_join(pmed, by = c("DUPERSID", "EVNTIDX"))
  
  #check eventype
  cond_pmed %>% count(EVENTYPE)
  
  
  #Rolling up to person level
  # pers_cond_pmed = cond_pmed %>%
  #   group_by(DUPERSID) %>%
  #   summarize(
  #     num_fills = n_distinct(RXRECIDX),
  #     tot_exp = sum(!!sym(rx_tot_var)),
  #     oop_exp = sum(!!sym(rx_oop_var)),
  #     day_sup = sum(daysup_imput)
  #   ) %>%
  #   mutate(ADHD_med_flag = 1)
 
   pers_cond_pmed <- cond_pmed %>%
    group_by(DUPERSID, RXDRGNAM) %>%
    summarize(
      num_fills = n_distinct(RXRECIDX),
      day_sup   = sum(daysup_imput, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from  = RXDRGNAM,
      values_from = c(num_fills, day_sup),
      values_fill = 0   # fill missing with 0
    ) %>%
    left_join(
      cond_pmed %>%
        group_by(DUPERSID) %>%
        summarize(
          num_fills = n_distinct(RXRECIDX),
          day_sup = sum(daysup_imput, na.rm = TRUE),
          tot_exp = sum(!!sym(rx_tot_var), na.rm = TRUE),
          oop_exp = sum(!!sym(rx_oop_var), na.rm = TRUE),
          .groups = "drop"
        ),
      by = "DUPERSID"
    ) %>%
    mutate(ADHD_med_flag = 1,
           multifill = as.integer(rowSums(across(starts_with("num_fills_")) >= 1) >= 2))
  
  fyc_pers_cond_med = fyc %>% 
    full_join(pers_cond_pmed, by="DUPERSID") %>%
    replace_na(list(ADHD_med_flag = 0)) %>%
    full_join(cond_flag, by = "DUPERSID") %>%
    replace_na(list(ADHD_flag = 0))
  
  
  # survey design
  
  meps_dsgn = svydesign(
    id = ~VARPSU,
    strata = ~VARSTR,
    weights = as.formula(paste0("~", wtvar)),
    data = fyc_pers_cond_med,
    nest = TRUE
  )


  adhd_desgn = subset(meps_dsgn, ADHD_flag == 1)

  adhd_meds_desgn = subset(meps_dsgn, ADHD_med_flag == 1)
  
  print(svymean(~ADHD_med_flag,
                design = meps_dsgn))
  
  print(svymean(~ADHD_med_flag,
               design = adhd_desgn))
    
  
  print(svymean(~multifill+
                  tot_exp +
                  oop_exp +
                  num_fills +
                  day_sup +
                  day_sup_METHYLPHENIDATE +
                  day_sup_LISDEXAMFETAMINE +
                  `day_sup_AMPHETAMINE-DEXTROAMPHETAMINE`, design = adhd_meds_desgn))
  # 
  # 
  # # Total number of people with ADHD
  # print(svytotal(~ADHD_flag, 
  #           design = meps_dsgn))
  # 
  # 
  # print(svymean(~ADHD_flag,
  #          design = meps_dsgn))
  # 
  # print(svyby(~ADHD_flag,
  #             by=~SEX, 
  #             FUN = svymean,
  #             design = meps_dsgn))
  # 
  # print(svyby(~ADHD_flag,
  #             by=~RACETHX, 
  #             FUN = svymean,
  #             design = meps_dsgn))
  # 
  # print(svyby(~ADHD_flag,
  #             by=as.formula(paste0("~",povcat_var)), 
  #             FUN = svymean,
  #             design = meps_dsgn))
  # 
  # # among people with ADHD who has medication by gender, race, poverty
  # 
  # print(svyby(~ADHD_med_flag,
  #             by = ~SEX,
  #             FUN = svymean,
  #             design = adhd_desgn))
  # 
  # print(svyby(~ADHD_med_flag,
  #             by = ~RACETHX,
  #             FUN = svymean,
  #             design = adhd_desgn))
  # 
  # print(svyby(~ADHD_med_flag,
  #             by = as.formula(paste0("~", povcat_var)),
  #             FUN = svymean,
  #             design = adhd_desgn))
  # 
  # 
  # #national Total for people with meds
  # 
  # print(svytotal(~ADHD_med_flag +
  #            num_fills +
  #            tot_exp +
  #            oop_exp +
  #            day_sup, design = adhd_meds_desgn))
  
  # return(fyc_pers_cond_med)
  
}

meds_function(year = 2020) 

for (yr in 2016:2022) {
  cat("===== Year:", yr, "=====\n")
  meds_function(year = yr)
  cat("\n")
}


#names###################
meps_dsgn = svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT20F,
  data = fyc_pers_cond_med,
  nest = TRUE
)

adhd_desgn = subset(meps_dsgn, ADHD_flag == 1)

adhd_meds_desgn = subset(meps_dsgn, ADHD_med_flag == 1)


print(svymean(~multifill+
                 tot_exp +
                 oop_exp +
                 day_sup_METHYLPHENIDATE, design = adhd_meds_desgn))

print(svyby(~ADHD_med_flag +
              num_fills +
              tot_exp +
              oop_exp +
              day_sup,
            by=~RACETHX,
            FUN=svymean,
            design = adhd_meds_desgn))

ADHD_med = fyc_pers_cond_med %>% filter(ADHD_med_flag==1)

view(ADHD_med)


meds = read_MEPS(year = 2020, type ="RX")

meds_ADHD = meds %>% filter(TC1S1 ==71)

view(meds_ADHD)

df_summary <- meds_ADHD %>%
  group_by(DUPERSID) %>%
  summarise(
    drug_list = str_c(RXDRGNAM, collapse = ", "),
    all_same = as.integer(n_distinct(RXDRGNAM) == 1),
    .groups = "drop"
  ) %>%
  filter(all_same == 0)


nrow(df_summary)

df_flag <- meds_ADHD %>%
  group_by(DUPERSID) %>%
  mutate(all_same = n_distinct(RXNAME) == 1) %>%
  ungroup()

table(df_flag$all_same)

view(df_flag)
