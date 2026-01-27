# Goal
# 
# Replicate findings by Jeun et al. (2023)
# doi.org/10.1177/10870547231210284

load("data/meps_jeun_unfiltered.rda")

# =================================
# exclusions
# =================================

# 0. starting n
n0_unw <- nrow(unfiltered_data)
n0_w   <- sum(unfiltered_data$PERWT)   # if PERWT already pooled, or PERWTyyF

# 1. ADHD diagnosis
step1 <- unfiltered_data %>%
  filter(ADHD == TRUE)
n1_unw  <- nrow(step1)
n1_w    <- sum(step1$PERWT)

# 2. age ≥18
step2 <- step1 %>%
  filter(AGE53X >= 18)
n2_unw <- nrow(step2)
n2_w   <- sum(step2$PERWT)

# 3. ADHD diagnosis AND prescription drug
step3 <- step2 %>%
  filter(n_fills > 0)
n3_unw <- nrow(step3)
n3_w   <- sum(step3$PERWT)

# 4. non-missing supply days (total_days > 0)
step4 <- step3 %>%
  filter(total_days > 0)
n4_unw <- nrow(step4)
n4_w   <- sum(step4$PERWT)

# 5. non-missing PDC (implies valid month/year)
step5 <- step4 %>%
  filter(!is.na(avg_PDC))
n5_unw <- nrow(step5)
n5_w   <- sum(step5$PERWT)

# final analytic data
cleaned_data <- step5
nrow(cleaned_data)

flow_table <- tibble(
  step = 1:5,
  description = c("ADHD diagnosis",
                  "Age ≥18",
                  "ADHD + ≥1 ADHD Rx",
                  "Non-missing supply days",
                  "Non-missing PDC"),
  unweighted_n = c(n1_unw, n2_unw, n3_unw, n4_unw, n5_unw),
  weighted_n   = c(n1_w,   n2_w,   n3_w,   n4_w,   n5_w)
)
flow_table

# =================================
# survey design
# =================================

# Create pooled weight
cleaned_data <- cleaned_data %>%
  dplyr::ungroup() %>%
  mutate(PERWT_POOL = PERWT / 7)

meps_design <- svydesign(
  id     = ~VARPSU,
  strata = ~VARSTR,
  weights= ~PERWT_POOL,
  data   = cleaned_data,
  nest   = TRUE
)

# =================================
# summary statistics
# =================================
weighted_n <- sum(weights(meps_design))
weighted_n

weighted_adherence <- svytable(~ADHERENT, design = meps_design)
adherence_df <- as.data.frame(weighted_adherence) %>%
  mutate(
    Wn = Freq,
    Wpct = 100 * Wn / sum(Wn)
  )

make_table_block <- function(var, design) {
  f <- as.formula(paste0("~", var, " + ADHERENT"))
  tab <- svytable(f, design)

  as.data.frame(tab) %>%
    group_by(ADHERENT) %>%
    mutate(
      Wn   = Freq,
      Wpct = 100 * Wn / sum(Wn)
    ) %>%
    ungroup()
}

sex_block     <- make_table_block("SEX",      meps_design)
age_block     <- make_table_block("AGE_CAT",  meps_design)
race_block    <- make_table_block("RACETHX",  meps_design)
marital_block <- make_table_block("MARITAL",  meps_design)
region_block  <- make_table_block("REGION53", meps_design)
edu_block     <- make_table_block("EDUC",     meps_design)
income_block  <- make_table_block("INCOME",   meps_design)
poverty_block <- make_table_block("POVERTY",  meps_design)
inscov_block  <- make_table_block("INSCOV",   meps_design)

income_block  <- svyby(~TTLP, ~ADHERENT, meps_design, svymean)
avgpdc_block  <- svyby(~avg_PDC, ~ADHERENT, meps_design, svymean)
oop_block     <- svyby(~TOTSLF, ~ADHERENT, meps_design, svymean)
pcs_block     <- svyby(~PCS, ~ADHERENT, meps_design, svymean)
mcs_block     <- svyby(~MCS, ~ADHERENT, meps_design, svymean)

# =================================
# logistic regression on adherence
# =================================

adherence_glm <- svyglm(
  ADHERENT ~ SEX + AGE_CAT + RACE + MARITAL + REGION53 + 
    EDUC + INCOME + POVERTY + INSCOV + any_new + IS_ER + 
    COMORBID_CAT,
  design = meps_design,
  family = binomial()
)

summary(adherence_glm)

logit_ci <- confint(adherence_glm)               # 95% CIs on log-odds scale
logit_est <- coef(adherence_glm)

or_table <- data.frame(
  term   = names(logit_est),
  OR     = exp(logit_est),
  OR_low = exp(logit_ci[, 1]),
  OR_high= exp(logit_ci[, 2]),
  row.names = NULL
)

or_table_fmt <- or_table %>%
  mutate(
    OR_CI = sprintf("%.2f (%.2f, %.2f)", OR, OR_low, OR_high)
  ) %>%
  select(term, OR_CI)

or_table_fmt

# =================================
# linear regression on utilization based on adherence
# =================================

# office visits
ob_glm <- svyglm(
  COND_OB ~ avg_PDC,
  design = meps_design,
  family = gaussian()
)

summary(ob_glm)

# ip nights
ip_glm <- svyglm(
  COND_IP ~ avg_PDC,
  design = meps_design,
  family = gaussian()
)

summary(ip_glm)

# er visits
er_glm <- svyglm(
  COND_ER ~ avg_PDC,
  design = meps_design,
  family = gaussian()
)

summary(er_glm)

# home health visits
hh_glm <- svyglm(
  COND_HH ~ avg_PDC,
  design = meps_design,
  family = gaussian()
)

summary(hh_glm)