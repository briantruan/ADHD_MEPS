# Goal: Change in diagnosis-attributed medication use (fills) as a function of COVID-19 as the primary policy shock - multiple regressions analysis

# adhd_rx_n is outcome variable, year is main independent variable

test_model <- svyglm(
  adhd_rx_n ~ year,
  design = meps_design_final
)
summary(test_model)

reg_kitchen_sink <- svyglm(
  log(1+adhd_oop) ~ year + AGE53X + sex + ethnicity + marital + 
                     education + has_insurance + 
                     income_2021,
  design = meps_design_final
)
summary(reg_kitchen_sink)

reg_1 <- svyglm(
  adhd_total_spend ~ year + AGE53X + sex + ethnicity + marital + 
                     education + has_insurance + medicaid + 
                     income_2021,
  design = meps_design_final
)
summary(reg_1)

reg_2 <- svyglm(
  adhd_total_spend ~ year + sex + ethnicity + marital + 
                     education + has_insurance + medicaid + 
                     income_2021,
  design = meps_design_final
)
summary(reg_2)

reg_3 <- svyglm(
  adhd_total_spend ~ year + sex + marital + 
                     education + has_insurance + medicaid + 
                     income_2021,
  design = meps_design_final
)
summary(reg_3)

reg_4 <- svyglm(
  adhd_total_spend ~ year + sex + marital + 
                     has_insurance + medicaid + 
                     income_2021,
  design = meps_design_final
)
summary(reg_4)

reg_5 <- svyglm(
  adhd_total_spend ~ year + marital + 
                     has_insurance + medicaid + 
                     income_2021,
  design = meps_design_final
)
summary(reg_5)

reg_6 <- svyglm(
  adhd_total_spend ~ year + marital + medicaid + 
                     income_2021,
  design = meps_design_final
)
summary(reg_6)

reg_7 <- svyglm(
  adhd_total_spend ~ year + marital + medicaid,
  design = meps_design_final
)
summary(reg_7)

reg_8 <- svyglm(
  adhd_total_spend ~ year + medicaid,
  design = meps_design_final
)
summary(reg_8)

reg_9 <- svyglm(
  adhd_total_spend ~ year,
  design = meps_design_final
)
summary(reg_9)

add_reg <- svyglm(
  adhd_total_spend ~ year + AGE53X + medicaid + POVCAT,
  design = meps_design_final,
  subset = adhd_any_rx == "Yes"
)
summary(add_reg)

# 14. estimates
svyby(
  ~flag_adhd_dx,
  ~year,
  design = meps_design_adhd,
  FUN = svytotal,
  na.rm = TRUE,
  vartype = c("se")
)

svyby(
  ~AGE53X,
  ~year,
  design = meps_design_adhd,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se")
)

svyby(
  ~adhd_fills + adhd_total_spend + adhd_oop + adhd_private + adhd_medicaid,
  ~year,
  design = meps_design_adhd_fills_T,
  FUN = svymean,
  na.rm = TRUE,
  vartype = "se"
)

# mann-whitney u test
svyranktest(adhd_fills ~ year, design = meps_design_adhd)
svyranktest(adhd_total_spend ~ year, design = meps_design_adhd)
svyranktest(adhd_oop ~ year, design = meps_design_adhd)
svyranktest(adhd_private ~ year, design = meps_design_adhd)
svyranktest(adhd_medicaid ~ year, design = meps_design_adhd)

# variable of interest
dep_var <- "adhd_oop_log"

# glm
modelt1 <- svyglm(as.formula(paste(dep_var, "~ post_covid")),
                design = meps_design_adhd, family = quasipoisson())
summary(model1)$coefficients

# step-wise model with these vars: + sex + AGE53X + ethnicity + marital + education + has_insurance
modelt2 <- svyglm(as.formula(paste(dep_var, "~ post_covid + sex")),
                design = meps_design_adhd, family = quasipoisson())
summary(modelt2)$coefficients

modelt3 <- svyglm(as.formula(paste(dep_var, "~ post_covid + AGE53X")),
                design = meps_design_adhd, family = quasipoisson())
summary(modelt3)$coefficients

modelt4 <- svyglm(as.formula(paste(dep_var, "~ post_covid + ethnicity")),
                design = meps_design_adhd, family = quasipoisson())
summary(modelt4)$coefficients

modelt5 <- svyglm(as.formula(paste(dep_var, "~ post_covid + marital")),
                design = meps_design_adhd, family = quasipoisson())
summary(modelt5)$coefficients

modelt6 <- svyglm(as.formula(paste(dep_var, "~ post_covid + education")),
                design = meps_design_adhd, family = quasipoisson())
summary(modelt6)$coefficients

modelt7 <- svyglm(as.formula(paste(dep_var, "~ post_covid + has_insurance")),
                design = meps_design_adhd, family = quasipoisson())
summary(modelt7)$coefficients

modelt8 <- svyglm(as.formula(paste(dep_var, "~ post_covid + income_2021")),
                design = meps_design_adhd, family = quasipoisson())
summary(modelt8)$coefficients

# ---- MODEL BUILDING ----
modelf1 <- svyglm(as.formula(paste(dep_var, "~ post_covid + has_insurance")),
                design = meps_design_adhd, family = quasipoisson())
summary(modelf1)$coefficients

modelf2 <- svyglm(as.formula(paste(dep_var, "~ post_covid + has_insurance + income_2021")),
                design = meps_design_adhd, family = quasipoisson())
summary(modelf2)$coefficients

modelf3 <- svyglm(as.formula(paste(dep_var, "~ post_covid + has_insurance + marital + AGE53X")),
                design = meps_design_adhd, family = quasipoisson())
summary(modelf3)$coefficients

modelf4 <- svyglm(as.formula(paste(dep_var, "~ post_covid + has_insurance + marital + AGE53X + education")),
                design = meps_design_adhd, family = quasipoisson())
summary(modelf4)$coefficients