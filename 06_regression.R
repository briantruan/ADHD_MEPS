# Goal: Change in diagnosis-attributed medication use (fills) as a function of COVID-19 as the primary policy shock - multiple regressions analysis

# adhd_rx_n is outcome variable, year is main independent variable

test_model <- svyglm(
  adhd_rx_n ~ year,
  design = meps_design_final
)
summary(test_model)

reg_kitchen_sink <- svyglm(
  adhd_total_spend ~ year + AGE53X + sex + ethnicity + marital + 
                     education + has_insurance + medicaid + 
                     medicare + income_2021,
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
