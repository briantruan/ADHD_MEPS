#Goal: Did ADHD medication fills change from 2019 to 2021 after accounting for demographics and socioeconomic factors?
# adhd_rx_n is outcome variable, year is main independent variable

#load packages
library(survey)
library(dplyr)
library(gtsummary)

#Model 1- Unadjusted
model1 <- svyglm(
  adhd_fills ~ year,
  design = meps_design_subset_adhd,
  family = quasipoisson()
)

summary(model1)

#Model 2 - Adding Age
model2 <- svyglm(
  adhd_fills ~ year + AGE53X,
  design = meps_design_subset_adhd,
  family = quasipoisson()
)

summary(model2)


#Model 3 - Adding Sex
model3 <- svyglm(
  adhd_fills ~ year + AGE53X + sex,
  design = meps_design_subset_adhd,
  family = quasipoisson()
)

summary(model3)

#Model 4 - Adding Race
model4 <- svyglm(
  adhd_fills ~ year + AGE53X + sex + race,
  design = meps_design_subset_adhd,
  family = quasipoisson()
)

summary(model4)

#Model 5 - Adding Ethnicity
model5 <- svyglm(
  adhd_fills ~ year + AGE53X + sex + ethnicity,
  design = meps_design_subset_adhd,
  family = quasipoisson()
)

summary(model5)

#cannot run race and ethicity due to too much overlap

#Model 6 - Adding Education
model6 <- svyglm(
  adhd_fills ~ year + AGE53X + sex + race + education,
  design = meps_design_subset_adhd,
  family = quasipoisson()
)

summary(model6)

#Model 7 - Adding Insurance
model7 <- svyglm(
  adhd_fills ~ year + AGE53X + sex + race + education + has_insurance,
  design = meps_design_subset_adhd,
  family = quasipoisson()
)

summary(model7)

#Model 8 - Adding Income (Final)
model8 <- svyglm(
  adhd_fills ~ year + AGE53X + sex + race + education + has_insurance + POVCAT,
  design = meps_design_subset_adhd,
  family = quasipoisson()
)

summary(model8)


model9_simple <- svyglm(
  adhd_fills ~ year + AGE53X + sex + race,
  design = meps_design_subset_adhdfills,
  family = quasipoisson()
)
summary(model9)
exp(coef(model9))
confint.default(model9)
ci <- exp(confint.default(model9))

#Converting to IRRS
exp(coef(model8))
# ADHD prescription fills in 2021 were about 2.5% higher than in 2019. For each additional year of age, ADHD fills increase by about 1.2%. 
# Males had about 3% higher ADHD fill rates than females. Black participants had about 58% lower ADHD fill rates than White participants.
# Other race groups had about 64% lower fills than White participants. People with high school education or less had about 43% lower 
# ADHD fill compared with college-educated individuals. 21% higher fill rates than Medicaid-only. "Other public" 71% lower fill 
# compared to Medicaid-only. "Private- only" had 45% lower fill rates than Medicaid-only. "Uninsured" had 31% lower fill rates than 
# Medicaid-only. Income has almost no effect per $1 increase.

#Confidence Intervals
confint.default(model8)
ci <- exp(confint.default(model8))


#Results Table

coef_table <- coef(summary(model8))

results <- data.frame(
  term = names(coef(model8)),
  IRR = exp(coef(model8)),
  CI_low = exp(confint.default(model8)[,1]),
  CI_high = exp(confint.default(model8)[,2]),
  p_value = 2 * pt(
    abs(coef_table[, "t value"]),
    df = degf(meps_design_subset_adhd),
    lower.tail = FALSE
  )
)

results

# Age: IRR = 1.01, p = 0.13 ; not significant.
# Male sex: IRR = 1.03, p = 0.89; not significant.
# Black race: IRR = 0.42, p = 0.002; significantly lower ADHD fill rate compared with White participants.
# Other race: IRR = 0.36, p < 0.001; significantly lower ADHD fill rate compared with White participants.
# High school or less: IRR = 0.57, p = 0.062; lower fills, but just above conventional significance.
# Other public insurance: IRR = 0.29, p = 0.012;  significantly lower fills compared with the reference insurance group.
# Private only insurance: IRR = 0.55, p = 0.021; significantly lower fills compared with the reference insurance group.
# Uninsured: IRR = 0.69, p = 0.19; not significant.
# Income: IRR ≈ 1.00, p = 0.55; no clear association.
