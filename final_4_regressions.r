#Model 1- Unadjusted
model1 <- svyglm(
  adhd_fills ~ year,
  design = design_step4,
  family = quasipoisson()
)

summary(model1)

#Model 2 - Adding Age
model2 <- svyglm(
  adhd_fills ~ year + age_group,
  design = design_step4,
  family = quasipoisson()
)

summary(model2)

#Model 3 - Adding Sex
model3 <- svyglm(
  adhd_fills ~ year + age_group + sex,
  design = design_step4,
  family = quasipoisson()
)

summary(model3)

#Model 4 - Adding Race
model4 <- svyglm(
  adhd_fills ~ year + age_group + sex + race,
  design = design_step4,
  family = quasipoisson()
)

summary(model4)

#Model 5 - Adding Ethnicity
model5 <- svyglm(
  adhd_fills ~ year + age_group + sex + ethnicity,
  design = design_step4,
  family = quasipoisson()
)

summary(model5)

#cannot run race and ethicity due to too much overlap

#Model 6 - Adding Education
model6 <- svyglm(
  adhd_fills ~ year + age_group + sex + race + education,
  design = design_step4,
  family = quasipoisson()
)

summary(model6)

#Model 7 - Adding Insurance
model7 <- svyglm(
  adhd_fills ~ year + age_group + sex + race + education + has_insurance,
  design = design_step4,
  family = quasipoisson()
)

summary(model7)

#Model 8 - Adding Income (Final)
model8 <- svyglm(
  adhd_fills ~ year + age_group + sex + race + education + has_insurance 
                    + povcat + year:has_insurance,
  design = design_step4,
  family = quasipoisson()
)

summary(model8)

#Converting to IRRS
exp(coef(model8))

# these are probably not accurate:

# ADHD prescription fills in 2021 were about 2.5% higher than in 2019. 
# For each additional year of age, ADHD fills increase by about 1.2%. 
# Males had about 3% higher ADHD fill rates than females. 
# Black participants had about 58% lower ADHD fill rates than White.
# Other race groups had about 64% lower fills than White participants. 
# People with high school education or less had about 43% lower 
# ADHD fill compared with college-educated individuals. 21% higher 
# fill rates than Medicaid-only. "Other public" 71% lower fill 
# compared to Medicaid-only. "Private- only" had 45% lower fill rates than
#  Medicaid-only. "Uninsured" had 31% lower fill rates than 
# Medicaid-only. Income has almost no effect per $1 increase.

#Confidence Intervals
confint.default(model8)
ci <- exp(confint.default(model8))
summary (ci)

#Results Table

coef_table <- coef(summary(model8))

results <- data.frame(
  term = names(coef(model8)),
  IRR = exp(coef(model8)),
  CI_low = exp(confint.default(model8)[,1]),
  CI_high = exp(confint.default(model8)[,2]),
  p_value = 2 * pt(
    abs(coef_table[, "t value"]),
    df = degf(design_step4),
    lower.tail = FALSE
  )
)

results

model9 <- svyglm(
  adhd_fills ~ year + age_group + sex + race + education + has_insurance 
                    + povcat + year:has_insurance,
  design = design_step5,
  family = quasipoisson()
)
summary(model9)
exp(coef(model9))
confint.default(model9)
ci2 <- exp(confint.default(model9))
summary (ci2)

coef_table2 <- coef(summary(model9))

results2 <- data.frame(
  term = rownames(coef_table2),
  IRR = exp(coef(model9)),
  CI_low = ci2[, 1],
  CI_high = ci2[, 2],
  p_value = 2 * pt(
    abs(coef_table2[, "t value"]),
    df = degf(design_step5),
    lower.tail = FALSE
  )
)

results2

# ---- FOREST PLOTS OF IRRS ----

term_labels <- c(
  "year2021:has_insuranceNo insurance" = "2021 x No insurance",
  "year2021" = "Year (ref: 2019)",
  "sexMale" = "Sex (ref: Female)",
  "raceOther" = "Race: Other (ref: White)",
  "raceBlack" = "Race: Black (ref: White)",
  "povcatLow income" = "Low income (ref: Very low income)",
  "povcatMiddle income" = "Middle income (ref: Very low income)",
  "povcatHigh income" = "High income (ref: Very low income)",
  "has_insuranceNo insurance" = "No insurance (ref: Has insurance)",
  "educationHigh school graduation or less" = "High school or less (ref: College or more)",
  "age_groupAdult (18+ years)" = "Age group (ref: Child <18 years)",
  "(Intercept)" = "Intercept"
)

forest1 <- ggplot(results, aes(x = fct_reorder(factor(term, 
                 levels = names(term_labels), 
                 labels = term_labels), IRR), 
                 y = IRR, 
                 ymin = CI_low, 
                 ymax = CI_high)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip(ylim = c(0, 8)) +
  theme_minimal() +
  labs(title = "Incidence rate ratios (IRRs) of ADHD fills, full cohort", 
       x = "Term", y = "IRR (95% CI)")

forest2 <- ggplot(results2, aes(x = fct_reorder(factor(term, 
                                levels = names(term_labels), 
                                labels = term_labels), IRR), 
                                y = IRR, 
                                ymin = CI_low, 
                                ymax = CI_high)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Incidence rate ratios (IRRs) of ADHD fills among those with fills", 
       x = "Term", y = "IRR (95% CI)")

# Save forest plots
ggsave("exports/forest_plot_full_cohort.png", forest1, width = 8, height = 6)
ggsave("exports/forest_plot_fills_only.png", forest2, width = 8, height = 6)

# using gt tables, include each result from models 1 through 8
# in a table format, showing sequential addition of covariates 
# and how IRR (+ 95 CI, p-val) changes for every term.

library(gtsummary)

tbl1 <- tbl_regression(model1, exponentiate = TRUE)
tbl2 <- tbl_regression(model2, exponentiate = TRUE)
tbl3 <- tbl_regression(model3, exponentiate = TRUE)
tbl4 <- tbl_regression(model4, exponentiate = TRUE)
tbl5 <- tbl_regression(model5, exponentiate = TRUE)
tbl6 <- tbl_regression(model6, exponentiate = TRUE)
tbl7 <- tbl_regression(model7, exponentiate = TRUE)
tbl8 <- tbl_regression(model8, exponentiate = TRUE)

combined_table <- tbl_merge(
  tbls = list(tbl1, tbl2, tbl3, tbl4, tbl5, tbl6, tbl7, tbl8),
  tab_spanner = c(
    "Model 1",
    "Model 2",
    "Model 3",
    "Model 4",
    "Model 5",
    "Model 6",
    "Model 7",
    "Model 8"
  )
)

combined_table
combined_table %>%
  bold_labels()

library(dplyr)
library(gt)

results_clean <- results %>%
  mutate(
    IRR = round(IRR, 2),
    CI = paste0(round(CI_low, 2), "–", round(CI_high, 2)),
    p_value = ifelse(p_value < 0.001, "<0.001", round(p_value, 3))
  ) %>%
  select(term, IRR, CI, p_value)

results_gt <- results_clean %>%
  gt() %>%
  tab_header(
    title = md("**Table 4. Survey-weighted quasipoisson regression of ADHD medication fills**")
  ) %>%
  cols_label(
    term = md("**Characteristic**"),
    IRR = md("**IRR**"),
    CI = md("**95% CI**"),
    p_value = md("**p-value**")
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,
    heading.title.font.size = 13,
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    table.border.top.width = px(2),
    table.border.bottom.width = px(2),
    column_labels.border.top.width = px(2),
    column_labels.border.bottom.width = px(2),
    row.striping.include_table_body = FALSE
  ) %>%
  tab_source_note(
    source_note = md("IRR = incidence rate ratio; CI = confidence interval. Model adjusted for year, age, sex, race, education, insurance status, and poverty category.")
  )

results_gt


