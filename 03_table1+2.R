# Table 1 and 2: demographics and medication use among ADHD cohort
# Goal: Create Table 1 with demographics and 
# Table 2 with medication use among ADHD cohort, 
# comparing 2019 vs 2021

# recall the inclusion/exclusion criteria:
# from 07_clink.R step 10: kept participants with data in both years
# from 07_clink.R: flagged people with adhd dx and adhd rx fills

# fyc_clean is our clean dataset with everyone in it

# ---- FYC_CLEAN ADDL CHANGES ----

# adult income only, remove Medicare, create age/group variables
fyc_clean <- fyc_clean %>%
  mutate(
    adult_income_2021 = if_else(AGE53X >= 18, income_2021, NA_real_),

    insurance_table1 = if_else(
      str_detect(insurance, "Medicare"),
      NA_character_,
      insurance
    ),
    insurance_table1 = factor(insurance_table1),
    
    age_group = case_when(
      AGE53X < 18 ~ "Child (<18 years)",
      AGE53X >= 18 ~ "Adult (18+ years)",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("Child (<18 years)", "Adult (18+ years)")),
    
    age_sex_group = case_when(
      AGE53X < 18 & sex == "Male" ~ "Child, male",
      AGE53X < 18 & sex == "Female" ~ "Child, female",
      AGE53X >= 18 & sex == "Male" ~ "Adult, male",
      AGE53X >= 18 & sex == "Female" ~ "Adult, female",
      TRUE ~ NA_character_
    ),
    age_sex_group = factor(
      age_sex_group,
      levels = c("Child, male", "Child, female", "Adult, male", "Adult, female")
    )
  ) %>% 
  mutate(
    race = relevel(factor(race), ref = "White"),
    year = factor(year, levels = c(2019, 2021))
  )

# ---- SURVEY SETUP ----
options(survey.lonely.psu = "adjust")

# this has everyone
meps_design <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT,
  data = fyc_clean,
  nest = TRUE
)

# include everyone ≤65 years old
meps_design_subset_age       <- subset(meps_design, AGE53X <= 65)

# subset to adhd only
meps_design_subset_adhd      <- subset(meps_design_subset_age, flag_adhd_dx == 1)

# subset to adhd only and with fills
meps_design_subset_adhdfills <- subset(meps_design_subset_age, flag_adhd_dx == 1 & adhd_pmed_flag == 1)

# ---- TABLE 1: demographics ----
table1 <- tbl_svysummary(
  meps_design_subset_adhd,
  by = year,
  include = c(
    AGE53X,
    age_group,
    age_sex_group,
    race,
    ethnicity,
    education,
    has_insurance,
    insurance_table1,
    adult_income_2021,
    totslf_2021
  ),
  label = list(
    AGE53X ~ "Age (years)",
    age_group ~ "Age group",
    age_sex_group ~ "Age-sex subgroup",
    race ~ "Race",
    ethnicity ~ "Ethnicity",
    education ~ "Education",
    has_insurance ~ "Has insurance",
    insurance_table1 ~ "Type of insurance",
    adult_income_2021 ~ "Household (18+ years) income (2021 USD)",
    totslf_2021 ~ "Total out-of-pocket spending (2021 USD)"
  ),
  statistic = list(
    all_categorical() ~ "{n} ({p}%)",
    AGE53X ~ "{mean} ({sd})",
    adult_income_2021 ~ "{median} ({p25}-{p75})",
    totslf_2021 ~ "{median} ({p25}-{p75})"
  ),
  missing = "no"
) %>%
  add_p() %>%
  bold_labels()

table1

table1_gt <- as_gt(table1)
gtsave(table1_gt, filename = file.path("exports", "table1.docx"))

# ---- TABLE 2: among ADHD full cohort and with fills ----

# ---- USELESS ----
# table is kind of useless

#  table2_all <- tbl_svysummary(
#   meps_design_subset_adhd,
#   by = year,
#   include = c(
#     adhd_pmed_flag,
#     adhd_fills,
#     adhd_total_spend,
#     adhd_oop,
#     adhd_private,
#     adhd_medicaid,
#     oop_share
#   ),
#   label = list(
#     adhd_pmed_flag ~ "Any ADHD medication",
#     adhd_fills ~ "Number of ADHD medication fills",
#     adhd_total_spend ~ "Total ADHD medication spending (2021 USD)",
#     adhd_oop ~ "Out-of-pocket ADHD spending (2021 USD)",
#     adhd_private ~ "Private insurance ADHD spending (2021 USD)",
#     adhd_medicaid ~ "Medicaid ADHD spending (2021 USD)",
#     oop_share ~ "Out-of-pocket share of ADHD medication spending (%)"
#   ),
#   statistic = list(
#     all_categorical() ~ "{n} ({p}%)",
#     all_continuous() ~ "{mean} ({sd})"
#   ),
#   missing = "no"
# ) %>% 
#   add_p() %>%
#   bold_labels()

# table2_all

# ---- USE THIS ----

# use design variable meps_design_subset_adhdfills
table2_fills <- tbl_svysummary(
  meps_design_subset_adhdfills,
  by = year,
  include = c(
    n_drug_names,
    adhd_fills,
    adhd_days_supp,
    pill_qty_mean,
    adhd_total_spend,
    adhd_oop,
    adhd_private,
    adhd_medicaid,
    oop_share
  ),
  label = list(
    n_drug_names ~ "Number of unique ADHD drugs used",
    adhd_fills ~ "Number of ADHD medication fills",
    adhd_days_supp ~ "Number of ADHD medication days supplied",
    pill_qty_mean ~ "Average number of pills per fill",
    adhd_total_spend ~ "Total ADHD medication spending (2021 USD)",
    adhd_oop ~ "Out-of-pocket ADHD spending (2021 USD)",
    adhd_private ~ "Private insurance ADHD spending (2021 USD)",
    adhd_medicaid ~ "Medicaid ADHD spending (2021 USD)",
    oop_share ~ "Out-of-pocket share of ADHD medication spending (%)"
  ),
  statistic = list(
    all_categorical() ~ "{n} ({p}%)",
    adhd_fills ~ "{mean} ({sd})",
    adhd_days_supp ~ "{mean} ({sd})",
    pill_qty_mean ~ "{mean} ({sd})",
    adhd_total_spend ~ "{median} ({p25}-{p75})",
    adhd_oop ~ "{median} ({p25}-{p75})",
    adhd_private ~ "{median} ({p25}-{p75})",
    adhd_medicaid ~ "{median} ({p25}-{p75})",
    oop_share ~ "{mean} ({sd})"
  ),
  missing = "no"
) %>% 
  add_p() %>%
  bold_labels() %>% 
  modify_table_styling(
    columns = label,
    footnote = "Participants who were on different formulations of the same drug (e.g., normal and extended-release) are characterized as using 2 unique ADHD drugs.")

table2_fills

# table2_combined <- tbl_merge(
#   tbls = list(table2_all, table2_fills),
#   tab_spanner = c("**All ADHD cohort**", "**ADHD cohort with fills**")
# )

# table2_combined

table2_gt <- as_gt(table2_fills)
gtsave(table2_gt, filename = file.path("exports", "table2.docx"))