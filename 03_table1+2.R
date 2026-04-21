#TODO: use the CLINK files to link speciifc, e.g., rx and condition files to ensure that we are limiting to the same analytic sample across all analyses

options(survey.lonely.psu = "adjust")

ids_both <- intersect(fyc_2019$DUPERSID, fyc_2021$DUPERSID)

adhd_ids_2019 <- cond_2019 %>%
  filter(!is.na(ICD10CDX)) %>%
  filter(str_detect(ICD10CDX, "^F90")) %>%
  distinct(DUPERSID) %>%
  pull(DUPERSID)

adhd_ids_2021 <- cond_2021 %>%
  filter(!is.na(ICD10CDX)) %>%
  filter(str_detect(ICD10CDX, "^F90")) %>%
  distinct(DUPERSID) %>%
  pull(DUPERSID)

adhd_ids <- union(adhd_ids_2019, adhd_ids_2021)

analytic_ids <- intersect(ids_both, adhd_ids)

fyc_2019_sub <- fyc_2019 %>%
  filter(DUPERSID %in% analytic_ids) %>%
  filter(AGE53X <= 63) %>%
  mutate(
    year = factor(2019),
    income_2021 = income_2021,
    totslf_2021 = totslf_2021
  ) %>%
  filter(if_all(all_of(c("DUPERSID", "VARPSU", "VARSTR", "PERWT")), ~ !is.na(.)))

fyc_2021_sub <- fyc_2021 %>%
  filter(DUPERSID %in% analytic_ids) %>%
  filter(AGE53X <= 65) %>%
  mutate(
    year = factor(2021),
    income_2021 = TTLP,
    totslf_2021 = TOTSLF
  ) %>%
  filter(if_all(all_of(c("DUPERSID", "VARPSU", "VARSTR", "PERWT")), ~ !is.na(.)))

num_vars <- c(
  "OBTOTV", "OPTOTV", "IPDIS", "IPNGTD", "RXTOT", "ERTOT",
  "TTLP", "TOTSLF", "income_2021", "totslf_2021",
  "AGE53X", "VPCS42", "VMCS42", "PERWT", "VARSTR", "VARPSU"
)

fyc_2019_sub <- fyc_2019_sub %>%
  mutate(across(any_of(num_vars), ~ as.numeric(as.character(.))))

fyc_2021_sub <- fyc_2021_sub %>%
  mutate(across(any_of(num_vars), ~ as.numeric(as.character(.))))

fyc_combined <- bind_rows(fyc_2019_sub, fyc_2021_sub)

# ---- NEW AGE + SEX VARIABLES FOR TABLE 1 ----
fyc_combined <- fyc_combined %>%
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
  )

meps_design <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT,
  data = fyc_combined,
  nest = TRUE
)

# ---- UPDATED TABLE 1 ----
table1 <- tbl_svysummary(
  meps_design,
  by = year,
  include = c(
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
  sort = list(
    insurance_table1 ~ "alphanumeric"
  ),
  label = list(
    age_group ~ "Age group",
    age_sex_group ~ "Age-sex subgroup",
    race ~ "Race",
    ethnicity ~ "Ethnicity",
    education ~ "Education",
    has_insurance ~ "Has insurance",
    insurance_table1 ~ "Type of insurance",
    adult_income_2021 ~ "Household (18+ years) income (2021 USD)",
    totslf_2021 ~ "Total spending (2021 USD)"
  ),
  statistic = list(
    all_categorical() ~ "{n} ({p}%)",
    adult_income_2021 ~ "{median} ({p25}, {p75})",
    totslf_2021 ~ "{median} ({p25}, {p75})"
  ),
  missing = "no"
) %>%
  add_p() %>%
  bold_labels()

table1

table1_gt <- as_gt(table1)
gt::gtsave(table1_gt, filename = file.path("exports", "table1.docx"))


# CPI adjustment to 2021 dollars
cpi_2019  <- 256.974
cpi_2021  <- 278.802
cpi_ratio <- cpi_2021 / cpi_2019

adhd_rx_summary <- rx_ndc %>%
  mutate(
    across(c(RXSF, RXMR, RXMD, RXPV, RXVA, RXTR, RXOF, RXSL, RXWC, RXOT, RXXP),
           ~ as.numeric(as.character(.))),
    RXDAYSUP = as.numeric(as.character(RXDAYSUP)),
    RXDAYSUP = if_else(RXDAYSUP < 0, NA_real_, RXDAYSUP)
  ) %>%
  mutate(
    across(c(RXSF, RXMR, RXMD, RXPV, RXVA, RXTR, RXOF, RXSL, RXWC, RXOT, RXXP),
           ~ if_else(year == 2019, .x * cpi_ratio, .x))
  ) %>%
  group_by(DUPERSID, year) %>%
  summarise(
    adhd_any_rx = "Yes",
    adhd_rx_n = n(),
    adhd_daysup_total = sum(RXDAYSUP, na.rm = TRUE),
    adhd_total_spend = sum(RXXP, na.rm = TRUE, digits = 2),
    adhd_oop = sum(RXSF, na.rm = TRUE, digits = 2),
    adhd_private = sum(RXPV, na.rm = TRUE, digits = 2),
    adhd_medicaid = sum(RXMD, na.rm = TRUE, digits = 2),
    adhd_medicare = sum(RXMR, na.rm = TRUE, digits = 2),
    .groups = "drop"
  ) %>%
  mutate(
    oop_share = if_else(adhd_total_spend > 0, adhd_oop / adhd_total_spend * 100, NA_real_)
  )

fyc_2019_t2 <- fyc_2019_sub %>%
  left_join(
    adhd_rx_summary %>% filter(year == 2019) %>% select(-year),
    by = "DUPERSID"
  ) %>%
  mutate(
    adhd_any_rx = as.character(adhd_any_rx),
    adhd_any_rx = replace_na(adhd_any_rx, "No"),
    adhd_any_rx = factor(adhd_any_rx, levels = c("No", "Yes")),
    across(
      c(adhd_rx_n, adhd_daysup_total, adhd_total_spend, adhd_oop,
        adhd_private, adhd_medicaid, adhd_medicare, oop_share),
      ~ replace_na(., 0)
    )
  )

fyc_2021_t2 <- fyc_2021_sub %>%
  left_join(
    adhd_rx_summary %>% filter(year == 2021) %>% select(-year),
    by = "DUPERSID"
  ) %>%
  mutate(
    adhd_any_rx = as.character(adhd_any_rx),
    adhd_any_rx = replace_na(adhd_any_rx, "No"),
    adhd_any_rx = factor(adhd_any_rx, levels = c("No", "Yes")),
    across(
      c(adhd_rx_n, adhd_daysup_total, adhd_total_spend, adhd_oop,
        adhd_private, adhd_medicaid, adhd_medicare, oop_share),
      ~ replace_na(., 0)
    )
  )

fyc_combined_t2 <- bind_rows(fyc_2019_t2, fyc_2021_t2)

meps_design_final <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT,
  data = fyc_combined_t2,
  nest = TRUE
)

# Full ADHD cohort
table2_all <- tbl_svysummary(
  meps_design_final,
  by = year,
  include = c(
    adhd_any_rx,
    adhd_rx_n,
    adhd_daysup_total,
    adhd_total_spend,
    adhd_oop,
    adhd_private,
    adhd_medicaid,
    adhd_medicare,
    oop_share
  ),
  label = list(
    adhd_any_rx ~ "Any ADHD medication",
    adhd_rx_n ~ "Number of ADHD medication fills recorded for that person in that year",
    adhd_daysup_total ~ "Total ADHD medication days supplied",
    adhd_total_spend ~ "Total ADHD medication spending (2021 USD)",
    adhd_oop ~ "Out-of-pocket ADHD spending (2021 USD)",
    adhd_private ~ "Private insurance ADHD spending (2021 USD)",
    adhd_medicaid ~ "Medicaid ADHD spending (2021 USD)",
    adhd_medicare ~ "Medicare ADHD spending (2021 USD)",
    oop_share ~ "Out-of-pocket share of ADHD medication spending (%)"
  ),
  statistic = list(
    all_categorical() ~ "{n} ({p}%)",
    all_continuous() ~ "{mean} ({sd})"
    # adhd_total_spend ~ "{median} ({p25}, {p75})",
    # adhd_oop ~ "{median} ({p25}, {p75})",
    # adhd_private ~ "{median} ({p25}, {p75})",
    # adhd_medicaid ~ "{median} ({p25}, {p75})",
    # adhd_medicare ~ "{median} ({p25}, {p75})",
    # oop_share ~ "{mean} ({sd})"
  ),
  missing = "no"
) %>%
  add_p() %>%
  bold_labels()

# Subset analysis: only those with any ADHD medication
meps_design_final_users <- subset(meps_design_final, adhd_any_rx == "Yes")

table2_users <- tbl_svysummary(
  meps_design_final_users,
  by = year,
  include = c(
    adhd_rx_n,
    adhd_daysup_total,
    adhd_total_spend,
    adhd_oop,
    adhd_private,
    adhd_medicaid,
    adhd_medicare,
    oop_share
  ),
  label = list(
    adhd_rx_n ~ "Number of ADHD medication fills recorded for that person in that year",
    adhd_daysup_total ~ "Total ADHD medication days supplied",
    adhd_total_spend ~ "Total ADHD medication spending (2021 USD)",
    adhd_oop ~ "Out-of-pocket ADHD spending (2021 USD)",
    adhd_private ~ "Private insurance ADHD spending (2021 USD)",
    adhd_medicaid ~ "Medicaid ADHD spending (2021 USD)",
    adhd_medicare ~ "Medicare ADHD spending (2021 USD)",
    oop_share ~ "Out-of-pocket share of ADHD medication spending (%)"
  ),
  statistic = list(
    all_categorical() ~ "{n} ({p}%)",
    all_continuous() ~ "{mean} ({sd})"
  ),
  missing = "ifany"
) %>%
  add_p() %>%
  bold_labels()

# Optional merged table
table2_combined <- tbl_merge(
  tbls = list(table2_all, table2_users),
  tab_spanner = c(
    "**All ADHD respondents**",
    "**Among those with any ADHD medication**"
  )
)

# Save merged table
table2_combined_gt <- as_gt(table2_combined)
gt::gtsave(table2_combined_gt, filename = file.path("exports", "table2_combined.docx"))
