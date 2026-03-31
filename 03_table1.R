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
  # filter(AGE53X >= 18) %>%
  mutate(
    year = factor(2019),
    sex = factor(SEX),
    income_2021 = income_2021,
    totslf_2021 = totslf_2021,
    race = factor(race),
    ethnicity = factor(ethnicity),
    education = factor(education),
    insurance = factor(insurance),
    has_insurance = factor(has_insurance)
  ) %>%
  filter(if_all(all_of(c("DUPERSID", "VARPSU", "VARSTR", "PERWT")), ~ !is.na(.)))

fyc_2021_sub <- fyc_2021 %>%
  filter(DUPERSID %in% analytic_ids) %>%
  # filter(AGE53X >= 18) %>%
  mutate(
    year = factor(2021),
    sex = factor(SEX),
    income_2021 = TTLP,
    totslf_2021 = TOTSLF,
    race = factor(race),
    ethnicity = factor(ethnicity),
    education = factor(education),
    insurance = factor(insurance),
    has_insurance = factor(has_insurance)
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

meps_design <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT,
  data = fyc_combined,
  nest = TRUE
)

table1 <- tbl_svysummary(
  meps_design,
  by = year,
  include = c(
    AGE53X,
    sex,
    race,
    ethnicity,
    education,
    has_insurance,
    insurance,
    income_2021,
    totslf_2021
  ),
  label = list(
    AGE53X ~ "Age, years",
    sex ~ "Sex",
    race ~ "Race",
    ethnicity ~ "Ethnicity",
    education ~ "Education",
    has_insurance ~ "Has insurance", 
    insurance ~ "Type of Insurance",
    income_2021 ~ "Household income (2021 USD)",
    totslf_2021 ~ "Total spending (2021 USD)"
  ),
  statistic = list(
    AGE53X ~ "{mean} ({sd})",
    income_2021 ~ "{median} ({p25}, {p75})",
    totslf_2021 ~ "{median} ({p25}, {p75})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  missing = "ifany"
)

table1 <- add_p(table1)
table1 <- bold_labels(table1)

table1

table1_gt <- as_gt(table1)
gt::gtsave(table1_gt, filename = file.path("exports", "table1.docx"))