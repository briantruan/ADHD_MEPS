

options(survey.lonely.psu = "adjust")

# --------------------------------------------------
# 1. Define ADHD cohort
# --------------------------------------------------

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

# --------------------------------------------------
# 2. Build minimal person-level file for survey design
# --------------------------------------------------

fyc_2019_sub <- fyc_2019 %>%
  filter(DUPERSID %in% analytic_ids) %>%
  transmute(
    DUPERSID,
    VARPSU = suppressWarnings(as.numeric(as.character(VARPSU))),
    VARSTR = suppressWarnings(as.numeric(as.character(VARSTR))),
    PERWT  = suppressWarnings(as.numeric(as.character(PERWT))),
    year   = factor(2019)
  ) %>%
  filter(if_all(all_of(c("DUPERSID", "VARPSU", "VARSTR", "PERWT")), ~ !is.na(.)))

fyc_2021_sub <- fyc_2021 %>%
  filter(DUPERSID %in% analytic_ids) %>%
  transmute(
    DUPERSID,
    VARPSU = suppressWarnings(as.numeric(as.character(VARPSU))),
    VARSTR = suppressWarnings(as.numeric(as.character(VARSTR))),
    PERWT  = suppressWarnings(as.numeric(as.character(PERWT))),
    year   = factor(2021)
  ) %>%
  filter(if_all(all_of(c("DUPERSID", "VARPSU", "VARSTR", "PERWT")), ~ !is.na(.)))

fyc_combined <- bind_rows(fyc_2019_sub, fyc_2021_sub)

# --------------------------------------------------
# 3. Standardize medication names in RX
# --------------------------------------------------

standardize_adhd_med <- function(drug_name) {
  case_when(
    str_detect(toupper(drug_name), "LISDEXAMFETAMINE|VYVANSE") ~ "Lisdexamfetamine",
    str_detect(toupper(drug_name), "DEXMETHYLPHENIDATE|FOCALIN") ~ "Dexmethylphenidate",
    str_detect(toupper(drug_name), "METHYLPHENIDATE|RITALIN|CONCERTA|METADATE|DAYTRANA|QUILLIVANT|COTEMPLA|APTENSIO|JORNAY") ~ "Methylphenidate",
    str_detect(toupper(drug_name), "AMPHETAMINE|ADDERALL|MYDAYIS") ~ "Amphetamine",
    str_detect(toupper(drug_name), "ATOMOXETINE|STRATTERA") ~ "Atomoxetine",
    str_detect(toupper(drug_name), "VILOXAZINE|QELBREE") ~ "Viloxazine",
    str_detect(toupper(drug_name), "GUANFACINE|INTUNIV|TENEX") ~ "Guanfacine",
    str_detect(toupper(drug_name), "CLONIDINE|KAPVAY|CATAPRES") ~ "Clonidine",
    TRUE ~ NA_character_
  )
}

rx_2019_adhd <- rx_2019 %>%
  filter(DUPERSID %in% analytic_ids) %>%
  mutate(
    medication = standardize_adhd_med(RXDRGNAM),
    year = factor(2019)
  ) %>%
  filter(!is.na(medication))

rx_2021_adhd <- rx_2021 %>%
  filter(DUPERSID %in% analytic_ids) %>%
  mutate(
    medication = standardize_adhd_med(RXDRGNAM),
    year = factor(2021)
  ) %>%
  filter(!is.na(medication))

rx_person_med <- bind_rows(rx_2019_adhd, rx_2021_adhd) %>%
  distinct(DUPERSID, year, medication)

# --------------------------------------------------
# 4. Pivot to wide medication indicators
# --------------------------------------------------

med_wide <- rx_person_med %>%
  mutate(use = 1) %>%
  pivot_wider(
    id_cols = c(DUPERSID, year),
    names_from = medication,
    values_from = use,
    values_fill = 0
  )

# Keep all meds here so missing columns can still be added if absent
all_meds <- c(
  "Lisdexamfetamine",
  "Dexmethylphenidate",
  "Methylphenidate",
  "Amphetamine",
  "Atomoxetine",
  "Viloxazine",
  "Guanfacine",
  "Clonidine"
)

for (med in all_meds) {
  if (!med %in% names(med_wide)) {
    med_wide[[med]] <- 0
  }
}

# --------------------------------------------------
# 5. Merge medication indicators onto person-level file
# --------------------------------------------------

table2_data <- fyc_combined %>%
  left_join(med_wide, by = c("DUPERSID", "year")) %>%
  mutate(
    across(
      any_of(all_meds),
      ~ ifelse(is.na(.), 0, .)
    ),
    across(
      any_of(all_meds),
      ~ factor(.x, levels = c(0, 1), labels = c("No", "Yes"))
    )
  )

# --------------------------------------------------
# 6. Survey design
# --------------------------------------------------

meps_design2 <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT,
  data = table2_data,
  nest = TRUE
)

# --------------------------------------------------
# 7. Table 2: only medications retained in analysis
#    Exclude guanfacine, clonidine, and omit viloxazine row
# --------------------------------------------------

display_meds <- c(
  "Lisdexamfetamine",
  "Dexmethylphenidate",
  "Methylphenidate",
  "Amphetamine",
  "Atomoxetine",
  "Guanfacine",
  "Clonidine"
)

table2 <- tbl_svysummary(
  meps_design2,
  by = year,
  include = all_of(display_meds),
  label = list(
    Lisdexamfetamine ~ "Lisdexamfetamine",
    Dexmethylphenidate ~ "Dexmethylphenidate",
    Methylphenidate ~ "Methylphenidate",
    Amphetamine ~ "Amphetamine mixed salts",
    Atomoxetine ~ "Atomoxetine"
  ),
  statistic = all_categorical() ~ "{n} ({p}%)",
  missing = "no"
) %>%
  add_p() %>%
  bold_labels() %>%
  modify_footnote(
    all_stat_cols() ~ "n (%)"
  ) %>%
  modify_footnote(
    p.value ~ "Pearson's X^2: Rao & Scott adjustment"
  ) %>%
  modify_caption(
    "**Table 2. Weighted proportion of ADHD cohort using specific study medications, by year**"
  ) %>%
  modify_table_styling(
    columns = label,
    footnote = "Viloxazine use was 0% in both 2019 and 2021 and is not shown. Guanfacine and clonidine were excluded because they may have been prescribed for indications other than ADHD."
  )

table2
