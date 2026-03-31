options(survey.lonely.psu = "adjust")

standardize_adhd_med <- function(drug_name) {
  case_when(
    str_detect(toupper(drug_name), "LISDEXAMFETAMINE|VYVANSE") ~ "Lisdexamfetamine",
    str_detect(toupper(drug_name), "DEXMETHYLPHENIDATE|FOCALIN") ~ "Dexmethylphenidate",
    str_detect(toupper(drug_name), "METHYLPHENIDATE|RITALIN|CONCERTA|METADATE|DAYTRANA|QUILLIVANT|COTEMPLA|APTENSIO|JORNAY") ~ "Methylphenidate",
    str_detect(toupper(drug_name), "AMPHETAMINE|ADDERALL|MYDAYIS") ~ "Amphetamine",
    str_detect(toupper(drug_name), "ATOMOXETINE|STRATTERA|AXEPTA|ATTENTROL|ATTERA|ATONEXT|STARKID") ~ "Atomoxetine",
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

med_wide <- rx_person_med %>%
  mutate(use = 1) %>%
  pivot_wider(
    id_cols = c(DUPERSID, year),
    names_from = medication,
    values_from = use,
    values_fill = 0
  )

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

table3_data <- fyc_combined_t2 %>%
  left_join(med_wide, by = c("DUPERSID", "year")) %>%
  filter(adhd_any_rx == "Yes") %>%
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

meps_design3 <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT,
  data = table3_data,
  nest = TRUE
)

display_meds <- c(
  "Lisdexamfetamine",
  "Dexmethylphenidate",
  "Methylphenidate",
  "Amphetamine",
  "Atomoxetine",
  "Guanfacine",
  "Clonidine"
)

table3 <- tbl_svysummary(
  meps_design3,
  by = year,
  include = all_of(display_meds),
  label = list(
    Lisdexamfetamine ~ "Lisdexamfetamine",
    Dexmethylphenidate ~ "Dexmethylphenidate",
    Methylphenidate ~ "Methylphenidate",
    Amphetamine ~ "Amphetamine mixed salts",
    Atomoxetine ~ "Atomoxetine",
    Guanfacine ~ "Guanfacine",
    Clonidine ~ "Clonidine"
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
    "**Table 3. Weighted proportion of ADHD cohort using specific study medications, by year**"
  ) %>%
  modify_table_styling(
    columns = label,

    # we are going to double check if the alpha 2 agonists are prescribed for ADHD

    footnote = "Viloxazine use was 0% in both 2019 and 2021 and is not shown."
  )

table3

table3_gt <- as_gt(table3)
gt::gtsave(table3_gt, filename = file.path("exports", "table3.docx"))