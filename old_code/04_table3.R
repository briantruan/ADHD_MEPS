# Table 3: types of ADHD meds used among ADHD cohort
# Goal: Create Table 3 with types of ADHD meds used 
# among ADHD cohort, comparing 2019 vs 2021
# includes people with fills only

# use design variable meps_design_subset_adhdfills
# variable name drug_names is the names of drugs used
# delimited by semicolon
# count the number of each drug or combination of drugs used

# rename from all caps to title case
meps_design_subset_adhdfills <- meps_design_subset_adhdfills %>%
  update(
    drug_names = str_to_title(drug_names)
  ) %>% 
  # recode drug names to more user-friendly labels
  update(
    drug_names = case_when(
      str_detect(drug_names, "Amphetamine-Dextroamphetamine") ~ "Amphetamines",
      str_detect(drug_names, "Amphetamine-Dextroamphetamine; Clonidine") ~ "Mixed therapy",
      str_detect(drug_names, "Amphetamine-Dextroamphetamine; Methylphenidate") ~ "Mixed therapy",
      str_detect(drug_names, "Clonidine; Dexmethylphenidate") ~ "Mixed therapy",
      str_detect(drug_names, "Dexmethylphenidate; Guanfacine") ~ "Mixed therapy",
      str_detect(drug_names, "Bupropion") ~ NA_character_,
      TRUE ~ drug_names
    )
  )

table3 <- tbl_svysummary(
  meps_design_subset_adhdfills,
  by = year,
  include = drug_names,
  label = list(drug_names ~ "ADHD medication types"),
  statistic = all_continuous() ~ "{n} ({p}%)",
  missing = "no"
) %>% 
  add_p() %>%
  bold_labels() %>% 
  modify_table_styling(
    columns = label,
    footnote = "Participants who were on different formulations of the same drug (e.g., normal and extended-release) were included in the same category and not under \"Mixed therapy.\""
  )

table3

# ---- OLD CODE ----

# standardize drug names
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

# use svydesign meps_design_subset_adhdfills


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
  modify_table_styling(
    columns = label,
    footnote = "Viloxazine use was 0% in both 2019 and 2021 and is not shown."
  )

table3

table3_gt <- as_gt(table3)
gt::gtsave(table3_gt, filename = file.path("exports", "table3.docx"))