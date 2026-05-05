# ---- TABLE 1 ----
# Create a table of descriptive statistics
# make gt tables
table1 <- tbl_svysummary(
    design_step4,
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
      povcat
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
      povcat ~ "Poverty category"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} ({sd})"
    ),
    missing = "no"
  ) %>%
  add_p() %>%
  bold_labels()

table1_gt <- as_gt(table1)
gtsave(table1_gt, "exports/table1.html")
gtsave(table1_gt, "exports/table1.docx")

# ---- TABLE 2 ----
# Create a table of ADHD related metrics
table2 <- tbl_svysummary(
  design_step5,
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
    all_continuous() ~ "{mean} ({sd})"
    # adhd_fills ~ "{mean} ({sd})",
    # adhd_days_supp ~ "{mean} ({sd})",
    # pill_qty_mean ~ "{mean} ({sd})",
    # adhd_total_spend ~ "{median} ({p25}-{p75})",
    # adhd_oop ~ "{median} ({p25}-{p75})",
    # adhd_private ~ "{median} ({p25}-{p75})",
    # adhd_medicaid ~ "{median} ({p25}-{p75})",
    # oop_share ~ "{mean} ({sd})"
  ),
  missing = "no"
) %>% 
  add_p() %>%
  bold_labels() %>% 
  modify_table_styling(
    columns = label,
    footnote = "Participants who were on different formulations of the same 
    drug (e.g., normal and extended-release) are characterized as using 2 
    unique ADHD drugs.")

table2_gt <- as_gt(table2)
gtsave(table2_gt, "exports/table2.html")
gtsave(table2_gt, "exports/table2.docx")

# ---- TABLE 3 ----
# Goal: Create Table 3 with types of ADHD meds used 
# among ADHD cohort, comparing 2019 vs 2021
# includes people with fills only

# use design variable design_step5
# variable name drug_names is the names of drugs used
# delimited by semicolon
# count the number of each drug or combination of drugs used

# rename from all caps to title case
design_step5 <- design_step5 %>%
  update(
    drug_names = str_to_title(drug_names)
  ) %>% 
  # recode drug names to more user-friendly labels
  update(
    drug_names = case_when(
      str_detect(drug_names, "Amphetamine-Dextroamphetamine") 
        ~ "Amphetamines",
      str_detect(drug_names, "Amphetamine-Dextroamphetamine; Clonidine") 
        ~ "Mixed therapy",
      str_detect(drug_names, "Amphetamine-Dextroamphetamine; Methylphenidate") 
        ~ "Mixed therapy",
      str_detect(drug_names, "Clonidine; Dexmethylphenidate") 
        ~ "Mixed therapy",
      str_detect(drug_names, "Clonidine; Methylphenidate") 
        ~ "Mixed therapy",
      str_detect(drug_names, "Dexmethylphenidate; Guanfacine") 
        ~ "Mixed therapy",
      str_detect(drug_names, "Atomoxetine; Methylphenidate") 
        ~ "Mixed therapy",
      str_detect(drug_names, "Guanfacine; Methylphenidate") 
        ~ "Mixed therapy",
      str_detect(drug_names, "Mixed Therapy") 
      ~ "Mixed therapy",
      str_detect(drug_names, "Non-Stimulants") 
        ~ "Non-stimulants",
      str_detect(drug_names, "Atomoxetine") ~ "Non-stimulants",
      str_detect(drug_names, "Bupropion") ~ "Non-stimulants",
      str_detect(drug_names, "Clonidine") ~ "Non-stimulants",
      str_detect(drug_names, "Guanfacine") ~ "Non-stimulants",
      # str_detect(drug_names, "Bupropion") ~ NA_character_,
      TRUE ~ drug_names
    )
  )

table3 <- tbl_svysummary(
  design_step5,
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
    footnote = "Participants who were on different formulations of the same 
    drug (e.g., normal and extended-release) were included in the same category
    and not under \"Mixed therapy.\""
  )

table3_gt <- as_gt(table3)
gtsave(table3_gt, "exports/table3.html")
gtsave(table3_gt, "exports/table3.docx")