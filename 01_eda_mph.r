# Exploratory Data Analysis
# Goal: To understand the structure of the data, 
# identify patterns, and detect any anomalies or outliers.
# MPH-specific: only looking at pre-post two years, 
# no longitudinal analyses

# load rds data
all_data <- readRDS("data/meps_all_years.rds")

# merge fyc_2019 and fyc_2021 in all_data as fyc_simple
fyc_2019 <- all_data$fyc_2019
fyc_2021 <- all_data$fyc_2021
cond_2019 <- all_data$cond_2019
cond_2021 <- all_data$cond_2021
rx_2019 <- all_data$rx_2019
rx_2021 <- all_data$rx_2021

# clear var all_data
rm(all_data)
gc()

# convert haven labels to factors in df fyc 
# remove preceding numbers from factor levels
clean_labels <- function(x) {
  if (haven::is.labelled(x)) x <- haven::as_factor(x)
  if (is.factor(x)) levels(x) <- gsub("^[0-9]+\\s*", "", levels(x))
  x
}

fyc_2019 <- fyc_2019 %>% mutate(across(where(~ haven::is.labelled(.x) || is.factor(.x)), clean_labels))
fyc_2021 <- fyc_2021 %>% mutate(across(where(~ haven::is.labelled(.x) || is.factor(.x)), clean_labels))

# dollars: convert to 2021 dollars using CPI
cpi_2019  <- 256.974
cpi_2021  <- 278.802
cpi_ratio <- cpi_2021 / cpi_2019

fyc_2019$income_2021 <- fyc_2019$TTLP * cpi_ratio
fyc_2019$totslf_2021 <- fyc_2019$TOTSLF * cpi_ratio

# pcs/mcs: if not numeric, NA
fyc_2019 <- fyc_2019 %>% mutate(across(c(VPCS42, VMCS42), ~ as.numeric(.x)))
fyc_2021 <- fyc_2021 %>% mutate(across(c(VPCS42, VMCS42), ~ as.numeric(.x)))

# rename INSURC19 in fyc_2019 to INSURC
# rename INSURC21 in fyc_2021 to INSURC
fyc_2019 <- fyc_2019 %>% dplyr::rename(INSURC = INSURC19)
fyc_2021 <- fyc_2021 %>% dplyr::rename(INSURC = INSURC21)

recode_fyc <- function(df) {
  df %>% 
    mutate(
      sex = case_when(
        SEX == "MALE" ~ "Male",
        SEX == "FEMALE" ~ "Female",
        TRUE ~ NA_character_
      ),
      ethnicity = case_when(
        RACETHX == "HISPANIC" ~ "Hispanic",
        TRUE ~ "Non-Hispanic"
      ),
      race = case_when(
        RACETHX == "NON-HISPANIC ASIAN ONLY" ~ "Asian",
        RACETHX == "NON-HISPANIC BLACK ONLY" ~ "Black",
        RACETHX == "NON-HISPANIC OTHER RACE OR MULTIPLE RACE" ~ "Other",
        RACETHX == "NON-HISPANIC WHITE ONLY" ~ "White",
        TRUE ~ NA_character_
      ),
      race = factor(race, levels = c("White", "Black", "Asian", "Other")),
      marital = case_when(
        MARRY53X %in% c("-1 INAPPLICABLE", "-7 REFUSED", "-8 DK") ~ NA_character_,
        MARRY53X %in% c("MARRIED", "MARRIED IN ROUND") ~ "Married",
        TRUE ~ "Not married"
      ),
      REGION53 = na_if(REGION53, "-1 INAPPLICABLE"),
      education = case_when(
        EDUCYR %in% c("-1 INAPPLICABLE", "-7 REFUSED", "-8 DK", "-15 CANNOT BE COMPUTED") ~ NA_character_,
        EDUCYR %in% c("NO SCHOOL/KINDERGARTEN ONLY", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "GRADE 12") ~ "High school graduation or less",
        EDUCYR %in% c("1 YEAR COLLEGE", "2 YEARS COLLEGE", "3 YEARS COLLEGE", "4 YEARS COLLEGE", "5+ YEARS COLLEGE") ~ "College education or greater",
        TRUE ~ NA_character_
      ),
      insurance = case_when(
        INSURC %in% c("<65 UNINSURED", "65+ UNINSURED") ~ "Uninsured",
        INSURC == "<65 ANY PRIVATE" & MCAID53X != "YES" ~ "Private only",
        INSURC == "<65 ANY PRIVATE" & MCAID53X == "YES" ~ "Medicaid, with private",
        INSURC == "<65 PUBLIC ONLY" & MCAID53X == "YES" ~ "Medicaid only",
        INSURC == "<65 PUBLIC ONLY" & MCAID53X != "YES" ~ "Other public",
        INSURC == "65+ EDITED MEDICARE AND PRIVATE" & MCAID53X != "YES" ~ "Medicare, with private",
        INSURC == "65+ EDITED MEDICARE AND PRIVATE" & MCAID53X == "YES" ~ "Medicare, dual-eligible",
        INSURC == "65+ EDITED MEDICARE ONLY" & MCAID53X != "YES" ~ "Medicare only",
        INSURC == "65+ EDITED MEDICARE ONLY" & MCAID53X == "YES" ~ "Medicare, dual-eligible",
        INSURC == "65+ EDITED MEDICARE AND OTH PUB ONLY" & MCAID53X == "YES" ~ "Medicare, dual-eligible",
        INSURC == "65+ EDITED MEDICARE AND OTH PUB ONLY" & MCAID53X != "YES" ~ "Medicare, with other public",
        INSURC == "65+ NO MEDICARE AND ANY PUBLIC/PRIVATE" & MCAID53X == "YES" ~ "Medicaid, with private",
        INSURC == "65+ NO MEDICARE AND ANY PUBLIC/PRIVATE" & MCAID53X != "YES" ~ "Private only",
        INSCOV == "UNINSURED" ~ "Uninsured",
        INSCOV == "ANY PRIVATE" ~ "Private only",
        INSCOV == "PUBLIC ONLY" & MCAID53X == "YES" ~ "Medicaid only",
        INSCOV == "PUBLIC ONLY" & MCAID53X != "YES" ~ "Other public",
        TRUE ~ NA_character_
      ),
      insurance = factor(insurance, levels = c(
        "Private only",
        "Medicaid only",
        "Medicaid, with private",
        "Medicare only",
        "Medicare, with private",
        "Medicare, with other public",
        "Medicare, dual-eligible",
        "Other public",
        "Uninsured"
      )),
      medicaid = case_when(
        insurance == "Medicaid only" ~ "Medicaid only",
        insurance == "Medicaid, with private" ~ "Medicaid, with private, non-Medicare",
        TRUE ~ "No Medicaid"
      ),
      medicare = case_when(
        insurance %in% c("Medicare, with private", "Medicare, dual-eligible", "Medicare, with other public", "Medicare only") ~ "Medicare, any",
        TRUE ~ "No Medicare"
      ),
      private_ins = case_when(
        insurance == "Private only" ~ "Private only",
        TRUE ~ "Other/uninsured"
      ),
      has_insurance = case_when(
        INSCOV == "UNINSURED" ~ "No insurance",
        INSCOV %in% c("ANY PRIVATE", "PUBLIC ONLY") ~ "Has insurance",
        TRUE ~ NA_character_
      )
    )
}

fyc_2019 <- recode_fyc(fyc_2019)
fyc_2021 <- recode_fyc(fyc_2021)
