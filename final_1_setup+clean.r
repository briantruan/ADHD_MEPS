# ---- SETUP ----
# Package installation and library loading.

# # Only need to run these once (uncomment if needed)
# install.packages("foreign")
# install.packages("devtools")
# install.packages("tidyverse")
# install.packages("readr")
# install.packages("readxl")
# install.packages("haven")
# install.packages("dplyr")
# install.packages("gtsummary")
# isntall.packages("gt")
# install.packages("stringr")
# install.packages("survey")
# install.packages("DiagrammeR")
# install.packages("DiagrammeRsvg")
# install.packages("rsvg")

# # Run these every time you restart R:
library(foreign)
library(devtools)
library(tidyverse)
library(readr)
library(readxl)
library(haven)
library(dplyr)
library(gtsummary)
library(gt)
library(stringr)
library(survey)
library(glue)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# only run once if needed
# install_github("sshrestha274/meps_r_pkg/MEPS")

library(MEPS)

# ---- LOAD DATA HELPER FUNCTION ----
# This function will help us download the data and save to an RDS file if we 
# haven't already done so already.

load_meps_year <- function(year) {
  short_year <- substr(year, 3, 4)
  
  perwt  <- paste0("PERWT",  short_year, "F")
  ttlp   <- paste0("TTLP",   short_year, "X")
  povcat <- paste0("POVCAT", short_year)
  inscov <- paste0("INSCOV", short_year)
  insurc <- paste0("INSURC", short_year)
  totslf <- paste0("TOTSLF", short_year)
  obtotv <- paste0("OBTOTV", short_year)
  optotv <- paste0("OPTOTV", short_year)
  ipdis  <- paste0("IPDIS",  short_year)
  ipngtd <- paste0("IPNGTD", short_year)
  rxtot  <- paste0("RXTOT",  short_year)
  ertot  <- paste0("ERTOT",  short_year)
  
  vars_to_keep <- c(
    "DUPERSID", "PANEL", "VARSTR", "VARPSU", perwt, ttlp,
    "SEX", "AGE53X", "RACETHX", "MARRY53X", "REGION53",
    "EDUYRDG", "EDUCYR", povcat,
    "PCS42", "MCS42", "VPCS42", "VMCS42",
    "HAVEUS42", inscov, insurc, "MCAID53X",
    totslf, obtotv, optotv, ipdis, ipngtd, rxtot, ertot
  )
  
  fyc <- read_MEPS(year = year, type = "FYC") %>%
    select(any_of(vars_to_keep)) %>%
    rename(
      PERWT  = all_of(perwt),
      TTLP   = all_of(ttlp),
      POVCAT = all_of(povcat),
      INSCOV = all_of(inscov),
      TOTSLF = all_of(totslf),
      OBTOTV = all_of(obtotv),
      OPTOTV = all_of(optotv),
      IPDIS  = all_of(ipdis),
      IPNGTD = all_of(ipngtd),
      RXTOT  = all_of(rxtot),
      ERTOT  = all_of(ertot)
    )
  
  cond <- read_MEPS(year = year, type = "COND")
  
  rx <- read_MEPS(year = year, type = "RX")
  
  link <- read_MEPS(year = year, type = "CLNK")
  
  list(fyc = fyc, cond = cond, rx = rx, link = link)

}

save_all_years <- function(years = 2017:2023, 
                           out_file = file.path("data", 
                                                "meps_all_years.rds")) {
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  
  all_data <- list()
  for (yr in years) {
    d <- load_meps_year(yr)
    suffix <- paste0("_", yr)
    all_data[[paste0("fyc", suffix)]]  <- d$fyc
    all_data[[paste0("cond", suffix)]] <- d$cond
    all_data[[paste0("rx", suffix)]]   <- d$rx
    all_data[[paste0("link", suffix)]] <- d$link
  }
  
  saveRDS(all_data, file = out_file)
  invisible(out_file)
}

# Save the MEPS data into an accessible format

# save_all_years()

# ---- DATA CLEANING ----
# Process:
# 1. Read in the datasets of interest
# 2. Clean the FYC data which contains demographic data
# 3. Sort through the NDC FDA data to get drug classes
# 4. USE CLINK files to link ADHD dx-specific rx
# 5. Merge datasets

# ---- READ IN DATA ----
all_data <- readRDS(file.path("data", "meps_all_years.rds"))

years_of_interst <- c(2019, 2021)

for (year in years_of_interst) {
  assign(paste0("fyc_", year), all_data[[paste0("fyc_", year)]])
  assign(paste0("cond_", year), all_data[[paste0("cond_", year)]])
  assign(paste0("rx_", year), all_data[[paste0("rx_", year)]])
  assign(paste0("link_", year), all_data[[paste0("link_", year)]])
}

rm(all_data)
gc()

# ---- CLEAN FYC ----
# in fyc_2019, some things are inappropriately haven labelled
# convert haven labels to numeric
inappropriate_labels <- c("VPCS42", "VMCS42", "OBTOTV", "OPTOTV", "IPDIS", 
                          "IPNGTD", "ERTOT")
fyc_2019 <- fyc_2019 %>%
  mutate(across(any_of(inappropriate_labels), ~ as.numeric(as.character(.x))))
fyc_2021 <- fyc_2021 %>%
  mutate(across(any_of(inappropriate_labels), ~ as.numeric(as.character(.x))))

# convert haven labels to factors in df fyc 
# remove preceding numbers from factor levels
clean_labels <- function(x) {
  if (haven::is.labelled(x)) x <- haven::as_factor(x)
  if (is.factor(x)) levels(x) <- gsub("^[0-9]+\\s*", "", levels(x))
  x
}

fyc_2019 <- fyc_2019 %>% mutate(
  across(where(~ haven::is.labelled(.x) || is.factor(.x)), clean_labels))
fyc_2021 <- fyc_2021 %>% mutate(
  across(where(~ haven::is.labelled(.x) || is.factor(.x)), clean_labels))

# dollars: convert to 2021 dollars using CPI
cpi_2019  <- 256.974
cpi_2021  <- 278.802
cpi_ratio <- cpi_2021 / cpi_2019

fyc_2019$income_2021 <- fyc_2019$TTLP * cpi_ratio
fyc_2019$totslf_2021 <- fyc_2019$TOTSLF * cpi_ratio
fyc_2019$obtotv_2021 <- fyc_2019$OBTOTV * cpi_ratio
fyc_2019$optotv_2021 <- fyc_2019$OPTOTV * cpi_ratio
fyc_2019$rxtot_2021  <- fyc_2019$RXTOT  * cpi_ratio
fyc_2019$ertot_2021  <- fyc_2019$ERTOT  * cpi_ratio

fyc_2021$income_2021 <- fyc_2021$TTLP
fyc_2021$totslf_2021 <- fyc_2021$TOTSLF
fyc_2021$obtotv_2021 <- fyc_2021$OBTOTV
fyc_2021$optotv_2021 <- fyc_2021$OPTOTV
fyc_2021$rxtot_2021  <- fyc_2021$RXTOT
fyc_2021$ertot_2021  <- fyc_2021$ERTOT

# recode

# rename INSURC19 in fyc_2019 to INSURC
# rename INSURC21 in fyc_2021 to INSURC
fyc_2019 <- fyc_2019 %>% rename(INSURC = INSURC19)
fyc_2021 <- fyc_2021 %>% rename(INSURC = INSURC21)

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
        MARRY53X %in% c("-1 INAPPLICABLE", "-7 REFUSED", "-8 DK") 
          ~ NA_character_,
        MARRY53X %in% c("MARRIED", "MARRIED IN ROUND") 
          ~ "Married",
        TRUE 
          ~ "Not married"
      ),
      REGION53 = na_if(REGION53, "-1 INAPPLICABLE"),
      education = case_when(
        EDUCYR %in% c("-1 INAPPLICABLE", "-7 REFUSED", 
                      "-8 DK", "-15 CANNOT BE COMPUTED") 
                      ~ NA_character_,
        EDUCYR %in% c("NO SCHOOL/KINDERGARTEN ONLY", "1", "2", "3", "4", "5", 
                      "6", "7", "8", "9", "10", "11", "GRADE 12") 
                      ~ "High school graduation or less",
        EDUCYR %in% c("1 YEAR COLLEGE", "2 YEARS COLLEGE", "3 YEARS COLLEGE", 
                      "4 YEARS COLLEGE", "5+ YEARS COLLEGE") 
                      ~ "College education or greater",
        TRUE ~ NA_character_
      ),
      insurance = case_when(
        INSURC %in% c("<65 UNINSURED", "65+ UNINSURED") 
          ~ "Uninsured",
        INSURC == "<65 ANY PRIVATE" & MCAID53X != "YES" 
          ~ "Private only",
        INSURC == "<65 ANY PRIVATE" & MCAID53X == "YES" 
          ~ "Medicaid, with private",
        INSURC == "<65 PUBLIC ONLY" & MCAID53X == "YES" 
          ~ "Medicaid only",
        INSURC == "<65 PUBLIC ONLY" & MCAID53X != "YES" 
          ~ "Other public",
        INSURC == "65+ EDITED MEDICARE AND PRIVATE" & MCAID53X != "YES" 
          ~ "Medicare, with private",
        INSURC == "65+ EDITED MEDICARE AND PRIVATE" & MCAID53X == "YES" 
          ~ "Medicare, dual-eligible",
        INSURC == "65+ EDITED MEDICARE ONLY" & MCAID53X != "YES" 
          ~ "Medicare only",
        INSURC == "65+ EDITED MEDICARE ONLY" & MCAID53X == "YES" 
          ~ "Medicare, dual-eligible",
        INSURC == "65+ EDITED MEDICARE AND OTH PUB ONLY" & MCAID53X == "YES" 
          ~ "Medicare, dual-eligible",
        INSURC == "65+ EDITED MEDICARE AND OTH PUB ONLY" & MCAID53X != "YES" 
          ~ "Medicare, with other public",
        INSURC == "65+ NO MEDICARE AND ANY PUBLIC/PRIVATE" & MCAID53X == "YES"
          ~ "Medicaid, with private",
        INSURC == "65+ NO MEDICARE AND ANY PUBLIC/PRIVATE" & MCAID53X != "YES" 
          ~ "Private only",
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
        insurance == "Medicaid, with private" 
          ~ "Medicaid, with private, non-Medicare",
        TRUE ~ "No Medicaid"
      ),
      medicare = case_when(
        insurance %in% c("Medicare, with private", "Medicare, dual-eligible", 
                         "Medicare, with other public", "Medicare only") 
                          ~ "Medicare, any",
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
      ),
      povcat = case_when(
        POVCAT %in% c("POOR/NEGATIVE", "NEAR POOR") ~ "Very low income",
        POVCAT == "LOW INCOME" ~ "Low income",
        POVCAT == "MIDDLE INCOME" ~ "Middle income",
        POVCAT == "HIGH INCOME" ~ "High income",
        TRUE ~ NA_character_
      ),
      povcat = factor(povcat, levels = c(
        "Very low income", 
        "Low income", 
        "Middle income", 
        "High income"
      )),
      region = case_when(
        REGION53 == "NORTHEAST" ~ "Northeast",
        REGION53 == "MIDWEST" ~ "Midwest",
        REGION53 == "SOUTH" ~ "South",
        REGION53 == "WEST" ~ "West",
        TRUE ~ NA_character_
      )
    )
}

fyc_2019 <- recode_fyc(fyc_2019)
fyc_2021 <- recode_fyc(fyc_2021)

# ---- SORT NDC ----
# load ndc
files <- list.files("data/ndc", pattern = "\\.csv$", full.names = TRUE)

ndc_all <- files %>% 
  map_dfr(
    ~ read_csv(.x, col_types = cols(.default = col_character())),
    .id = "source_file"
  ) %>% 
  distinct() %>% 
  filter(
    str_detect(`Pharm Class`, "Central Nervous System Stimulant") |
      str_detect(
        toupper(`Proprietary Name`),
        "ATOMOXETINE|STRATTERA|VILOXAZINE|QELBREE|GUANFACINE|INTUNIV|TENEX||CLONIDINE|KAPVAY|CATAPRES"
      )
  )

ndc_all$formulation <- ifelse(
  str_detect(toupper(ndc_all$`Dosage Form`), "EXTENDED"),
  "Extended",
  "Normal"
)

ndc_stim <- ndc_all %>%
  select(
    `Proprietary Name`,
    `NDC Package Code`,
    `Dosage Form`,
    formulation
  ) %>%
  distinct()

# NDC adds - for legibility; remove to be compatible with MEPS
ndc_stim$ndc_compatible <- str_remove_all(ndc_stim$`NDC Package Code`, "-")

# add leading 0 to match MEPS RX dataset
ndc_stim <- ndc_stim %>% 
  mutate(ndc_compatible = str_pad(ndc_compatible, width = 11, 
                                  side = "left", pad = "0"))

# in rx_2019 and rx_2021, remove year specific suffix (e.g., RXSF19X -> RXSF)
# year-specific vars: RXSF, RXMR, RXMD, RXPV, RXVA, 
#                     RXTR, RXOF, RXSL, RXWC, RXOT, RXXP
rx_2019 <- rx_2019 %>% rename_with(~ str_remove(.x, "19X$"), matches("19X$"))
rx_2021 <- rx_2021 %>% rename_with(~ str_remove(.x, "21X$"), matches("21X$"))

# convert to 2021 dollars using CPI
cpi_2019  <- 256.974
cpi_2021  <- 278.802
cpi_ratio <- cpi_2021 / cpi_2019
rx_2019 <- rx_2019 %>%
  mutate(
    RXSF = RXSF * cpi_ratio,
    RXMR = RXMR * cpi_ratio,
    RXMD = RXMD * cpi_ratio,
    RXPV = RXPV * cpi_ratio,
    RXVA = RXVA * cpi_ratio,
    RXTR = RXTR * cpi_ratio,
    RXOF = RXOF * cpi_ratio,
    RXSL = RXSL * cpi_ratio,
    RXWC = RXWC * cpi_ratio,
    RXOT = RXOT * cpi_ratio,
    RXXP = RXXP * cpi_ratio
  )

rx_2019_ndc <- list(rx_2019 = rx_2019) %>%
  imap_dfr(
    ~ .x %>%
      left_join(ndc_stim, join_by(RXNDC == ndc_compatible), 
                          relationship = "many-to-many") %>%
      filter(!is.na(`NDC Package Code`)) %>%
      mutate(year = as.integer(str_remove(.y, "^rx_")))
  )

rx_2021_ndc <- list(rx_2021 = rx_2021) %>%
  imap_dfr(
    ~ .x %>%
      left_join(ndc_stim, join_by(RXNDC == ndc_compatible), 
                          relationship = "many-to-many") %>%
      filter(!is.na(`NDC Package Code`)) %>%
      mutate(year = as.integer(str_remove(.y, "^rx_")))
  )

# some things are inappropriately haven labelled; convert to numeric first
# and correct labels as needed

# RXBEGMM: convert to numeric and <0 to NA
rx_2019_ndc <- rx_2019_ndc %>% mutate(
  RXBEGMM = as.numeric(as.character(RXBEGMM)))
rx_2021_ndc <- rx_2021_ndc %>% mutate(
  RXBEGMM = as.numeric(as.character(RXBEGMM)))
rx_2019_ndc <- rx_2019_ndc %>% mutate(
  RXBEGMM = ifelse(RXBEGMM < 0, NA, RXBEGMM))
rx_2021_ndc <- rx_2021_ndc %>% mutate(
  RXBEGMM = ifelse(RXBEGMM < 0, NA, RXBEGMM))

# RXBEGYRX: convert to numeric and <0 to NA
rx_2019_ndc <- rx_2019_ndc %>% mutate(
  RXBEGYRX = as.numeric(as.character(RXBEGYRX)))
rx_2021_ndc <- rx_2021_ndc %>% mutate(
  RXBEGYRX = as.numeric(as.character(RXBEGYRX)))
rx_2019_ndc <- rx_2019_ndc %>% mutate(
  RXBEGYRX = ifelse(RXBEGYRX < 0, NA, RXBEGYRX))
rx_2021_ndc <- rx_2021_ndc %>% mutate(
  RXBEGYRX = ifelse(RXBEGYRX < 0, NA, RXBEGYRX))

# RXDAYSUP: convert to numeric and <0 to NA
rx_2019_ndc <- rx_2019_ndc %>% mutate(
  RXDAYSUP = as.numeric(as.character(RXDAYSUP)))
rx_2021_ndc <- rx_2021_ndc %>% mutate(
  RXDAYSUP = as.numeric(as.character(RXDAYSUP)))
rx_2019_ndc <- rx_2019_ndc %>% mutate(
  RXDAYSUP = ifelse(RXDAYSUP < 1, NA, RXDAYSUP))
rx_2021_ndc <- rx_2021_ndc %>% mutate(
  RXDAYSUP = ifelse(RXDAYSUP < 1, NA, RXDAYSUP))

# for some reason, PRN gets coded as 999; convert to NA
rx_2019_ndc <- rx_2019_ndc %>% mutate(
  RXDAYSUP = ifelse(RXDAYSUP == 999, NA, RXDAYSUP))
rx_2021_ndc <- rx_2021_ndc %>% mutate(
  RXDAYSUP = ifelse(RXDAYSUP == 999, NA, RXDAYSUP))

rx_2019_ndc <- rx_2019_ndc %>% mutate(
  across(where(~ haven::is.labelled(.x) || is.factor(.x)), clean_labels))
rx_2021_ndc <- rx_2021_ndc %>% mutate(
  across(where(~ haven::is.labelled(.x) || is.factor(.x)), clean_labels))

# ---- LINKAGE ----
# -----------------------------------------------------------------------------
# Code credit: 
# github.com/HHS-AHRQ/MEPS/blob/master/R/workshop_exercises/cond_pmed_2020.R
#
# National totals:
#   - Total number of people w PMED purchase for ADHD
#   - Total PMED fills for ADHD
#   - Total PMED expenditures for ADHD
#
# Per-person averages among ppl with any PMED for ADHD
#   - Avg PMED fills for ADHD, by SEX and Poverty (POVCAT)
#   - Avg PMED exp for ADHD per person w/ ADHD fills, by SEX and pov (POVCAT)
# 
# Input files:
#   - 2019/2021 Prescribed Medicines file         rx_2019 and rx_2021
#   - 2019/2021 Conditions file                   cond_2019 and cond_2021
#   - 2019/2021 CLNK: Condition-Event Link file   link_2019 and link_2021
#   - 2019/2021 Full-Year Consolidated file       fyc_2019 and fyc_2021
# -----------------------------------------------------------------------------

# Set survey option for lonely PSUs
options(survey.lonely.psu='adjust')
options(survey.adjust.domain.lonely = TRUE)

# >> For PMED file, rename LINKIDX to EVNTIDX to merge with Conditions
rx_2019_ndc <- rx_2019_ndc %>% rename(EVNTIDX = LINKIDX)
rx_2021_ndc <- rx_2021_ndc %>% rename(EVNTIDX = LINKIDX)

# Prepare data for estimation -------------------------------------------------

# Subset condition records to ADHD (any ICD10CDX = "F90")
adhd_2019 <- cond_2019 %>% 
  filter(str_starts(ICD10CDX, "F90"))

adhd_2021 <- cond_2021 %>% 
  filter(str_starts(ICD10CDX, "F90"))

# Merge ADHD conditions with PMED file, using CLNK as crosswalk -----
# De-duplicate on EVNTIDX (so we don't 'double-count' events)

adhd_clnk_2019 <- adhd_2019 %>% 
  inner_join(link_2019, by = c("DUPERSID", "CONDIDX", "PANEL"))
adhd_clnk_2021 <- adhd_2021 %>% 
  inner_join(link_2021, by = c("DUPERSID", "CONDIDX", "PANEL"))
adhd_clnk_distinct_2019 <- adhd_clnk_2019 %>% 
  distinct(PANEL, DUPERSID, EVNTIDX, ICD10CDX, CCSR1X, EVENTYPE)
adhd_clnk_distinct_2021 <- adhd_clnk_2021 %>% 
  distinct(PANEL, DUPERSID, EVNTIDX, ICD10CDX, CCSR1X, EVENTYPE)

# apply clean_labels to adhd_clnk_distinct for PANEL variable
adhd_clnk_distinct_2019 <- adhd_clnk_distinct_2019 %>% mutate(
  PANEL = clean_labels(PANEL))
adhd_clnk_distinct_2021 <- adhd_clnk_distinct_2021 %>% mutate(
  PANEL = clean_labels(PANEL))

adhd_merged_2019 <- adhd_clnk_distinct_2019 %>% 
  inner_join(rx_2019_ndc, by = c("DUPERSID", "EVNTIDX", "PANEL"))
adhd_merged_2021 <- adhd_clnk_distinct_2021 %>% 
  inner_join(rx_2021_ndc, by = c("DUPERSID", "EVNTIDX", "PANEL"))

# QC: check that EVENTYPE = 8 PRESCRIBED MEDICINE for all rows
# TRUE

# QC: View top PMEDS for ADHD
adhd_merged_2019 %>% 
    count(RXDRGNAM) %>% 
    arrange(-n)

# QC: View top PMEDS for ADHD
adhd_merged_2021 %>% 
    count(RXDRGNAM) %>% 
    arrange(-n)

# Roll up to person-level data ------------------------------------------------

drug_by_pers_2019 <- adhd_merged_2019 %>% 
  group_by(DUPERSID) %>% 
  summarize(
    adhd_fills = n_distinct(RXRECIDX),
    adhd_days_supp = sum(RXDAYSUP, na.rm = TRUE),
    adhd_total_spend = sum(RXXP, na.rm = TRUE),
    adhd_oop = sum(RXSF, na.rm = TRUE),
    adhd_private = sum(RXPV, na.rm = TRUE),
    adhd_medicaid = sum(RXMD, na.rm = TRUE),
    n_drug_names = n_distinct(RXDRGNAM),
    n_formulations = n_distinct(formulation),
    drug_names = paste(sort(unique(na.omit(RXDRGNAM))), collapse = "; "),
    formulations = paste(sort(unique(na.omit(formulation))), collapse = "; "),
    pill_qty_total = sum(RXQUANTY, na.rm = TRUE),
    pill_qty_mean = mean(RXQUANTY, na.rm = TRUE),
    .groups = "drop") %>% 
  mutate(adhd_days_supp = ifelse(adhd_days_supp == 0, NA, adhd_days_supp)) %>%
  mutate(adhd_pmed_flag = 1) %>% 
  mutate(oop_share = if_else(adhd_total_spend > 0, 
                             adhd_oop / adhd_total_spend * 100, NA_real_))

drug_by_pers_2021 <- adhd_merged_2021 %>% 
  group_by(DUPERSID) %>% 
  summarize(
    adhd_fills = n_distinct(RXRECIDX),
    adhd_days_supp = sum(RXDAYSUP, na.rm = TRUE),
    adhd_total_spend = sum(RXXP, na.rm = TRUE),
    adhd_oop = sum(RXSF, na.rm = TRUE),
    adhd_private = sum(RXPV, na.rm = TRUE),
    adhd_medicaid = sum(RXMD, na.rm = TRUE),
    n_drug_names = n_distinct(RXDRGNAM),
    n_formulations = n_distinct(formulation),
    drug_names = paste(sort(unique(na.omit(RXDRGNAM))), collapse = "; "),
    formulations = paste(sort(unique(na.omit(formulation))), collapse = "; "),
    pill_qty_total = sum(RXQUANTY, na.rm = TRUE),
    pill_qty_mean = mean(RXQUANTY, na.rm = TRUE),
    .groups = "drop") %>% 
  mutate(adhd_days_supp = ifelse(adhd_days_supp == 0, NA, adhd_days_supp)) %>%
  mutate(adhd_pmed_flag = 1) %>% 
  mutate(oop_share = if_else(adhd_total_spend > 0, 
                             adhd_oop / adhd_total_spend * 100, NA_real_))

# Merge onto FYC file ---------------------------------------------------------
#  >> Need to capture all Strata (VARSTR) and PSUs (VARPSU) for all MEPS sample
#     persons for correct variance estimation

fyc_adhd_merged_2019 <- fyc_2019 %>% 
  full_join(drug_by_pers_2019, by="DUPERSID") %>% 
  replace_na(list(adhd_pmed_flag = 0))

fyc_adhd_merged_2021 <- fyc_2021 %>% 
  full_join(drug_by_pers_2021, by="DUPERSID") %>% 
  replace_na(list(adhd_pmed_flag = 0))

# QC: should have same number of rows as FYC file
# crash if false
stopifnot(nrow(fyc_2019) == nrow(fyc_adhd_merged_2019) && 
          nrow(fyc_2021) == nrow(fyc_adhd_merged_2021))
  
# QC: adhd_pmed_flag counts should be equal to rows in drug_by_pers
flags_2019 <- fyc_adhd_merged_2019 %>% filter(
  adhd_pmed_flag == 1) %>% nrow()
flags_2021 <- fyc_adhd_merged_2021 %>% filter(
  adhd_pmed_flag == 1) %>% nrow()
stopifnot(nrow(drug_by_pers_2019) == flags_2019 && 
          nrow(drug_by_pers_2021) == flags_2021)

# Clean up: remove intermediate dfs
rm(
  adhd_2019, adhd_2021, 
  adhd_clnk_2019, adhd_clnk_2021, 
  adhd_clnk_distinct_2019, adhd_clnk_distinct_2021,
  adhd_merged_2019, adhd_merged_2021, 
  drug_by_pers_2019, drug_by_pers_2021,
  # cond_2019, cond_2021,
  fyc_2019, fyc_2021,
  rx_2019, rx_2021,
  rx_2019_ndc, rx_2021_ndc,
  link_2019, link_2021
)

# ---- FINAL CLEAN ----
# add year variable to fyc_adhd_merged_2019 and fyc_adhd_merged_2021 
# to distinguish in combined dataset
fyc_adhd_merged_2019 <- fyc_adhd_merged_2019 %>% mutate(year = 2019)
fyc_adhd_merged_2021 <- fyc_adhd_merged_2021 %>% mutate(year = 2021)

# from cond files, identify people with ADHD diagnosis
adhd_cond_2019 <- cond_2019 %>% 
  filter(str_starts(ICD10CDX, "F90")) %>% 
  select(DUPERSID) %>% 
  distinct() %>% 
  mutate(adhd_cond_flag = 1)
adhd_cond_2021 <- cond_2021 %>% 
  filter(str_starts(ICD10CDX, "F90")) %>% 
  select(DUPERSID) %>% 
  distinct() %>% 
  mutate(adhd_cond_flag = 1)

# merge onto fyc_adhd_merged_2019 and fyc_adhd_merged_2021
fyc_adhd_merged_2019 <- fyc_adhd_merged_2019 %>% 
  left_join(adhd_cond_2019, by = "DUPERSID") %>% 
  replace_na(list(adhd_cond_flag = 0))
fyc_adhd_merged_2021 <- fyc_adhd_merged_2021 %>% 
  left_join(adhd_cond_2021, by = "DUPERSID") %>% 
  replace_na(list(adhd_cond_flag = 0))

# combined
fyc_clean <- bind_rows(fyc_adhd_merged_2019, fyc_adhd_merged_2021) %>%
  mutate(
    year = factor(year, levels = c(2019, 2021)),

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