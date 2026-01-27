# Goal
# 
# Replicate findings by Jeun et al. (2023)
# doi.org/10.1177/10870547231210284

# STOP STOP STOP
#
# go to r2_repl_analysis.r to load clean data instead
#
# STOP STOP STOP

# set survey options
options(survey.lonely.psu="adjust")

# # ---------- ONLY NEED TO RUN ONCE ----------
# # uncomment to run if needed
# #
# # import data sources
# # fyc = full-year consolidated: person-level demo data
# fyc2013 <- read_MEPS(year = 2013, type = "FYC")
# fyc2014 <- read_MEPS(year = 2014, type = "FYC")
# fyc2015 <- read_MEPS(year = 2015, type = "FYC")
# fyc2016 <- read_MEPS(year = 2016, type = "FYC")
# fyc2017 <- read_MEPS(year = 2017, type = "FYC")
# fyc2018 <- read_MEPS(year = 2018, type = "FYC")
# fyc2019 <- read_MEPS(year = 2019, type = "FYC")

# # rx = prescribed medicines: event-level
# rx2013 <- read_MEPS(year = 2013, type = "RX")
# rx2014 <- read_MEPS(year = 2014, type = "RX")
# rx2015 <- read_MEPS(year = 2015, type = "RX")
# rx2016 <- read_MEPS(year = 2016, type = "RX")
# rx2017 <- read_MEPS(year = 2017, type = "RX")
# rx2018 <- read_MEPS(year = 2018, type = "RX")
# rx2019 <- read_MEPS(year = 2019, type = "RX")

# # cond = medical conditions: condition-level
# cond2013 <- read_MEPS(year = 2013, type = "COND")
# cond2014 <- read_MEPS(year = 2014, type = "COND")
# cond2015 <- read_MEPS(year = 2015, type = "COND")
# cond2016 <- read_MEPS(year = 2016, type = "COND")
# cond2017 <- read_MEPS(year = 2017, type = "COND")
# cond2018 <- read_MEPS(year = 2018, type = "COND")
# cond2019 <- read_MEPS(year = 2019, type = "COND")

# # save to .rda files for faster loading next time
# save(fyc2013, fyc2014, fyc2015, fyc2016, fyc2017, fyc2018, fyc2019,
#      rx2013, rx2014, rx2015, rx2016, rx2017, rx2018, rx2019,
#      cond2013, cond2014, cond2015, cond2016, cond2017, cond2018, cond2019,
#      file = "data/meps_data_2013_2019.rda")
# # -----------------------------------------

# load data from .rda file
load("data/meps_data_2013_2019.rda")

# -----------------------------------------
# helper functions for processing fyc
# slim_fyc: rename year-specific variables and rename other variables
slim_fyc <- function(fyc_df, year) {
  fyc_df %>%
    select(any_of(vars_keep)) %>%
    rename(
      PERWT  = paste0("PERWT", year, "F"),
      TTLP   = paste0("TTLP", year, "X"),
      POVCAT = paste0("POVCAT", year),
      INSCOV = paste0("INSCOV", year),
      TOTSLF = paste0("TOTSLF", year),
      OBTOTV = paste0("OBTOTV", year),
      OPTOTV = paste0("OPTOTV", year),
      IPDIS  = paste0("IPDIS", year),
      IPNGTD = paste0("IPNGTD", year),
      RXTOT  = paste0("RXTOT", year),
      ERTOT  = paste0("ERTOT", year)
    )
}

# fyc 2013-2015 have EDUYRDG, redefine to EDUC
  # use the following mapping:
  #  <3 = "NO_DEGREE"
  #   3 = "GED"
  # 4-7 = "HS"
  #  >8 = "BA_OR_HIGHER"
educ_13_15_recode <- function(df) {
  df %>%
    mutate(EDUC = case_when(
      EDUYRDG < 3 ~ "NO_DEGREE",
      EDUYRDG == 3 ~ "GED",
      EDUYRDG >= 4 & EDUYRDG <= 7 ~ "HS",
      EDUYRDG > 8 ~ "BA_OR_HIGHER",
      TRUE ~ NA_character_
    )) %>%
    select(-EDUYRDG)
}

# fyc 2016-2019 have EDUCYR, redefine to EDUC
  # use the following mapping:
  # <9    = "NO_DEGREE"
  # 12    = "HS"
  # 13-15 = "SOME_COLLEGE"
  # >15   = "BA_OR_HIGHER"
educ_16_19_recode <- function(df) {
  df %>%
    mutate(EDUC = case_when(
      EDUCYR < 9 ~ "NO_DEGREE",
      EDUCYR == 12 ~ "HS",
      EDUCYR >= 13 & EDUCYR <= 15 ~ "SOME_COLLEGE",
      EDUCYR > 15 ~ "BA_OR_HIGHER",
      TRUE ~ NA_character_
    )) %>%
    select(-EDUCYR)
}

# rename sex
sex_recode <- function(df) {
  df %>%
    mutate(SEX = case_when(
      SEX == 1 ~ "MALE",
      SEX == 2 ~ "FEMALE",
      TRUE ~ NA_character_
    ))
}

# age category
age_cat_recode <- function(df) {
  df %>% 
    mutate(AGE_CAT = case_when(
      AGE53X >= 18 & AGE53X <= 25 ~ "18-25",
      AGE53X >= 26 & AGE53X <= 35 ~ "26-35",
      AGE53X >= 36 & AGE53X <= 45 ~ "36-45",
      AGE53X >= 46 & AGE53X <= 55 ~ "46-55",
      AGE53X >= 56 & AGE53X <= 65 ~ "56-65",
      AGE53X > 65 ~ "65+",
      TRUE ~ NA_character_
    ))
}

# race recode
race_recode <- function(df) {
  df %>% 
    mutate(RACE = case_when(
      RACETHX == 2 ~ "WHITE",
      RACETHX == 1 | RACETHX >= 3 ~ "NON-WHITE",
      TRUE ~ NA_character_
    )) %>% 
    mutate(RACETHX = case_when(
      RACETHX == 1 ~ "HISPANIC",
      RACETHX == 2 ~ "WHITE",
      RACETHX == 3 ~ "BLACK",
      RACETHX == 4 ~ "ASIAN",
      RACETHX == 5 ~ "OTHER",
      TRUE ~ NA_character_
    ))
}

# marital status recode
marital_recode <- function(df) {
  df %>% 
    mutate(MARITAL = case_when(
      MARRY53X == 1 ~ "MARRIED",
      MARRY53X >= 2 & MARRY53X <=6 ~ "NON-MARRIED",
      MARRY53X == 7 ~ "MARRIED",
      MARRY53X >= 8 ~ "NON-MARRIED",
      TRUE ~ NA_character_
    )) %>% 
    select(-MARRY53X)
}

# region recode
region_recode <- function(df) {
  df %>% 
    mutate(REGION53 = case_when(
      REGION53 == 1 ~ "NORTHEAST",
      REGION53 == 2 ~ "MIDWEST",
      REGION53 == 3 ~ "SOUTH",
      REGION53 == 4 ~ "WEST",
      TRUE ~ NA_character_
    ))
}

# income recode
income_recode <- function(df) {
  df %>% 
    mutate(INCOME = case_when(
      TTLP < 50000 ~ "LESS_THAN_50K",
      TTLP >= 50000 & TTLP < 75000 ~ "50K_TO_74K",
      TTLP >= 75000 & TTLP < 100000 ~ "75K_TO_99K",
      TTLP >= 100000 ~ "100K_OR_MORE",
      TRUE ~ NA_character_
    ))
}

# poverty status recode
poverty_recode <- function(df) {
  df %>% 
    mutate(POVERTY = case_when(
      POVCAT == 1 ~ "POOR",
      POVCAT == 2 ~ "POOR",
      POVCAT == 3 ~ "LOW_INCOME",
      POVCAT == 4 ~ "MIDDLE_INCOME",
      POVCAT == 5 ~ "HIGH_INCOME",
      TRUE ~ NA_character_
    )) %>% 
    select(-POVCAT)
}

# insurance type recode
ins_recode <- function(df) {
  df %>% 
    mutate(INSCOV = case_when(
      INSCOV == 3 ~ "UNINSURED",
      INSCOV == 2 ~ "PUBLIC",
      INSCOV == 1 ~ "PRIVATE",
      TRUE ~ NA_character_
    ))
}

pcs_mcs_recode <- function(df) {
  if (!any(c("PCS42", "VPCS42", "MCS42", "VMCS42") %in% names(df))) return(df)

  df |>
    mutate(
      PCS = if ("PCS42" %in% names(df)) PCS42 else if ("VPCS42" %in% names(df)) VPCS42 else NA_real_,
      MCS = if ("MCS42" %in% names(df)) MCS42 else if ("VMCS42" %in% names(df)) VMCS42 else NA_real_
    ) |>
    select(-any_of(c("PCS42", "VPCS42", "MCS42", "VMCS42")))
}

# add_year: add year variable to each data frame
add_year <- function(df, year) {
  df$YEAR <- year
  df
}
# -----------------------------------------

# --process dfs to prevent memory issues---
vars_keep <- c(
  "DUPERSID","PANEL","VARSTR","VARPSU",                 # design

  "PERWT13F", "PERWT14F", "PERWT15F", "PERWT16F",       # design   - weights
  "PERWT17F", "PERWT18F", "PERWT19F",                   # design   - weights

  "SEX", "AGE53X", "RACETHX", "MARRY53X", "REGION53",   # demo/ses - constant

  "EDUYRDG",                                            # educ     - 2013-2015
  "EDUCYR",                                             # educ     - 2016-2019

  "TTLP13X", "TTLP14X", "TTLP15X", "TTLP16X",           # demo/ses - income
  "TTLP17X", "TTLP18X", "TTLP19X",                      # demo/ses - income

  "POVCAT13", "POVCAT14", "POVCAT15", "POVCAT16",       # demo/ses - poverty cat
  "POVCAT17", "POVCAT18", "POVCAT19",                   # demo/ses - poverty cat
  
  "INSCOV13", "INSCOV14", "INSCOV15", "INSCOV16",       # demo/ses - insurance
  "INSCOV17", "INSCOV18", "INSCOV19",                   # demo/ses - insurance

  "TOTSLF13", "TOTSLF14", "TOTSLF15", "TOTSLF16",       # oop
  "TOTSLF17", "TOTSLF18", "TOTSLF19",                   # oop

  "PCS42", "MCS42",                                     # sf12     - 2013-2016
  "VMCS42", "VPCS42",                                   # vr12     - 2017-2019

  "OBTOTV13", "OBTOTV14", "OBTOTV15", "OBTOTV16",       # util     - office-based visits
  "OBTOTV17", "OBTOTV18", "OBTOTV19",                   # util     - office-based visits

  "OPTOTV13", "OPTOTV14", "OPTOTV15", "OPTOTV16",       # util     - outpatient visits
  "OPTOTV17", "OPTOTV18", "OPTOTV19",                   # util     - outpatient visits

  "IPDIS13", "IPDIS14", "IPDIS15", "IPDIS16",           # util     - inpatient discharges
  "IPDIS17", "IPDIS18", "IPDIS19",                      # util     - inpatient discharges

  "IPNGTD13", "IPNGTD14", "IPNGTD15", "IPNGTD16",       # util     - inpatient nights
  "IPNGTD17", "IPNGTD18", "IPNGTD19",                   # util     - inpatient nights

  "RXTOT13", "RXTOT14", "RXTOT15", "RXTOT16",           # util     - prescriptions
  "RXTOT17", "RXTOT18", "RXTOT19",                      # util     - prescriptions

  "ERTOT13", "ERTOT14", "ERTOT15", "ERTOT16",           # util     - emergency room visits
  "ERTOT17", "ERTOT18", "ERTOT19"                       # util     - emergency room visits
)

fyc2013_small <- fyc2013 %>% select(any_of(vars_keep))
fyc2014_small <- fyc2014 %>% select(any_of(vars_keep))
fyc2015_small <- fyc2015 %>% select(any_of(vars_keep))
fyc2016_small <- fyc2016 %>% select(any_of(vars_keep))
fyc2017_small <- fyc2017 %>% select(any_of(vars_keep))
fyc2018_small <- fyc2018 %>% select(any_of(vars_keep))
fyc2019_small <- fyc2019 %>% select(any_of(vars_keep))

rm(fyc2013, fyc2014, fyc2015, fyc2016, fyc2017, fyc2018, fyc2019); gc()

fyc2013_small <- slim_fyc(fyc2013_small, 13)
fyc2014_small <- slim_fyc(fyc2014_small, 14)
fyc2015_small <- slim_fyc(fyc2015_small, 15)
fyc2016_small <- slim_fyc(fyc2016_small, 16)
fyc2017_small <- slim_fyc(fyc2017_small, 17)
fyc2018_small <- slim_fyc(fyc2018_small, 18)
fyc2019_small <- slim_fyc(fyc2019_small, 19)

fyc2013_small <- educ_13_15_recode(fyc2013_small)
fyc2014_small <- educ_13_15_recode(fyc2014_small)
fyc2015_small <- educ_13_15_recode(fyc2015_small)
fyc2016_small <- educ_16_19_recode(fyc2016_small)
fyc2017_small <- educ_16_19_recode(fyc2017_small)
fyc2018_small <- educ_16_19_recode(fyc2018_small)
fyc2019_small <- educ_16_19_recode(fyc2019_small)

fyc2013_small <- sex_recode(fyc2013_small)
fyc2014_small <- sex_recode(fyc2014_small)
fyc2015_small <- sex_recode(fyc2015_small)
fyc2016_small <- sex_recode(fyc2016_small)
fyc2017_small <- sex_recode(fyc2017_small)
fyc2018_small <- sex_recode(fyc2018_small)
fyc2019_small <- sex_recode(fyc2019_small)

fyc2013_small <- age_cat_recode(fyc2013_small)
fyc2014_small <- age_cat_recode(fyc2014_small)
fyc2015_small <- age_cat_recode(fyc2015_small)
fyc2016_small <- age_cat_recode(fyc2016_small)
fyc2017_small <- age_cat_recode(fyc2017_small)
fyc2018_small <- age_cat_recode(fyc2018_small)
fyc2019_small <- age_cat_recode(fyc2019_small)

fyc2013_small <- race_recode(fyc2013_small)
fyc2014_small <- race_recode(fyc2014_small)
fyc2015_small <- race_recode(fyc2015_small)
fyc2016_small <- race_recode(fyc2016_small)
fyc2017_small <- race_recode(fyc2017_small)
fyc2018_small <- race_recode(fyc2018_small)
fyc2019_small <- race_recode(fyc2019_small)

fyc2013_small <- marital_recode(fyc2013_small)
fyc2014_small <- marital_recode(fyc2014_small)
fyc2015_small <- marital_recode(fyc2015_small)
fyc2016_small <- marital_recode(fyc2016_small)
fyc2017_small <- marital_recode(fyc2017_small)
fyc2018_small <- marital_recode(fyc2018_small)
fyc2019_small <- marital_recode(fyc2019_small)

fyc2013_small <- region_recode(fyc2013_small)
fyc2014_small <- region_recode(fyc2014_small)
fyc2015_small <- region_recode(fyc2015_small)
fyc2016_small <- region_recode(fyc2016_small)
fyc2017_small <- region_recode(fyc2017_small)
fyc2018_small <- region_recode(fyc2018_small)
fyc2019_small <- region_recode(fyc2019_small)

fyc2013_small <- income_recode(fyc2013_small)
fyc2014_small <- income_recode(fyc2014_small)
fyc2015_small <- income_recode(fyc2015_small)
fyc2016_small <- income_recode(fyc2016_small)
fyc2017_small <- income_recode(fyc2017_small)
fyc2018_small <- income_recode(fyc2018_small)
fyc2019_small <- income_recode(fyc2019_small)

fyc2013_small <- poverty_recode(fyc2013_small)
fyc2014_small <- poverty_recode(fyc2014_small)
fyc2015_small <- poverty_recode(fyc2015_small)
fyc2016_small <- poverty_recode(fyc2016_small)
fyc2017_small <- poverty_recode(fyc2017_small)
fyc2018_small <- poverty_recode(fyc2018_small)
fyc2019_small <- poverty_recode(fyc2019_small)

fyc2013_small <- ins_recode(fyc2013_small)
fyc2014_small <- ins_recode(fyc2014_small)
fyc2015_small <- ins_recode(fyc2015_small)
fyc2016_small <- ins_recode(fyc2016_small)
fyc2017_small <- ins_recode(fyc2017_small)
fyc2018_small <- ins_recode(fyc2018_small)
fyc2019_small <- ins_recode(fyc2019_small)

fyc2013_small <- pcs_mcs_recode(fyc2013_small)
fyc2014_small <- pcs_mcs_recode(fyc2014_small)
fyc2015_small <- pcs_mcs_recode(fyc2015_small)
fyc2016_small <- pcs_mcs_recode(fyc2016_small)
fyc2017_small <- pcs_mcs_recode(fyc2017_small)
fyc2018_small <- pcs_mcs_recode(fyc2018_small)
fyc2019_small <- pcs_mcs_recode(fyc2019_small)

# bind dfs for fyc
fyc_all <- bind_rows(
  add_year(fyc2013_small, 2013),
  add_year(fyc2014_small, 2014),
  add_year(fyc2015_small, 2015),
  add_year(fyc2016_small, 2016),
  add_year(fyc2017_small, 2017),
  add_year(fyc2018_small, 2018),
  add_year(fyc2019_small, 2019)
)

rm(fyc2013_small, fyc2014_small, fyc2015_small,
   fyc2016_small, fyc2017_small, fyc2018_small,
   fyc2019_small); gc()

# drop EDUCYR from fyc_all
fyc_all <- fyc_all %>% select(-any_of(c("EDUYRDG", "EDUCYR")))
# -----------------------------------------

# ------------process cond dfs-------------

# flag_conditions: flag if person has relevant diagnosis code
flag_conditions <- function(df) {
  df %>%
    mutate(
      ADHD = case_when(
        YEAR <= 2015 & !is.na(ICD9CODX)  & str_detect(ICD9CODX,  "^314") ~ TRUE,
        YEAR >= 2016 & !is.na(ICD10CDX) & str_detect(ICD10CDX, "^F90") ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    mutate(
      ADJUSTMENT_DISORDER = case_when(
        YEAR <= 2015 & !is.na(ICD9CODX)  & str_detect(ICD9CODX,  "^309") ~ TRUE,
        YEAR >= 2016 & !is.na(ICD10CDX) & str_detect(ICD10CDX, "^F43") ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    mutate(
      ANXIETY = case_when(
        YEAR <= 2015 & !is.na(ICD9CODX)  & str_detect(ICD9CODX,  "^300") ~ TRUE,
        YEAR >= 2016 & !is.na(ICD10CDX) & str_detect(ICD10CDX, "^F41") ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    mutate(
      BIPOLAR_DISORDER = case_when(
        YEAR <= 2015 & !is.na(ICD9CODX)  & str_detect(ICD9CODX,  "^296") ~ TRUE,
        YEAR >= 2016 & !is.na(ICD10CDX) & str_detect(ICD10CDX, "^F31") ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    mutate(
      CONDUCT_DISORDER = case_when(
        YEAR <= 2015 & !is.na(ICD9CODX)  & str_detect(ICD9CODX,  "^312") ~ TRUE,
        YEAR >= 2016 & !is.na(ICD10CDX) & str_detect(ICD10CDX, "^F91") ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    mutate(
      DEPRESSION = case_when(
        YEAR <= 2015 & !is.na(ICD9CODX)  & str_detect(ICD9CODX,  "^311") ~ TRUE,
        YEAR >= 2016 & !is.na(ICD10CDX) & str_detect(ICD10CDX, "^F32") ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    select(-any_of(c("ICD9CODX", "ICD10CDX")))
}

# -----------------------------------------

cond_vars_keep <- c(
  "DUPERSID", "PANEL",                                  # design

  "ICD9CODX", "ICD10CDX",                               # diagnosis codes

  "OPNUM", "OBNUM", "IPNUM", "ERNUM", "HHNUM"           # event counts
)

cond2013_small <- cond2013 %>% select(any_of(cond_vars_keep))
cond2014_small <- cond2014 %>% select(any_of(cond_vars_keep))
cond2015_small <- cond2015 %>% select(any_of(cond_vars_keep))
cond2016_small <- cond2016 %>% select(any_of(cond_vars_keep))
cond2017_small <- cond2017 %>% select(any_of(cond_vars_keep))
cond2018_small <- cond2018 %>% select(any_of(cond_vars_keep))
cond2019_small <- cond2019 %>% select(any_of(cond_vars_keep))

rm(list = paste0("cond", 2013:2019)); gc()

# bind dfs for cond
cond_all <- bind_rows(
  add_year(cond2013_small, 2013),
  add_year(cond2014_small, 2014),
  add_year(cond2015_small, 2015),
  add_year(cond2016_small, 2016),
  add_year(cond2017_small, 2017),
  add_year(cond2018_small, 2018),
  add_year(cond2019_small, 2019)
)

rm(list = ls(pattern = "cond[0-9]+_small")); gc()

cond_hcu <- cond_all %>%
  flag_conditions() %>% 
  group_by(DUPERSID, YEAR) %>%
  summarise(
    COND_OP = sum(OPNUM, na.rm = TRUE),
    COND_OB = sum(OBNUM, na.rm = TRUE),
    COND_IP = sum(IPNUM, na.rm = TRUE),
    COND_ER = sum(ERNUM, na.rm = TRUE),
    COND_HH = sum(HHNUM, na.rm = TRUE),
    ADHD                = any(ADHD, na.rm = TRUE),
    ADJUSTMENT_DISORDER = any(ADJUSTMENT_DISORDER, na.rm = TRUE),
    ANXIETY             = any(ANXIETY, na.rm = TRUE),
    BIPOLAR_DISORDER    = any(BIPOLAR_DISORDER, na.rm = TRUE),
    CONDUCT_DISORDER    = any(CONDUCT_DISORDER, na.rm = TRUE),
    DEPRESSION          = any(DEPRESSION, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    COMORBID = if_else(ADHD, rowSums(select(., ADJUSTMENT_DISORDER, 
    ANXIETY, BIPOLAR_DISORDER, CONDUCT_DISORDER, DEPRESSION), na.rm = TRUE), NA_integer_),

    COMORBID_CAT = case_when(
      COMORBID == 0   ~ "NONE",
      COMORBID <= 2   ~ "1-2",
      COMORBID <= 4   ~ "3-4",
      COMORBID == 5   ~ "5",
      TRUE            ~ NA_character_
    )
  )

# we are going to filter later
#
# adhd_hcu <- cond_all %>%
#   flag_adhd() %>%
#   filter(ADHD == TRUE) %>%
#   group_by(DUPERSID, YEAR) %>%
#   summarise(
#     ADHD_OP = sum(OPNUM, na.rm = TRUE),
#     ADHD_OB = sum(OBNUM, na.rm = TRUE),
#     ADHD_IP = sum(IPNUM, na.rm = TRUE),
#     ADHD_ER = sum(ERNUM, na.rm = TRUE),
#     ADHD_HH = sum(HHNUM, na.rm = TRUE),
#     .groups = "drop"
#   )

# -------------process rx dfs--------------

rx_vars_keep <- c(
  "DUPERSID", "PANEL",                                  # design

  "RXBEGYRX", "RXBEGMM", "RXDRGNAM", "RXNDC",           # rx info
  "RXDAYSUP"                                            # days supplied
)

# -----------------------------------------

rx2013_small <- rx2013 %>% select(any_of(rx_vars_keep))
rx2014_small <- rx2014 %>% select(any_of(rx_vars_keep))
rx2015_small <- rx2015 %>% select(any_of(rx_vars_keep))
rx2016_small <- rx2016 %>% select(any_of(rx_vars_keep))
rx2017_small <- rx2017 %>% select(any_of(rx_vars_keep))
rx2018_small <- rx2018 %>% select(any_of(rx_vars_keep))
rx2019_small <- rx2019 %>% select(any_of(rx_vars_keep))

rm(list = paste0("rx", 2013:2019)); gc()

# bind dfs for rx
rx_all <- bind_rows(
  add_year(rx2013_small, 2013),
  add_year(rx2014_small, 2014),
  add_year(rx2015_small, 2015),
  add_year(rx2016_small, 2016),
  add_year(rx2017_small, 2017),
  add_year(rx2018_small, 2018),
  add_year(rx2019_small, 2019)
)

rm(list = ls(pattern = "rx[0-9]+_small")); gc()

ndc_tbl <- read_csv("data/ndc_codes.csv")

ndc_tbl <- ndc_tbl %>%
  mutate(
    ndc_clean = gsub("[^0-9]", "", ndc),
    drug_name = as.factor(drug_name),
    release   = as.factor(release)
  ) %>%
  filter(ndc_clean != "") %>%
  distinct(ndc_clean, drug_name, release)

rx_all <- rx_all %>%
  mutate(
    RXNDC_clean = gsub("[^0-9]", "", RXNDC)
  )

rx_all <- rx_all %>% 
  left_join(ndc_tbl, by = c("RXNDC_clean" = "ndc_clean"))

rx_first <- rx_all %>% 
  filter(!is.na(drug_name)) %>%
  group_by(DUPERSID, YEAR) %>%
  summarise(
    first_yr = min(RXBEGYRX, na.rm = TRUE),
    first_mm = min(RXBEGMM, na.rm = TRUE),
    .groups   = "drop"
  )

rx_adhd_events <- rx_all %>% 
  filter(!is.na(drug_name)) %>%
  left_join(rx_first, by = c("DUPERSID", "YEAR")) %>%
  mutate(
    new_user = (RXBEGYRX == first_yr)
  )

rx_py_drug <- rx_adhd_events %>%
  mutate(
    # Keep only valid months 1–12; everything else becomes NA
    RXBEGMM_clean = if_else(RXBEGMM >= 1 & RXBEGMM <= 12,
                            RXBEGMM,
                            NA_real_)
  ) %>%
  group_by(DUPERSID, YEAR) %>%
  summarise(
    total_days = sum(RXDAYSUP, na.rm = TRUE),
    n_fills    = sum(!is.na(drug_name)),

    # first valid month among ADHD fills in that year
    first_mm   = if (all(is.na(RXBEGMM_clean))) NA_real_
                 else min(RXBEGMM_clean, na.rm = TRUE),

    new_user   = all(new_user),

    # drug counts
    n_amp = sum(drug_name == "AMPHETAMINE-DEXTROAMPHETAMINE", na.rm = TRUE),
    n_mpd = sum(drug_name == "METHYLPHENIDATE", na.rm = TRUE),
    n_lis = sum(drug_name == "LISDEXAMFETAMINE", na.rm = TRUE),
    n_dex = sum(drug_name == "DEXMETHYLPHENIDATE", na.rm = TRUE),

    # formulation counts
    n_imr = sum(release == "IR", na.rm = TRUE),
    n_exr = sum(release == "ER", na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    # Denominator logic:
    denom_days = case_when(
      n_fills == 0                      ~ NA_real_,         # no ADHD meds
      new_user & is.na(first_mm)        ~ NA_real_,         # missing start month → exclude
      new_user & !is.na(first_mm)       ~ (12 - first_mm + 1) * 30,
      !new_user                         ~ 365
    ),
    # Cap denominator between 1 and 365
    denom_days = pmin(pmax(denom_days, 1), 365),

    # PDC: must be between 0 and 1; set to NA if denom missing or nonpositive
    PDC = dplyr::case_when(
      is.na(denom_days)             ~ NA_real_,
      denom_days <= 0               ~ NA_real_,
      TRUE                          ~ pmin(pmax(total_days / denom_days, 0), 1)
    ),

    # drug name and formulation indicators
    IS_IR = if_else(n_imr > 0, TRUE, FALSE),
    IS_ER = if_else(n_exr > 0, TRUE, FALSE)
  )

rx_py_all <- rx_py_drug %>%
  group_by(DUPERSID, YEAR) %>%
  summarise(
    avg_PDC = mean(PDC, na.rm = TRUE),  # average across drugs
    total_days = total_days,
    n_fills = n_fills,
    n_drugs = sum(!is.na(PDC)),

    # molecule counts (any exposure to that drug in the year)
    n_amp  = n_amp,
    n_mpd  = n_mpd,
    n_lis  = n_lis,
    n_dex  = n_dex,

    # formulation counts (across all ADHD drugs)
    n_imr  = n_imr,
    n_exr  = n_exr,

    # drug and formulation indicators
    IS_IR     = any(IS_IR, na.rm = TRUE),
    IS_ER     = any(IS_ER, na.rm = TRUE),

    any_new = any(new_user, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    avg_PDC   = if_else(is.nan(avg_PDC), NA_real_, avg_PDC),  # if all NA
    ADHERENT  = if_else(!is.na(avg_PDC),
                        as.integer(avg_PDC >= 0.8),
                        NA_integer_)
  )

# merge without filters
unfiltered_data <- fyc_all %>%
  left_join(cond_hcu, by = c("DUPERSID", "YEAR")) %>%
  left_join(rx_py_all, by = c("DUPERSID", "YEAR"))

# save for faster loading
save(unfiltered_data, file = "data/meps_jeun_unfiltered.rda")