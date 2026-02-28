# # Only need to run these once (uncomment if needed)
# install.packages("foreign")  
# install.packages("devtools")
# install.packages("tidyverse")
# install.packages("readr")
# install.packages("readxl")
# install.packages("haven")
# install.packages("survey")

# # Run these every time you restart R:
library(foreign)
library(devtools)
library(tidyverse)
library(readr)
library(readxl)
library(haven)
library(survey)

install_github("sshrestha274/meps_r_pkg/MEPS")

library(MEPS)

# Helper functions for loading MEPS data
# and saving as .rda for easy access 
# (shouldn't need to be uncommented)

# Save data as .rda for easy loading
save_rda <- function(year = year) {
  options(survey.lonely.psu = "adjust")

  # year-specific variables
  # get last 2 digits for year
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

  # vars to keep
  # outcome: number of ADHD-related rx fills per person-year
  # explanatory: post-covid binary indicator

  # covariates: age, sex, race/ethnicity, pov status, SES status
  #             education, marital status, ins cov, geo region
  #             urban/rural, has usual source of care
  #
  # urbanicity: cannot be accessed w/o requesting direct from AHRQ

  # notes
  # insurance: can use INSCOV(YR) or INSURC(YR)
  # more granularity: can use MCAID53X (covered by Medicaid/SCHIP)


  # vars to keep
  vars_to_keep <- c("DUPERSID","PANEL","VARSTR","VARPSU", perwt, ttlp,    # design
                    "SEX", "AGE53X", "RACETHX", "MARRY53X", "REGION53",   # demo/ses
                    "EDUYRDG", "EDUCYR", povcat,                          # demo/ses
                    "PCS42", "MCS42", "VPCS42", "VMCS42",                 # SF-12
                    "HAVEUS42", inscov, insurc, "MCAID53X",               # access
                    totslf, obtotv, optotv, ipdis, ipngtd, rxtot, ertot)  # expenditures

  # read MEPS from lib
  fyc <- read_MEPS(year = year, type = "FYC") %>%
  
    select(any_of(vars_to_keep)) %>%
  
    rename(
      PERWT  = perwt,
      TTLP   = ttlp,
      POVCAT = povcat,
      INSCOV = inscov,
      TOTSLF = totslf,
      OBTOTV = obtotv,
      OPTOTV = optotv,
      IPDIS  = ipdis,
      IPNGTD = ipngtd,
      RXTOT  = rxtot,
      ERTOT  = ertot
    )

  # vars to keep
  cond_vars <- c("DUPERSID", "ICD10CDX", "OPNUM", "OBNUM", 
                 "IPNUM", "ERNUM", "HHNUM")

  rx_vars   <- c("DUPERSID", "RXBEGYRX", "RXBEGMM",
                 "RXDRGNAM", "RXNDC", "RXDAYSUP")

  cond <- read_MEPS(year = year, type = "COND") %>%
    select(any_of(cond_vars))

  rx <- read_MEPS(year = year, type = "RX") %>%
    select(any_of(rx_vars))

  # save as .rda for that year
    # ensure data directory exists
    if (!dir.exists("data")) dir.create("data", recursive = TRUE)

    out_file <- file.path("data", paste0("meps_", year, ".rda"))
  
    save(fyc, cond, rx, file = out_file)

    invisible(out_file)
}

# # uncomment if need to update dataset; otherwise; can directly load
# # the files in 01_eda.r and beyond

# for (year in 2017:2023) {
#   save_rda(year)
# }