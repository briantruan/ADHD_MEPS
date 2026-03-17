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

# Load MEPS data for a single year and return a named list: fyc, cond, rx
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

  vars_to_keep <- c("DUPERSID","PANEL","VARSTR","VARPSU", perwt, ttlp,
                    "SEX", "AGE53X", "RACETHX", "MARRY53X", "REGION53",
                    "EDUYRDG", "EDUCYR", povcat,
                    "PCS42", "MCS42", "VPCS42", "VMCS42",
                    "HAVEUS42", inscov, insurc, "MCAID53X",
                    totslf, obtotv, optotv, ipdis, ipngtd, rxtot, ertot)

  cond_vars <- c("DUPERSID", "ICD10CDX", "OPNUM", "OPCOND",
                 "OBNUM", "OBCOND",
                 "IPNUM", "IPCOND",
                 "ERNUM", "ERCOND",
                 "HHNUM", "HHCOND")

  rx_vars   <- c("DUPERSID", "RXBEGYRX", "RXBEGMM",
                 "RXDRGNAM", "RXNDC", "RXDAYSUP")

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

  cond <- read_MEPS(year = year, type = "COND") %>%
    select(any_of(cond_vars))

  rx <- read_MEPS(year = year, type = "RX") %>%
    select(any_of(rx_vars))

  list(fyc = fyc, cond = cond, rx = rx)
}


# Save multiple years into a single RDS file. Each dataset is stored with a
# year-suffixed name (e.g. fyc_2017, cond_2017, rx_2017) inside a single list.
save_all_years <- function(years = 2017:2023, out_file = file.path("data", "meps_all_years.rds")) {
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)

  all_data <- list()
  for (yr in years) {
    d <- load_meps_year(yr)
    suffix <- paste0("_", yr)
    all_data[[paste0("fyc", suffix)]]  <- d$fyc
    all_data[[paste0("cond", suffix)]] <- d$cond
    all_data[[paste0("rx", suffix)]]   <- d$rx
  }

  saveRDS(all_data, file = out_file)
  invisible(out_file)
}

# Uncomment to run:
save_all_years(2017:2023)