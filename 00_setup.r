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

# only run once if needed
# install_github("sshrestha274/meps_r_pkg/MEPS")

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
  
  # RX payment vars
  rxsf <- paste0("RXSF", short_year, "X")
  rxmr <- paste0("RXMR", short_year, "X")
  rxmd <- paste0("RXMD", short_year, "X")
  rxpv <- paste0("RXPV", short_year, "X")
  rxva <- paste0("RXVA", short_year, "X")
  rxtr <- paste0("RXTR", short_year, "X")
  rxof <- paste0("RXOF", short_year, "X")
  rxsl <- paste0("RXSL", short_year, "X")
  rxwc <- paste0("RXWC", short_year, "X")
  rxot <- paste0("RXOT", short_year, "X")
  rxxp <- paste0("RXXP", short_year, "X")
  
  vars_to_keep <- c(
    "DUPERSID", "PANEL", "VARSTR", "VARPSU", perwt, ttlp,
    "SEX", "AGE53X", "RACETHX", "MARRY53X", "REGION53",
    "EDUYRDG", "EDUCYR", povcat,
    "PCS42", "MCS42", "VPCS42", "VMCS42",
    "HAVEUS42", inscov, insurc, "MCAID53X",
    totslf, obtotv, optotv, ipdis, ipngtd, rxtot, ertot
  )
  
  cond_vars <- c(
    "DUPERSID", "ICD10CDX", "OPNUM", "OPCOND",
    "OBNUM", "OBCOND",
    "IPNUM", "IPCOND",
    "ERNUM", "ERCOND",
    "HHNUM", "HHCOND",
    "AGEDIAG", "RXNUM"
  )
  
  rx_vars <- c(
    "DUPERSID", "RXBEGYRX", "RXBEGMM",
    "RXDRGNAM", "RXNDC", "RXDAYSUP",
    rxsf, rxmr, rxmd, rxpv, rxva, rxtr, rxof, rxsl, rxwc, rxot, rxxp
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
  
  cond <- read_MEPS(year = year, type = "COND") %>%
    select(any_of(cond_vars))
  
  rx <- read_MEPS(year = year, type = "RX") %>%
    select(any_of(rx_vars)) %>%
    rename(
      RXSF = all_of(rxsf),
      RXMR = all_of(rxmr),
      RXMD = all_of(rxmd),
      RXPV = all_of(rxpv),
      RXVA = all_of(rxva),
      RXTR = all_of(rxtr),
      RXOF = all_of(rxof),
      RXSL = all_of(rxsl),
      RXWC = all_of(rxwc),
      RXOT = all_of(rxot),
      RXXP = all_of(rxxp)
    )
  
  list(fyc = fyc, cond = cond, rx = rx)
}

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

# save_all_years(2017:2023)