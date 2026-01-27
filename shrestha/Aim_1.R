# Prevalence estimates of ADHD per year
# 
# 2017 to 2023

library(MEPS)
library(dplyr)
library(survey)
library(tidyverse)

prev_function = function(year = year){
  
  options(survey.lonely.psu='adjust')
  
  # Build weight variable name based on year (last 2 digits)
  wtvar <- paste0("PERWT", substr(year, 3, 4), "F")
  
  fyc = read_MEPS(year = year, type = "FYC") %>%
    select(DUPERSID, VARSTR, VARPSU, !!sym(wtvar))
  
  cond = read_MEPS(year = year, type = "COND") %>%
    select(DUPERSID, CONDIDX, ICD10CDX)
  
  cond_ADHD = cond %>% 
    filter(ICD10CDX == "F90") %>% 
    mutate(ADHD = 1) %>% 
    distinct(DUPERSID, .keep_all = TRUE) 
  
  print(names(cond_ADHD))
 
  fyc_ADHD = full_join(fyc, cond_ADHD, by="DUPERSID") %>% 
    mutate(ADHD = if_else(is.na(ADHD), 0, 1))
 
  mepsdsgn = svydesign(
    id = ~VARPSU,
    strata = ~VARSTR,
    weights = as.formula(paste0("~", wtvar)),
    data = fyc_ADHD,
    nest = TRUE)
  
 
  print(svytotal(~ADHD, design = mepsdsgn))
  print(svymean(~ADHD, design = mepsdsgn))
}


for (yr in 2016:2023) {
  cat("===== Year:", yr, "=====\n")
  prev_function(year = yr)
  cat("\n")
}
prev_function(2021)

prev_function(2023)
