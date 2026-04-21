# Condition linkage
# Goal: Link condition and rx files using CLINK files
# to ensure that the we use rx specific for ADHD dx.
#
# Each step will follow instructions from MEPS github.

# 1. rename LINKIDX to EVNTIDX to merge with Conditions
rx_2019 <- rx_2019 %>% rename(EVNTIDX = LINKIDX)
rx_2021 <- rx_2021 %>% rename(EVNTIDX = LINKIDX)

# 2. subset to ADHD only
adhd_2019 <- cond_2019 %>% 
  filter(str_detect(ICD10CDX, "^F90"))
adhd_2021 <- cond_2021 %>% 
  filter(str_detect(ICD10CDX, "^F90"))

# 3. link to CLINK files
clnk_2019 <- adhd_2019 %>% 
  inner_join(link_2019, by = c("DUPERSID", "CONDIDX"))
clnk_2021 <- adhd_2021 %>% 
  inner_join(link_2021, by = c("DUPERSID", "CONDIDX"))

# 4. remove dupes
clnk_distinct_2019 <- clnk_2019 %>% 
  distinct(DUPERSID, EVNTIDX, ICD10CDX, CCSR1X, EVENTYPE)
clnk_distinct_2021 <- clnk_2021 %>% 
  distinct(DUPERSID, EVNTIDX, ICD10CDX, CCSR1X, EVENTYPE)

# QC: should be 0 duplicate EVNTIDX
  if (clnk_distinct_2019 %>% pull(EVNTIDX) %>% duplicated() %>% sum() == 0) {
    message("No duplicate EVNTIDX in 2019 CLINK data")
  } else {
    warning("Duplicate EVNTIDX found in 2019 CLINK data")
  }
  if (clnk_distinct_2021 %>% pull(EVNTIDX) %>% duplicated() %>% sum() == 0) {
    message("No duplicate EVNTIDX in 2021 CLINK data")
  } else {
    warning("Duplicate EVNTIDX found in 2021 CLINK data")
  }

# 5. link clnk_distinct to rx
adhd_rx_merged_2019 <- clnk_distinct_2019 %>% 
  inner_join(rx_2019, by = c("DUPERSID", "EVNTIDX")) 
adhd_rx_merged_2021 <- clnk_distinct_2021 %>% 
  inner_join(rx_2021, by = c("DUPERSID", "EVNTIDX"))

# QC: check that EVENTYPE = 8 PRESCRIBED MEDICINE for all rows
adhd_rx_merged_2019 %>% 
  count(EVENTYPE)

adhd_rx_merged_2021 %>% 
  count(EVENTYPE)

# 6. roll up to person-level data

adhd_2019_by_person <- adhd_rx_merged_2019 %>% 
  group_by(DUPERSID) %>% 
  summarize(
    adhd_fills = n_distinct(RXRECIDX),
    adhd_total_spend = sum(RXXP),
    adhd_oop = sum(RXSF),
    adhd_private = sum(RXPV),
    adhd_medicaid = sum(RXMD)) %>% 
  mutate(adhd_pmed_flag = 1) %>% 
  mutate(oop_share = if_else(adhd_total_spend > 0, adhd_oop / adhd_total_spend * 100, NA_real_))

adhd_2021_by_person <- adhd_rx_merged_2021 %>% 
  group_by(DUPERSID) %>% 
  summarize(
    adhd_fills = n_distinct(RXRECIDX),
    adhd_total_spend = sum(RXXP),
    adhd_oop = sum(RXSF),
    adhd_private = sum(RXPV),
    adhd_medicaid = sum(RXMD)) %>% 
  mutate(adhd_pmed_flag = 1) %>% 
  mutate(oop_share = if_else(adhd_total_spend > 0, adhd_oop / adhd_total_spend * 100, NA_real_))

# 7. clean up
rm(
  adhd_2019, adhd_2021,
  clnk_2019, clnk_2021,
  clnk_distinct_2019, clnk_distinct_2021,
  adhd_rx_merged_2019, adhd_rx_merged_2021
)
gc()

# 8. merge to fyc
fyc_adhd_2019 <- fyc_2019 %>% 
  full_join(adhd_2019_by_person, by = "DUPERSID") %>%
  replace_na(list(adhd_pmed_flag = 0))
fyc_adhd_2021 <- fyc_2021 %>% 
  full_join(adhd_2021_by_person, by = "DUPERSID") %>%
  replace_na(list(adhd_pmed_flag = 0))

# QC: check nrows
nrow(fyc_2019) == nrow(fyc_adhd_2019)
nrow(fyc_2021) == nrow(fyc_adhd_2021)

# QC: check flag counts
fyc_adhd_2019 %>% count(adhd_pmed_flag == 1)
nrow(adhd_2019_by_person)
fyc_adhd_2021 %>% count(adhd_pmed_flag == 1)
nrow(adhd_2021_by_person)

# 9. add year variable and combine fyc_adhd_2019 and fyc_adhd_2021
fyc_adhd_2019 <- fyc_adhd_2019 %>% mutate(year = 2019)
fyc_adhd_2021 <- fyc_adhd_2021 %>% mutate(year = 2021)

# final cleaned df
fyc_adhd_clean <- bind_rows(fyc_adhd_2019, fyc_adhd_2021)

# 10. define design variable
meps_design <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT,
  data = fyc_adhd_clean,
  nest = TRUE
)

meps_design_adhd <- subset(meps_design, adhd_pmed_flag == 1) 
meps_design_adhd_2019 <- subset(meps_design_adhd, year == 2019)
meps_design_adhd_2021 <- subset(meps_design_adhd, year == 2021)

# Make a binary factor for year and run svyttest
meps_design_adhd <- update(meps_design_adhd,
  year_bin = factor(ifelse(year == 2021, "2021", "2019"))
)

# 11. estimates
svymean(~ adhd_fills +
          adhd_total_spend +
          adhd_oop +
          adhd_private +
          adhd_medicaid,
          design = meps_design_adhd_2019)

svymean(~ adhd_fills +
          adhd_total_spend +
          adhd_oop +
          adhd_private +
          adhd_medicaid,
          design = meps_design_adhd_2021)

# hist to evaluate normality
svyhist(~ adhd_fills, design = meps_design_adhd)
svyhist(~ adhd_total_spend, design = meps_design_adhd)
svyhist(~ adhd_oop, design = meps_design_adhd)
svyhist(~ adhd_private, design = meps_design_adhd)
svyhist(~ adhd_medicaid, design = meps_design_adhd)

# mann-whitney u test
svyranktest(adhd_fills ~ year_bin, design = meps_design_adhd)
svyranktest(adhd_total_spend ~ year_bin, design = meps_design_adhd)
svyranktest(adhd_oop ~ year_bin, design = meps_design_adhd)
svyranktest(adhd_private ~ year_bin, design = meps_design_adhd)
svyranktest(adhd_medicaid ~ year_bin, design = meps_design_adhd)
