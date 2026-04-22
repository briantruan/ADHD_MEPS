# Condition linkage
# Goal: Link condition and rx files using CLINK files
# to ensure that the we use rx specific for ADHD dx.
#
# Each step will follow instructions from MEPS github.

# 1. rename LINKIDX to EVNTIDX to merge with Conditions
rx_2019_ndc <- rx_2019_ndc %>% rename(EVNTIDX = LINKIDX)
rx_2021_ndc <- rx_2021_ndc %>% rename(EVNTIDX = LINKIDX)

# 2. subset to ADHD only
adhd_2019 <- cond_2019 %>% 
  filter(str_detect(ICD10CDX, "^F90")) 
adhd_2021 <- cond_2021 %>% 
  filter(str_detect(ICD10CDX, "^F90"))

# 3. link to CLINK files
clnk_2019 <- adhd_2019 %>% 
  inner_join(link_2019, by = c("DUPERSID", "CONDIDX"), relationship = "many-to-many")
clnk_2021 <- adhd_2021 %>% 
  inner_join(link_2021, by = c("DUPERSID", "CONDIDX"), relationship = "many-to-many")

# 4. remove dupes
clnk_distinct_2019 <- clnk_2019 %>% 
  filter(EVENTYPE == 8) %>% 
  distinct(DUPERSID, EVNTIDX, ICD10CDX, CCSR1X, EVENTYPE, CONDIDX)
clnk_distinct_2021 <- clnk_2021 %>%
  filter(EVENTYPE == 8) %>%  
  distinct(DUPERSID, EVNTIDX, ICD10CDX, CCSR1X, EVENTYPE, CONDIDX)

# QC: should be 0 duplicate DUPERSID + EVNTIDX + CCSR1X + CONDIDX
if (
  clnk_distinct_2019 %>% 
    select(DUPERSID, EVNTIDX, CCSR1X, CONDIDX) %>% 
    duplicated() %>% 
    sum() == 0
) {
  message("No duplicate DUPERSID + EVNTIDX + CCSR1X + CONDIDX in 2019 CLINK data")
} else {
  warning("Duplicate DUPERSID + EVNTIDX + CCSR1X + CONDIDX found in 2019 CLINK data")
}

if (
  clnk_distinct_2021 %>% 
    select(DUPERSID, EVNTIDX, CCSR1X, CONDIDX) %>% 
    duplicated() %>% 
    sum() == 0
) {
  message("No duplicate DUPERSID + EVNTIDX + CCSR1X + CONDIDX in 2021 CLINK data")
} else {
  warning("Duplicate DUPERSID + EVNTIDX + CCSR1X + CONDIDX found in 2021 CLINK data")
}

# 5. link clnk_distinct to rx
adhd_rx_merged_2019 <- clnk_distinct_2019 %>% 
  inner_join(rx_2019_ndc, by = c("DUPERSID", "EVNTIDX"), relationship = "many-to-many") %>% 
  distinct(DUPERSID, RXRECIDX, .keep_all = TRUE) %>% 
  mutate(RXDAYSUP = na_if(RXDAYSUP, 0))
adhd_rx_merged_2021 <- clnk_distinct_2021 %>% 
  inner_join(rx_2021_ndc, by = c("DUPERSID", "EVNTIDX"), relationship = "many-to-many") %>% 
  distinct(DUPERSID, RXRECIDX, .keep_all = TRUE) %>% 
  mutate(RXDAYSUP = na_if(RXDAYSUP, 0))

# QC: check that EVENTYPE = 8 PRESCRIBED MEDICINE for all rows
adhd_rx_merged_2019 %>% 
  count(EVENTYPE)
nrow(adhd_rx_merged_2019)

adhd_rx_merged_2021 %>% 
  count(EVENTYPE)
nrow(adhd_rx_merged_2021)

# 6. roll up to person-level data

adhd_2019_by_person <- adhd_rx_merged_2019 %>% 
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
  mutate(oop_share = if_else(adhd_total_spend > 0, adhd_oop / adhd_total_spend * 100, NA_real_))

adhd_2021_by_person <- adhd_rx_merged_2021 %>% 
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
  mutate(oop_share = if_else(adhd_total_spend > 0, adhd_oop / adhd_total_spend * 100, NA_real_))

# 7. clean up
rm(
  adhd_2019, adhd_2021,
  clnk_2019, clnk_2021,
  clnk_distinct_2019, clnk_distinct_2021,
  adhd_rx_merged_2019, adhd_rx_merged_2021
)
gc()

# QC: Before joining, make sure fyc_2019 and fyc_2021 are unique by DUPERSID.
if (nrow(fyc_2019) == n_distinct(fyc_2019$DUPERSID)) {
  message("fyc_2019 is unique by DUPERSID")
} else {
  warning("fyc_2019 is not unique by DUPERSID")
}

if (nrow(fyc_2021) == n_distinct(fyc_2021$DUPERSID)) {
  message("fyc_2021 is unique by DUPERSID")
} else {
  warning("fyc_2021 is not unique by DUPERSID")
}

# 8. merge to fyc
fyc_adhd_2019 <- fyc_2019 %>% 
  left_join(adhd_2019_by_person, by = "DUPERSID") %>%
  replace_na(list(adhd_pmed_flag = 0))
fyc_adhd_2021 <- fyc_2021 %>% 
  left_join(adhd_2021_by_person, by = "DUPERSID") %>%
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

# 10. keep persons with data in both years only
ids_both <- intersect(fyc_adhd_2019$DUPERSID, fyc_adhd_2021$DUPERSID)
fyc_adhd_2019 <- fyc_adhd_2019 %>% filter(DUPERSID %in% ids_both)
fyc_adhd_2021 <- fyc_adhd_2021 %>% filter(DUPERSID %in% ids_both)

# 11. flag for ADHD in either year using cond files
adhd_cond_2019 <- cond_2019 %>% 
  filter(str_detect(ICD10CDX, "^F90")) %>% 
  distinct(DUPERSID) %>% 
  mutate(flag_adhd_dx = 1)
adhd_cond_2021 <- cond_2021 %>% 
  filter(str_detect(ICD10CDX, "^F90")) %>% 
  distinct(DUPERSID) %>% 
  mutate(flag_adhd_dx = 1)
fyc_adhd_2019 <- fyc_adhd_2019 %>% 
  left_join(adhd_cond_2019, by = "DUPERSID")
fyc_adhd_2021 <- fyc_adhd_2021 %>% 
  left_join(adhd_cond_2021, by = "DUPERSID")
fyc_adhd_2019 <- fyc_adhd_2019 %>% 
  mutate(flag_adhd_dx = replace_na(flag_adhd_dx, 0))
fyc_adhd_2021 <- fyc_adhd_2021 %>% 
  mutate(flag_adhd_dx = replace_na(flag_adhd_dx, 0))

# 12. final cleaned df
fyc_clean <- bind_rows(fyc_adhd_2019, fyc_adhd_2021)

# count num of adhd_pmed_flag vs. flag_adhd_dx
fyc_clean %>% count(adhd_pmed_flag, flag_adhd_dx)

# per shrestha's recommendation, we should log-transform spend vars
# also add binary indicator for post-covid (2021)
fyc_clean <- fyc_clean %>% 
  mutate(
    post_covid = if_else(year == 2021, 1, 0),
    adhd_total_spend_log = log(adhd_total_spend + 1),
    adhd_oop_log = log(adhd_oop + 1),
    adhd_private_log = log(adhd_private + 1),
    adhd_medicaid_log = log(adhd_medicaid + 1)
  )

# remove intermediate dfs
rm(fyc_adhd_2019, fyc_adhd_2021, adhd_cond_2019, adhd_cond_2021, adhd_2019_by_person, adhd_2021_by_person)

# remove unneeded dfs
rm(cond_2019, cond_2021, fyc_2019, fyc_2021, link_2019, link_2021, rx_2019, rx_2021)

gc()