# NDC drug codes (FDA)
# Goal: Use the rx dataset and isolate drugs that are
# FDA-approved for the treatment of ADHD
# Also determine if drugs are extended release

# load ndc
files <- list.files("data/ndc", pattern = "\\.csv$", full.names = TRUE)

ndc_all <- files %>% 
  map_dfr(
    ~ read_csv(.x, col_types = cols(.default = col_character())),
    .id = "source_file"
  ) %>% 
  distinct() %>% 
  filter(str_detect(`Pharm Class`, "Central Nervous System Stimulant"))

ndc_all$formulation <- NA
ndc_all$formulation <- ifelse(str_detect(ndc_all$`Dosage Form`, "EXTENDED"), "Extended", "Normal")

ndc_stim <- ndc_all %>%
  select(
    `Proprietary Name`,
    `NDC Package Code`,
    `Dosage Form`,
    formulation
  )

# NDC adds - for legibility; remove to be compatible with meps
ndc_stim$ndc_compatible <- str_remove_all(ndc_stim$`NDC Package Code`, "-")

# need to add a leading 0 to match MEPS RX dataset
ndc_stim <- ndc_stim %>% 
  mutate(ndc_compatible = str_pad(ndc_compatible, width = 11, side = "left", pad = "0"))

rx_ndc <- rx %>% 
  left_join(ndc_stim, join_by(RXNDC == ndc_compatible), relationship = "many-to-many") %>% 
  filter(!is.na(`NDC Package Code`)) %>% 
  select(
    DUPERSID,
    RXBEGYRX,
    RXBEGMM,
    RXDRGNAM,
    RXNDC,
    RXDAYSUP,
    formulation
  )