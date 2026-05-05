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
  mutate(ndc_compatible = str_pad(ndc_compatible, width = 11, side = "left", pad = "0"))

# in rx_2019 and rx_2021, remove year specific suffix (e.g., RXSF19X -> RXSF)
# year-specific vars: RXSF, RXMR, RXMD, RXPV, RXVA, RXTR, RXOF, RXSL, RXWC, RXOT, RXXP
rx_2019 <- rx_2019 %>% rename_with(~ str_remove(.x, "19X$"), matches("19X$"))
rx_2021 <- rx_2021 %>% rename_with(~ str_remove(.x, "21X$"), matches("21X$"))

rx_2019_ndc <- list(rx_2019 = rx_2019) %>%
  imap_dfr(
    ~ .x %>%
      left_join(ndc_stim, join_by(RXNDC == ndc_compatible), relationship = "many-to-many") %>%
      filter(!is.na(`NDC Package Code`)) %>%
      mutate(year = as.integer(str_remove(.y, "^rx_")))
  )

rx_2021_ndc <- list(rx_2021 = rx_2021) %>%
  imap_dfr(
    ~ .x %>%
      left_join(ndc_stim, join_by(RXNDC == ndc_compatible), relationship = "many-to-many") %>%
      filter(!is.na(`NDC Package Code`)) %>%
      mutate(year = as.integer(str_remove(.y, "^rx_")))
  )

# some things are inappropriately haven labelled; convert to numeric first
# and correct labels as needed

# RXBEGMM: convert to numeric and <0 to NA
rx_2019_ndc <- rx_2019_ndc %>% mutate(RXBEGMM = as.numeric(as.character(RXBEGMM)))
rx_2021_ndc <- rx_2021_ndc %>% mutate(RXBEGMM = as.numeric(as.character(RXBEGMM)))
rx_2019_ndc <- rx_2019_ndc %>% mutate(RXBEGMM = ifelse(RXBEGMM < 0, NA, RXBEGMM))
rx_2021_ndc <- rx_2021_ndc %>% mutate(RXBEGMM = ifelse(RXBEGMM < 0, NA, RXBEGMM))

# RXBEGYRX: convert to numeric and <0 to NA
rx_2019_ndc <- rx_2019_ndc %>% mutate(RXBEGYRX = as.numeric(as.character(RXBEGYRX)))
rx_2021_ndc <- rx_2021_ndc %>% mutate(RXBEGYRX = as.numeric(as.character(RXBEGYRX)))
rx_2019_ndc <- rx_2019_ndc %>% mutate(RXBEGYRX = ifelse(RXBEGYRX < 0, NA, RXBEGYRX))
rx_2021_ndc <- rx_2021_ndc %>% mutate(RXBEGYRX = ifelse(RXBEGYRX < 0, NA, RXBEGYRX))

# RXDAYSUP: convert to numeric and <0 to NA
rx_2019_ndc <- rx_2019_ndc %>% mutate(RXDAYSUP = as.numeric(as.character(RXDAYSUP)))
rx_2021_ndc <- rx_2021_ndc %>% mutate(RXDAYSUP = as.numeric(as.character(RXDAYSUP)))
rx_2019_ndc <- rx_2019_ndc %>% mutate(RXDAYSUP = ifelse(RXDAYSUP < 1, NA, RXDAYSUP))
rx_2021_ndc <- rx_2021_ndc %>% mutate(RXDAYSUP = ifelse(RXDAYSUP < 1, NA, RXDAYSUP))

# for some reason, PRN gets coded as 999; convert to NA
rx_2019_ndc <- rx_2019_ndc %>% mutate(RXDAYSUP = ifelse(RXDAYSUP == 999, NA, RXDAYSUP))
rx_2021_ndc <- rx_2021_ndc %>% mutate(RXDAYSUP = ifelse(RXDAYSUP == 999, NA, RXDAYSUP))

rx_2019_ndc <- rx_2019_ndc %>% mutate(across(where(~ haven::is.labelled(.x) || is.factor(.x)), clean_labels))
rx_2021_ndc <- rx_2021_ndc %>% mutate(across(where(~ haven::is.labelled(.x) || is.factor(.x)), clean_labels))