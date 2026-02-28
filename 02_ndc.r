# NDC drug codes (FDA)
# Goal: Use the rx dataset and isolate drugs that are
# FDA-approved for the treatment of ADHD
# Also determine if drugs are extended release

# load ndc
files <- list.files("data/ndc", pattern = "\\.csv$", full.names = TRUE)

ndc_all <- files |>
  purrr::map_dfr(
    ~ readr::read_csv(.x, col_types = readr::cols(.default = readr::col_character())),
    .id = "source_file"
  ) |>
  dplyr::distinct()

ndc_all
