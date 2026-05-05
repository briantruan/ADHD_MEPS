# ---- SELECTION ----
# 1. Was ≤65 in **BOTH** 2019 and 2021
# 2. No Medicare in **EITHER** 2019 and 2021
# 3. ADHD diagnosis in **BOTH** 2019 and 2021
# 4. ADHD and RX linkage in **EITHER** 2019 and 2021

# select based on criteria above
selection_counts <- list()

selection_people <- fyc_clean %>% 
  group_by(DUPERSID) %>% 
  summarize(
    # n_years = n_distinct(year),
    age_ok = all(AGE53X <= 65, na.rm = TRUE),
    no_medicare = all(medicare == "No Medicare", na.rm = TRUE),
    adhd_dx = all(adhd_cond_flag == 1, na.rm = TRUE),
    any_adhd_pmed = all(adhd_pmed_flag == 1, na.rm = TRUE),
    .groups = "drop"
  )

selection_counts$total <- nrow(fyc_clean)

# # Find DUPERSIDs that appear in both 2019 and 2021 - don't do this
# selection_counts$step1 <- selection_people %>% 
#   filter(n_years == 2) %>% 
#   nrow()

# Among those in both years,
# select those <= 65 years old
selection_counts$step2 <- selection_people %>% 
  filter(age_ok) %>% 
  nrow()

# Among those in both years and <= 65,
# select those without Medicare
selection_counts$step3 <- selection_people %>% 
  filter(age_ok, no_medicare) %>% 
  nrow()

# Among those in both years, <= 65,
# and without Medicare,
# select those with ADHD diagnosis
selection_counts$step4 <- selection_people %>% 
  filter(age_ok, no_medicare, adhd_dx) %>% 
  nrow()

# Among those in both years, <= 65,
# without Medicare, and with ADHD diagnosis,
# select those with ADHD PMED fills
# (adhd_pmed_flag == 1) in either year
selection_counts$step5 <- selection_people %>% 
  filter(age_ok, no_medicare, adhd_dx, any_adhd_pmed) %>% 
  nrow()

options(survey.lonely.psu = "adjust")

design <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT,
  data = fyc_clean,
  nest = TRUE
)

# removed n_years == 2 criteria to capture all people with ADHD PMED fills in either year
ids_step4 <- selection_people %>%
  filter(age_ok, no_medicare, adhd_dx) %>%
  pull(DUPERSID)

ids_step5 <- selection_people %>%
  filter(age_ok, no_medicare, adhd_dx, any_adhd_pmed == 1) %>%
  pull(DUPERSID)

design_step4 <- subset(design, DUPERSID %in% ids_step4)
design_step5 <- subset(design, DUPERSID %in% ids_step5)

# ---- FLOWCHART ----
# Create flowchart data
flowchart_data <- tibble(
  step = c("Total Records", "Age ≤ 65", "No Medicare", "ADHD Diagnosis", "ADHD PMED Fills"),
  count = c(
    selection_counts$total,
    selection_counts$step2,
    selection_counts$step3,
    selection_counts$step4,
    selection_counts$step5
  ),
  excluded = c(
    0,
    selection_counts$total - selection_counts$step2,
    selection_counts$step2 - selection_counts$step3,
    selection_counts$step3 - selection_counts$step4,
    selection_counts$step4 - selection_counts$step5
  )
)

# Visualize with grViz using glue syntax
flowchart <- grViz(glue("
  digraph {{
    graph [rankdir = TB, nodesep = 0.55, ranksep = 0.65]
    
    node [shape = box, style = 'rounded,filled', fillcolor = lightblue, fontname = Helvetica, fontsize = 12, width = 4.5]
    
    edge [fontname = Helvetica, fontsize = 11]
    
    a [label = 'Total records from 2019 and 2021\\nN = {selection_counts$total}']
    b [label = 'Age-eligible cohort (≤ 65 years)\\nN = {selection_counts$step2}']
    c [label = 'Non-Medicare age-eligible cohort\\nN = {selection_counts$step3}']
    d [label = 'Included analysis cohort (ADHD Diagnosis)\\nN = {selection_counts$step4}']
    e [label = 'Among ADHD patients, cohort with linked fills\\nN = {selection_counts$step5}']
    
    x1 [label = 'Excluded: Age >65\\nN = {selection_counts$total - selection_counts$step2}', style = 'rounded,dashed', width = 3.8]
    x2 [label = 'Excluded: Medicare beneficiaries\\nN = {selection_counts$step2 - selection_counts$step3}', style = 'rounded,dashed', width = 3.8]
    x3 [label = 'Excluded: No ADHD\\nN = {selection_counts$step3 - selection_counts$step4}', style = 'rounded,dashed', width = 3.8]

    a -> b
    b -> c
    c -> d
    d -> e
    
    b -> x1 [constraint = false]
    c -> x2 [constraint = false]
    d -> x3 [constraint = false]
    
    {{ rank = same; b; x1 }}
    {{ rank = same; c; x2 }}
    {{ rank = same; d; x3 }}
  }}
"))

flowchart_svg <- export_svg(flowchart)

writeLines(flowchart_svg, "exports/flowchart.svg")
rsvg_png(charToRaw(flowchart_svg), "exports/flowchart.png")
rsvg_pdf(charToRaw(flowchart_svg), "exports/flowchart.pdf")