# 08_flowchart.R
# Goal: Create an inclusion/exclusion flow chart directly from fyc_clean
install.packages("DiagrammeR")
install.packages("DiagrammeRsvg")
install.packages("rsvg")

# 08_flowchart.R
# Person-level inclusion/exclusion flowchart

library(dplyr)
library(stringr)
library(glue)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

if (!dir.exists("exports")) dir.create("exports", recursive = TRUE)

# ---------------------------
# GET STARTING COHORT COUNTS
# ---------------------------

# If fyc_2019/fyc_2021 are not currently loaded, reload them from saved RDS
if (!exists("fyc_2019") | !exists("fyc_2021")) {
  all_data_flow <- readRDS("data/meps_all_years.rds")
  fyc_2019_flow <- all_data_flow$fyc_2019
  fyc_2021_flow <- all_data_flow$fyc_2021
  rm(all_data_flow)
} else {
  fyc_2019_flow <- fyc_2019
  fyc_2021_flow <- fyc_2021
}

ids_2019 <- unique(fyc_2019_flow$DUPERSID)
ids_2021 <- unique(fyc_2021_flow$DUPERSID)

ids_either <- union(ids_2019, ids_2021)
ids_both   <- intersect(ids_2019, ids_2021)

n_combined <- length(ids_either)
n_not_both <- length(setdiff(ids_either, ids_both))
n_both     <- length(ids_both)

# ---------------------------
# PERSON-LEVEL FLAGS FROM FYC_CLEAN
# ---------------------------

flow_person <- fyc_clean %>%
  filter(DUPERSID %in% ids_both) %>%
  group_by(DUPERSID) %>%
  summarize(
    age_eligible = all(AGE53X <= 65, na.rm = TRUE),
    
    medicare_any = any(
      medicare == "Medicare, any" |
        str_detect(as.character(insurance), "Medicare"),
      na.rm = TRUE
    ),
    
    adhd_dx_2019 = any(year == 2019 & flag_adhd_dx == 1, na.rm = TRUE),
    adhd_dx_2021 = any(year == 2021 & flag_adhd_dx == 1, na.rm = TRUE),
    adhd_dx_both_years = adhd_dx_2019 & adhd_dx_2021,
    
    linked_adhd_med_fill_either_year = any(
      adhd_pmed_flag == 1,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# ---------------------------
# SEQUENTIAL FLOW COUNTS
# ---------------------------

flow_counts <- tibble(
  combined = n_combined,
  not_both = n_not_both,
  both = n_both,
  
  age_gt65 = flow_person %>%
    filter(!age_eligible) %>%
    nrow(),
  
  age_eligible = flow_person %>%
    filter(age_eligible) %>%
    nrow(),
  
  medicare_excluded = flow_person %>%
    filter(age_eligible, medicare_any) %>%
    nrow(),
  
  non_medicare_age_eligible = flow_person %>%
    filter(age_eligible, !medicare_any) %>%
    nrow(),
  
  no_adhd_dx_both = flow_person %>%
    filter(age_eligible, !medicare_any, !adhd_dx_both_years) %>%
    nrow(),
  
  adhd_dx_both = flow_person %>%
    filter(age_eligible, !medicare_any, adhd_dx_both_years) %>%
    nrow(),
  
  no_linked_fill = flow_person %>%
    filter(
      age_eligible,
      !medicare_any,
      adhd_dx_both_years,
      !linked_adhd_med_fill_either_year
    ) %>%
    nrow(),
  
  adhd_dx_both_with_fill = flow_person %>%
    filter(
      age_eligible,
      !medicare_any,
      adhd_dx_both_years,
      linked_adhd_med_fill_either_year
    ) %>%
    nrow()
)

fc <- flow_counts[1, ]

# ---------------------------
# FLOWCHART
# ---------------------------

flowchart <- grViz(glue("
digraph flowchart {{

graph [layout=dot, rankdir=TB]

node [
 shape=box
 style=rounded
 fontname=Helvetica
 fontsize=12
 width=5.0
]

A [
label='Combined 2019 and 2021 MEPS cohort\\nN = {fc$combined}'
]

B [
label='Respondents present in both 2019 and 2021\\nN = {fc$both}'
]

C [
label='Age-eligible cohort\\n≤65 years in both study years\\nN = {fc$age_eligible}'
]

D [
label='Non-Medicare age-eligible cohort\\nN = {fc$non_medicare_age_eligible}'
]

E [
label='Respondents with ADHD diagnosis\\nin both 2019 and 2021 (ICD-10 F90)\\nN = {fc$adhd_dx_both}'
]

F [
label='Respondents with ADHD diagnosis in both years\\nand linked ADHD medication fills in either year\\nN = {fc$adhd_dx_both_with_fill}'
]

X1 [
label='Excluded: not present in both years\\nN = {fc$not_both}'
style='rounded,dashed'
]

X2 [
label='Excluded: age >65 in either study year\\nN = {fc$age_gt65}'
style='rounded,dashed'
]

X3 [
label='Excluded: Medicare beneficiary in either study year\\nN = {fc$medicare_excluded}'
style='rounded,dashed'
]

X4 [
label='Excluded: ADHD diagnosis not present in both years\\nN = {fc$no_adhd_dx_both}'
style='rounded,dashed'
]

X5 [
label='Excluded: no linked ADHD medication fill\\nN = {fc$no_linked_fill}'
style='rounded,dashed'
]

A -> B
A -> X1

B -> C
B -> X2

C -> D
C -> X3

D -> E
D -> X4

E -> F
E -> X5

}}
"))

flowchart

# ---------------------------
# EXPORT
# ---------------------------

svg <- export_svg(flowchart)

writeLines(svg, "exports/flowchart.svg")
rsvg_png(charToRaw(svg), "exports/flowchart.png")
rsvg_pdf(charToRaw(svg), "exports/flowchart.pdf")

write.csv(flow_counts, "exports/flowchart_counts.csv", row.names = FALSE)
