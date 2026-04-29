# 08_flowchart.R
# Person-level inclusion/exclusion flowchart
# Main analytic cohort ends at persistent ADHD diagnosis cohort (N=195)
# Side note indicates linked medication fill subgroup (N=102)

library(dplyr)
library(stringr)
library(glue)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

if (!dir.exists("exports")) dir.create("exports", recursive = TRUE)

# ---------------------------
# LOAD SOURCE DATA IF NEEDED
# ---------------------------

if (!exists("fyc_2019") | !exists("fyc_2021")) {
  all_data_flow <- readRDS("data/meps_all_years.rds")
  fyc_2019_flow <- all_data_flow$fyc_2019
  fyc_2021_flow <- all_data_flow$fyc_2021
  rm(all_data_flow)
} else {
  fyc_2019_flow <- fyc_2019
  fyc_2021_flow <- fyc_2021
}

# ---------------------------
# INITIAL COHORT COUNTS
# ---------------------------

ids_2019 <- unique(fyc_2019_flow$DUPERSID)
ids_2021 <- unique(fyc_2021_flow$DUPERSID)

ids_either <- union(ids_2019, ids_2021)
ids_both   <- intersect(ids_2019, ids_2021)

n_combined <- length(ids_either)
n_not_both <- length(setdiff(ids_either, ids_both))
n_both     <- length(ids_both)

# ---------------------------
# PERSON-LEVEL FLAGS
# ---------------------------

flow_person <- fyc_clean %>%
  filter(DUPERSID %in% ids_both) %>%
  group_by(DUPERSID) %>%
  summarize(
    
    # eligible in both years
    age_eligible = all(AGE53X <= 65, na.rm = TRUE),
    
    # exclude Medicare
    medicare_any = any(
      medicare == "Medicare, any" |
        str_detect(as.character(insurance), "Medicare"),
      na.rm = TRUE
    ),
    
    # ADHD diagnosis in BOTH years
    adhd_dx_2019 = any(year == 2019 & flag_adhd_dx == 1, na.rm = TRUE),
    adhd_dx_2021 = any(year == 2021 & flag_adhd_dx == 1, na.rm = TRUE),
    adhd_dx_both_years = adhd_dx_2019 & adhd_dx_2021,
    
    # linked med fills in EITHER year
    linked_fill_either = any(
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
  
  linked_fill_subgroup = flow_person %>%
    filter(
      age_eligible,
      !medicare_any,
      adhd_dx_both_years,
      linked_fill_either
    ) %>%
    nrow()
)

fc <- flow_counts[1,]

# ---------------------------
# FLOWCHART
# ---------------------------

flowchart <- grViz(glue("
digraph flowchart {{

graph [
  layout=dot,
  rankdir=TB,
  nodesep=0.55,
  ranksep=0.65
]

node [
  shape=box,
  style=rounded,
  fontname=Helvetica,
  fontsize=12,
  width=5.2
]

edge [
  fontname=Helvetica,
  fontsize=11
]

A [
label='Combined 2019 and 2021 MEPS respondents\\nN = {fc$combined}'
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
label='Included analytic ADHD cohort\\nRespondents with ADHD diagnosis\\nin both 2019 and 2021 (ICD-10 F90)\\nN = {fc$adhd_dx_both}'
]

F [
label='Among included ADHD cohort:\\n{fc$linked_fill_subgroup} had linked ADHD medication fills',
shape=note
]

X1 [
label='Excluded: not present in both years\\nN = {fc$not_both}',
style='rounded,dashed',
width=4.3
]

X2 [
label='Excluded: age >65 in either study year\\nN = {fc$age_gt65}',
style='rounded,dashed',
width=4.3
]

X3 [
label='Excluded: Medicare beneficiary\\nin either study year\\nN = {fc$medicare_excluded}',
style='rounded,dashed',
width=4.3
]

X4 [
label='Excluded: ADHD diagnosis absent\\nin one or both study years\\nN = {fc$no_adhd_dx_both}',
style='rounded,dashed',
width=4.3
]

# Main vertical path
A -> B
B -> C
C -> D
D -> E
E -> F [style=dotted]

# Side exclusion boxes
B -> X1 [constraint=false]
C -> X2 [constraint=false]
D -> X3 [constraint=false]
E -> X4 [constraint=false]

# Force exclusion boxes to sit beside corresponding main boxes
{{ rank=same; B; X1 }}
{{ rank=same; C; X2 }}
{{ rank=same; D; X3 }}
{{ rank=same; E; X4 }}

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

write.csv(
  flow_counts,
  "exports/flowchart_counts.csv",
  row.names = FALSE
)
