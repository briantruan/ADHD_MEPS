# 08_flowchart.R
# Goal: Create an inclusion/exclusion flow chart directly from fyc_clean
install.packages("DiagrammeR")
install.packages("DiagrammeRsvg")
install.packages("rsvg")

library(dplyr)
library(glue)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

if (!dir.exists("exports")) dir.create("exports", recursive = TRUE)

# ---------------------------
# PERSON-YEAR FLOW COUNTS
# ---------------------------

flow_counts_py <- fyc_clean %>%
  mutate(
    age_le_65 = AGE53X <= 65,
    adhd_dx = flag_adhd_dx == 1,
    adhd_dx_and_fill = flag_adhd_dx == 1 & adhd_pmed_flag == 1
  ) %>%
  summarize(
    matched_person_years = n(),
    
    excluded_age_gt65 = sum(!age_le_65, na.rm = TRUE),
    age_le65_person_years = sum(age_le_65, na.rm = TRUE),
    
    excluded_no_adhd_dx = sum(age_le_65 & !adhd_dx, na.rm = TRUE),
    adhd_dx_person_years = sum(age_le_65 & adhd_dx, na.rm = TRUE),
    
    excluded_no_fill = sum(age_le_65 & adhd_dx & !adhd_dx_and_fill, na.rm = TRUE),
    adhd_dx_fill_person_years = sum(age_le_65 & adhd_dx_and_fill, na.rm = TRUE)
  )

fc <- flow_counts_py[1, ]

# ---------------------------
# FLOW CHART
# ---------------------------

flowchart_py <- grViz(glue("
digraph flowchart {{

graph [layout=dot, rankdir=TB]

node [
 shape=box
 style=rounded
 fontname=Helvetica
 fontsize=12
 width=4.8
]

A [
label='Matched respondents present in both 2019 and 2021\\nN = {fc$matched_person_years} person-year observations'
]

B [
label='Age-eligible cohort\\nN = {fc$age_le65_person_years} person-year observations'
]

C [
label='Observations with ADHD diagnosis\\nN = {fc$adhd_dx_person_years} person-year observations'
]

D [
label='Observations with ADHD diagnosis and linked ADHD medication fills\\nN = {fc$adhd_dx_fill_person_years} person-year observations'
]

E [
label='Excluded: age >65\\nN = {fc$excluded_age_gt65} person-year observations'
style='rounded,dashed'
]

F [
label='Excluded: no ADHD diagnosis\\nN = {fc$excluded_no_adhd_dx} person-year observations'
style='rounded,dashed'
]

G [
label='Excluded: no linked ADHD medication fills\\nN = {fc$excluded_no_fill} person-year observations'
style='rounded,dashed'
]

A -> B
A -> E

B -> C
B -> F

C -> D
C -> G

}}
"))

flowchart_py

# ---------------------------
# EXPORT
# ---------------------------

svg_py <- export_svg(flowchart_py)

writeLines(svg_py, "exports/flowchart_person_years.svg")
rsvg_png(charToRaw(svg_py), "exports/flowchart_person_years.png")
rsvg_pdf(charToRaw(svg_py), "exports/flowchart_person_years.pdf")

write.csv(
  flow_counts_py,
  "exports/flowchart_person_year_counts.csv",
  row.names = FALSE
)
