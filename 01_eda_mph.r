# Exploratory Data Analysis
# Goal: To understand the structure of the data, 
# identify patterns, and detect any anomalies or outliers.
# MPH-specific: only looking at pre-post two years, 
# no longitudinal analyses

# load 2019 data
load("data/meps_2019.rda")

# convert haven labels to factors in df fyc 
# remove preceding numbers from factor levels
fyc <- fyc %>%
  mutate(across(where(is.labelled), as_factor)) %>%
  mutate(across(where(is.factor), ~ gsub("^[0-9]+ ", "", .)))

# dollars: convert to 2021 dollars using CPI
cpi_2019  <- 256.974
cpi_2021  <- 278.802
cpi_ratio <- cpi_2021 / cpi_2019

fyc$income_2021 <- fyc$TTLP * cpi_ratio
fyc$totslf_2021 <- fyc$TOTSLF * cpi_ratio

# pcs/mcs: if not numeric, NA
fyc$VPCS42 <- as.numeric(fyc$VPCS42)
fyc$VMCS42 <- as.numeric(fyc$VMCS42)

# eda
summary(fyc$income_2021)
table(fyc$SEX)
table(fyc$RACETHX)
table(fyc$MARRY53X)
table(fyc$REGION53)
table(fyc$EDUCYR)
table(fyc$POVCAT)
summary(fyc$VPCS42)
summary(fyc$VMCS42)
table(fyc$INSCOV)
table(fyc$INSURC19)
table(fyc$MCAID53X)

# recode race/ethnicity
fyc$ethnicity <- NA
fyc$ethcnitiy <- ifelse(fyc$RACETHX == "HISPANIC", "Hispanic", "Non-Hispanic")
fyc$race      <- NA
fyc$race      <- ifelse(fyc$RACETHX == "NON-HISPANIC ASIAN ONLY", "Asian",
                 ifelse(fyc$RACETHX == "NON-HISPANIC BLACK ONLY", "Black",
                 ifelse(fyc$RACETHX == "NON-HISPANIC OTHER RACE OR MULTIPLE RACE", "Other",
                 ifelse(fyc$RACETHX == "NON-HISPANIC WHITE ONLY", "White", 
                 NA))))

# recode marital status
fyc$marital <- NA
fyc$marital <- ifelse(fyc$MARRY53X %in% c("-1 INAPPLICABLE", "-7 REFUSED", "-8 DK"), NA,
               ifelse(fyc$MARRY53X %in% c("MARRIED", "MARRIED IN ROUND"), "Married", 
               "Not married"))

# fix NA for region
fyc$REGION53 <- ifelse(fyc$REGION53 == "-1 INAPPLICABLE", NA, fyc$REGION53)

# recode education
fyc$education <- NA
fyc$education <- ifelse(fyc$EDUCYR %in% c("-1 INAPPLICABLE", "-7 REFUSED", "-8 DK", "-15 CANNOT BE COMPUTED"), NA,
                 ifelse(fyc$EDUCYR == "NO SCHOOL/KINDERGARTEN ONLY", "Less than high school",
                 ifelse(fyc$EDUCYR %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"), "Less than high school",
                 ifelse(fyc$EDUCYR == "GRADE 12", "High school graduation or greater",
                 ifelse(fyc$EDUCYR %in% c("1 YEAR COLLEGE", "2 YEARS COLLEGE", "3 YEARS COLLEGE", 
                                          "4 YEARS COLLEGE", "5+ YEARS COLLEGE"), "High school graduation or greater",
                 NA)))))

# recode insurance
fyc$insurance <- NA
fyc <- fyc %>%
  mutate(
    insurance = case_when(
      # 1) Uninsured (any age)
      INSURC19 %in% c("<65 UNINSURED", "65+ UNINSURED") ~ "Uninsured",
      
      # 2) Under 65, any private, no Medicaid
      INSURC19 == "<65 ANY PRIVATE" & MCAID53X != "YES" ~ "Private only",
      
      # 3) Under 65, any private, WITH Medicaid
      INSURC19 == "<65 ANY PRIVATE" & MCAID53X == "YES" ~ "Medicaid, with private",

      # 4) Under 65, public only
      INSURC19 == "<65 PUBLIC ONLY" & MCAID53X == "YES" ~ "Medicaid only",
      INSURC19 == "<65 PUBLIC ONLY" & MCAID53X != "YES" ~ "Other public",
      
      # 5) 65+, Medicare + private (includes Medigap/MA)
      INSURC19 == "65+ EDITED MEDICARE AND PRIVATE" & MCAID53X != "YES" ~ "Medicare, with private",
      
      # 6) 65+, Medicare + private + Medicaid (triple coverage)
      INSURC19 == "65+ EDITED MEDICARE AND PRIVATE" & MCAID53X == "YES" ~ "Medicare, dual-eligible",
      
      # 7) 65+, Medicare only
      INSURC19 == "65+ EDITED MEDICARE ONLY" & MCAID53X != "YES" ~ "Medicare only",
      
      # 8) 65+, Medicare only + Medicaid
      INSURC19 == "65+ EDITED MEDICARE ONLY" & MCAID53X == "YES" ~ "Medicare, dual-eligible",
      
      # 9) 65+, Medicare + other public (no private)
      INSURC19 == "65+ EDITED MEDICARE AND OTH PUB ONLY" & MCAID53X == "YES" ~ "Medicare, dual-eligible",
      INSURC19 == "65+ EDITED MEDICARE AND OTH PUB ONLY" & MCAID53X != "YES" ~ "Medicare, with other public",
      
      # 10) 65+, no Medicare but has public/private
      INSURC19 == "65+ NO MEDICARE AND ANY PUBLIC/PRIVATE" & MCAID53X == "YES" ~ "Medicaid, with private",
      INSURC19 == "65+ NO MEDICARE AND ANY PUBLIC/PRIVATE" & MCAID53X != "YES" ~ "Private only",
      
      # Fallback: use INSCOV as a coarse backup
      INSCOV == "UNINSURED" ~ "Uninsured",
      INSCOV == "ANY PRIVATE" ~ "Private only",
      INSCOV == "PUBLIC ONLY" & MCAID53X == "YES" ~ "Medicaid only",
      INSCOV == "PUBLIC ONLY" & MCAID53X != "YES" ~ "Other public",
      
      TRUE ~ NA
    )
  )

# Other dummies to account for insurance
# Medicaid only, Medicaid w private, other public
# Medicare (any), non-Medicare
# Private
# Uninsured

fyc$medicaid <- NA
fyc <- fyc %>% 
  mutate(
    medicaid = case_when(
      insurance == "Medicaid only" ~ "Medicaid only",
      insurance == "Medicaid, with private" ~ "Medicaid, with private, non-Medicare",
      TRUE ~ "No Medicaid"
    )
  )

fyc$medicare <- NA
fyc <- fyc %>% 
  mutate(
    medicare = case_when(
      insurance %in% c("Medicare, with private", "Medicare, dual-eligible", 
                       "Medicare, with other public", "Medicare only") ~ "Medicare, any",
      TRUE ~ "No Medicare"
    )
  )

fyc$private_ins <- ifelse(fyc$insurance == "Private only", "Private only", "Other/uninsured")

fyc$has_insurance <- ifelse(fyc$INSCOV == "UNINSURED", "No insurance", "Has insurance")