# Goal
# 
# Purchases and expenditures by generic drug name (RXDRGNAM)
#  - Number of people with purchase
#  - Total purchases
#  - Total expenditures

# Set survey option for lonely psu
options(survey.lonely.psu="adjust")

# Get data set

rx2016 <- read_MEPS(year = 2016, type = "RX")

# Aggregate to person-level

rx_personlevel <- rx2016 %>%
  group_by(DUPERSID, VARSTR, VARPSU, RXDRGNAM) %>%
  summarise(
    PERWT16F = mean(PERWT16F),
    pers_RXXP = sum(RXXP16X),
    n_purchases = n()) %>%
      ungroup %>%
      mutate(persons = 1)

# Define survey design and calculate estimates

rx_design <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT16F,
  data = rx_personlevel,
  nest = TRUE
  )

totals <- svyby(~persons + n_purchases + pers_RXXP,
                by = ~RXDRGNAM, FUN = svytotal, design = rx_design)

totals %>% select(persons, se.persons)         # Number of people with purchase
totals %>% select(n_purchases, se.n_purchases) # Total purchases
totals %>% select(pers_RXXP, se.pers_RXXP)     # Total expenditures

# this works!