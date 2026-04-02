# convert survey-design subsets to data frames with weights and plot with ggplot2
library(ggplot2)
library(dplyr)
library(scales)

design_2019 <- subset(meps_design_final, year == "2019")
design_2021 <- subset(meps_design_final, year == "2021")

design_rx_2019 <- subset(design_2019, adhd_any_rx == "Yes")
design_rx_2021 <- subset(design_2021, adhd_any_rx == "Yes")

build_df <- function(design, yr) {
  data.frame(
    year = yr,
    adhd_rx_n = design$variables$adhd_rx_n,
    adhd_total_spend = design$variables$adhd_total_spend,
    adhd_oop = design$variables$adhd_oop,
    wt = weights(design)
  )
}

df <- bind_rows(
  build_df(design_2019, "2019"),
  build_df(design_2021, "2021"),
  build_df(design_rx_2019, "2019"),
  build_df(design_rx_2021, "2021")
)

cols <- c("2019" = rgb(1, 0, 0, 0.5), "2021" = rgb(0, 0, 1, 0.5))

p_rx <- ggplot(df, aes(x = adhd_rx_n, weight = wt, fill = year)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = max(10, length(unique(na.omit(df$adhd_rx_n))))) +
  scale_y_continuous(labels = scales::label_number(scale = 1/10000)) +
  scale_fill_manual(values = cols) +
  labs(title = "Distribution of ADHD Rx Count in 2019 vs 2021", x = "Number of ADHD Rx", y = "Weighted count (10000s)")

p_spend <- ggplot(df, aes(x = adhd_total_spend, weight = wt, fill = year)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 60) +
  scale_y_continuous(labels = scales::label_number(scale = 1/10000)) +
  scale_fill_manual(values = cols) +
  labs(title = "Distribution of Total ADHD Rx Spend in 2019 vs 2021", x = "Total ADHD Rx Spend (2021 dollars)", y = "Weighted count (10000s)")

p_oop <- ggplot(df, aes(x = adhd_oop, weight = wt, fill = year)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 60) +
  scale_y_continuous(labels = scales::label_number(scale = 1/10000)) +
  scale_fill_manual(values = cols) +
  labs(title = "Distribution of Out-of-Pocket ADHD Rx Spend in 2019 vs 2021", x = "Out-of-Pocket ADHD Rx Spend (2021 dollars)", y = "Weighted count (10000s)")

p_adhd_rx <- ggplot(df, aes(x = adhd_rx_n, weight = wt, fill = year)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = max(10, length(unique(na.omit(df$adhd_rx_n))))) +
  scale_y_continuous(labels = scales::label_number(scale = 1/10000)) +
  scale_fill_manual(values = cols) +
  labs(title = "Distribution of ADHD Rx Count in 2019 vs 2021, ADHD with Rx only", x = "Number of ADHD Rx", y = "Weighted count (10000s)")

p_adhd_spend <- ggplot(df, aes(x = adhd_total_spend, weight = wt, fill = year)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 60) +
  scale_y_continuous(labels = scales::label_number(scale = 1/10000)) +
  scale_fill_manual(values = cols) +
  labs(title = "Distribution of Total ADHD Rx Spend in 2019 vs 2021, ADHD with Rx only", x = "Total ADHD Rx Spend (2021 dollars)", y = "Weighted count (10000s)")

p_adhd_oop <- ggplot(df, aes(x = adhd_oop, weight = wt, fill = year)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 60) +
  scale_y_continuous(labels = scales::label_number(scale = 1/10000)) +
  scale_fill_manual(values = cols) +
  labs(title = "Distribution of Out-of-Pocket ADHD Rx Spend in 2019 vs 2021, ADHD with Rx only", x = "Out-of-Pocket ADHD Rx Spend (2021 dollars)", y = "Weighted count (10000s)")

list(p_rx = p_rx, p_spend = p_spend, p_oop = p_oop, p_adhd_rx = p_adhd_rx, p_adhd_spend = p_adhd_spend, p_adhd_oop = p_adhd_oop)