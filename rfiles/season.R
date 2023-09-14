library(tidyverse)

source("rfiles/algo.R")

eviction_data <- read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
eviction_data <- as_tibble(data.frame(eviction_data))

eviction_data <- get_counts_by_month_year(na.omit(restructure(eviction_data)))


ggplot(eviction_data, aes(x = factor(Month_Name, levels = month.name), y = Eviction_Count, color = as.factor(Year))) +
  geom_point(size = 3) +  # Scatterplot points
  labs(
    title = "Eviction Counts by Month",
    x = "Month",
    y = "Eviction Count",
    color = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_color_discrete(name = "Year")