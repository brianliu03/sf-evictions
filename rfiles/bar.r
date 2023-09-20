library(tidyverse)
library(viridis)

source("rfiles/algo.R")

eviction_data <- read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
eviction_data <- as_tibble(data.frame(eviction_data))

eviction_data <- filter_no_2023(restructure(eviction_data))

eviction_data <- get_counts_by_year(eviction_data)

# Define the color palette from viridis
my_palette <- viridis_pal()(length(unique(eviction_data$Year)))

ggplot(eviction_data, aes(x = factor(Year), y = Eviction_Count, fill = factor(Year))) +
  geom_bar(stat = "identity") +  # Create a bar plot
  labs(
    title = "Eviction Counts by Year",
    x = "Year",
    y = "Eviction Count",
    fill = "Year"
  ) +
  scale_fill_manual(values = my_palette, guide = "none") +  # Use your custom color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability