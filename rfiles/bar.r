library(tidyverse)
library(viridis)

source("rfiles/algo.R")

eviction_data <- read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
eviction_data <- as_tibble(data.frame(eviction_data))

eviction_data <- filter_no_2023(restructure(eviction_data))

# eviction_data <- get_counts_by_year(eviction_data)
eviction_data <- get_counts_by_year_preserve_type(eviction_data)

eviction_data <- filter_eviction_types_small(eviction_data)

# Define the color palette from viridis
my_palette <- viridis_pal()(length(unique(eviction_data$eviction_category)))

custom_order2 <- c("Other", "Financial", "Fault", "Property_Change")

eviction_data$eviction_category <- factor(
  eviction_data$eviction_category,
  levels = custom_order2,
  ordered = TRUE
)

# Create the stacked bar plot with viridis color palette
ggplot(eviction_data, aes(x = factor(Year), y = Eviction_Count, fill = eviction_category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(
    title = "Eviction Counts by Year and Type",
    x = "Year",
    y = "Eviction Count",
    fill = "Eviction Type"
  ) +
  scale_fill_manual(
    values = my_palette,
    labels = c("Other", "Financial", "Fault", "Property Change"),
    name = "Eviction Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels (optional)