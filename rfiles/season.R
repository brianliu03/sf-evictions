library(tidyverse)
library(viridis)

source("rfiles/algo.R")

eviction_data <- read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
eviction_data <- as_tibble(data.frame(eviction_data))

eviction_data <- get_counts_by_month_year(na.omit(restructure(eviction_data)))

# Assuming you have the eviction_counts data frame as described

# Define the color palette from viridis
my_palette <- viridis_pal()(length(unique(eviction_data$Year)))


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
  scale_color_manual(values = my_palette, name = "Year")


# Calculate the total number of evictions for each year
total_evictions_by_year <- eviction_data %>%
  group_by(Year) %>%
  summarize(Total_Evictions = sum(Eviction_Count))

# Calculate the proportion of evictions for each month in each year
eviction_proportions <- eviction_data %>%
  left_join(total_evictions_by_year, by = "Year") %>%
  mutate(Proportion = Eviction_Count / Total_Evictions)

# Create a ggplot to visualize the proportions, facet by month
ggplot(eviction_proportions, aes(x = factor(Year), y = Proportion, fill = as.factor(Year))) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend
  labs(
    title = "Proportion of Evictions by Year (Faceted by Month)",
    x = "Year",
    y = "Proportion of Evictions"
  ) +
  facet_wrap(~Month_Name, ncol = 3) +  # Facet by month
  scale_fill_viridis(discrete = TRUE) +  # Use viridis color palette
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    legend.position = "none"  # Remove legend
  )