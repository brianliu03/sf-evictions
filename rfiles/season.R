library(tidyverse)
library(viridis)

source("rfiles/algo.R")

eviction_data <- read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
eviction_data <- as_tibble(data.frame(eviction_data))

eviction_data <- restructure(eviction_data)
# eviction_data <- filter_no_2023(eviction_data)
eviction_data <- get_counts_by_month_year(eviction_data)

# Assuming you have the eviction_counts data frame as described

# Define the color palette from viridis
my_palette <- viridis_pal()(length(unique(eviction_data$Year)))


ggplot(eviction_data, aes(x = factor(Month_Name, levels = month.name), y = Eviction_Count, group = as.factor(Year), color = as.factor(Year))) +
  geom_line(size = 1) +  # Lines connecting the data points for each year
  labs(
    title = "Eviction Counts by Month",
    x = "Month",
    y = "Eviction Count",
    color = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_color_manual(values = my_palette, name = "Year", guide = "none")

ggplot(eviction_data, aes(x = factor(Month_Name, levels = month.name), y = Eviction_Count, group = as.factor(Year), color = as.factor(Year))) +
  geom_line(size = 1) +  # Lines connecting the data points for each year
  labs(
    title = "Eviction Counts by Month",
    x = "Month",
    y = "Eviction Count",
    color = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_color_manual(values = my_palette, name = "Year", guide = "none") +  # Use your custom color palette and hide the legend
  facet_wrap(~Year, ncol = 3)  # Create separate line plots for each year (3 columns in this example)

# same plot as above but separate by year
for (year in unique(eviction_data$Year)) {
  p <- ggplot(eviction_data %>% filter(Year == year), aes(x = factor(Month_Name, levels = month.name), y = Eviction_Count)) +
    geom_line(size = 1) +  # Lines connecting the data points for each year
    labs(
      title = paste("Eviction Counts by Month - Year", year),
      x = "Month",
      y = "Eviction Count"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

    ggsave(filename = paste(
    "counts_by_month_", year, ".png", sep = ""), plot = p, width = 10, height = 8)
}



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
  facet_wrap(~factor(Month_Name, levels = month.name), ncol = 3) +  # Facet by month
  scale_fill_viridis(discrete = TRUE) +  # Use viridis color palette
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    legend.position = "none"  # Remove legend
  )