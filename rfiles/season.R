library(tidyverse)
library(viridis)

source("rfiles/algo.R")

eviction_data <- read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
eviction_data <- as_tibble(data.frame(eviction_data))

eviction_data <- restructure(eviction_data)
eviction_data <- filter_no_2023(eviction_data)
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



# Calculate the total number of evictions for each month
# Add mean eviction count for each month
# Add median eviction count for each month
total_evictions_by_month <- eviction_data %>%
  group_by(Month, Month_Name) %>%
  summarize(Total_Evictions = sum(Eviction_Count),
            Mean_Evictions = mean(Eviction_Count),
            Median_Evictions = median(Eviction_Count))

# Calculate the proportion of counts for each month over expected
# value and median value
eviction_proportions <- eviction_data %>%
  select(-Month_Name) %>%
  left_join(total_evictions_by_month, by = "Month") %>%
  mutate(Proportion_Mean = Eviction_Count / Mean_Evictions,
         Proportion_Median = Eviction_Count / Median_Evictions)

# Apply base-2 logarithm transformation to Proportion_Mean and Proportion_Median
eviction_proportions_transformed <- eviction_proportions %>%
  mutate(
    Transformed_Proportion_Mean = log(Proportion_Mean, base = 2),
    Transformed_Proportion_Median = log(Proportion_Median, base = 2)
  )

# Create a ggplot to visualize the proportions by month
ggplot(eviction_proportions_transformed, aes(x = factor(Month_Name, levels = month.name), y = Transformed_Proportion_Mean, group = as.factor(Year), color = as.factor(Year))) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 0.5, color = "black") +
  labs(
    title = "Proportion: Evictions Counts per Month over Mean Evictions by Month",
    x = "Month",  # Remove x-axis label
    y = "Proportion of Evictions (log_2)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +  # Remove x-axis text labels
  scale_color_manual(values = my_palette, name = "Month", guide = "none") +
  facet_wrap(~Year, ncol = 13)  # Create separate line plots for each year (13 columns in this example)

# Create a ggplot to visualize the proportions by month (median)
ggplot(eviction_proportions_transformed, aes(x = factor(Month_Name, levels = month.name), y = Transformed_Proportion_Median, group = as.factor(Year), color = as.factor(Year))) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 0.5, color = "black") +
  labs(
    title = "Proportion: Evictions Counts per Month over Median Evictions by Month",
    x = "Month",  # Remove x-axis label
    y = "Proportion of Evictions (log_2)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +  # Remove x-axis text labels
  scale_color_manual(values = my_palette, name = "Month", guide = "none") +
  facet_wrap(~Year, ncol = 13)  # Create separate line plots for each year (13 columns in this example)