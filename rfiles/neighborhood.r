library(tidyverse)
library(viridis)
library(plotly)

source("rfiles/algo.R")

eviction_data <- read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
eviction_data <- as_tibble(data.frame(eviction_data))

eviction_data <- restructure(eviction_data)
eviction_data <- filter_no_2023(eviction_data)
eviction_data <- filter_eviction_types_small(eviction_data)
eviction_data <- group_by_neighborhood_types(eviction_data)

# also remove File.Date
eviction_data <- eviction_data %>%
  mutate(Year = lubridate::year(File.Date),
         Month = lubridate::month(File.Date),
         Month_Name = month.name[lubridate::month(File.Date)])

eviction_data_month <- eviction_data %>%
  group_by(Year, Month, Month_Name, Neighborhood) %>%
  summarise(Eviction_Count = n())

eviction_data_year <- eviction_data %>%
  group_by(Year, Neighborhood) %>%
  summarise(Eviction_Count = n())

# Create a dodged bar plot for neighborhood trends
ggplot(eviction_data, aes(x = factor(Year), y = Eviction_Count, fill = Neighborhood)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(
    title = "Eviction Counts by Neighborhood and Year",
    x = "Year",
    y = "Eviction Count",
    fill = "Neighborhood"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = TRUE)  # Use viridis color palette

# Create a ggplot to overlay neighborhood trends by year
p <- ggplot(eviction_data_year, aes(x = factor(Year), y = Eviction_Count, group = Neighborhood, color = Neighborhood)) +
  geom_line(size = 1) +  # Lines connecting the data points for each neighborhood
  labs(
    title = "Eviction Counts by Neighborhood and Year",
    x = "Year",
    y = "Eviction Count",
    color = "Neighborhood"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_color_viridis(discrete = TRUE) + # Use viridis color palette
  guides(color = "none")
