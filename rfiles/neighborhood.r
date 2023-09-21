library(tidyverse)
library(viridis)

source("rfiles/algo.R")

eviction_data <- read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
eviction_data <- as_tibble(data.frame(eviction_data))

eviction_data <- restructure(eviction_data)
eviction_data <- filter_no_2023(eviction_data)
eviction_data <- filter_eviction_types_small(eviction_data)
eviction_data <- group_by_neighborhood(eviction_data)

# also remove File.Date
eviction_data <- eviction_data %>%
  mutate(Year = lubridate::year(File.Date),
         Month = lubridate::month(File.Date),
         Month_Name = month.name[lubridate::month(File.Date)])

eviction_data <- eviction_data %>%
  group_by(Year, Month, Month_Name, Neighborhood) %>%
  summarise(Eviction_Count = n())

# save to csv
write.csv(eviction_data, "eviction_data.csv", row.names = FALSE)