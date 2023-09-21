library(tidyverse)
library(viridis)

source("rfiles/algo.R")

eviction_data <- read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
eviction_data <- as_tibble(data.frame(eviction_data))

eviction_data <- restructure(eviction_data)
eviction_data <- filter_no_2023(eviction_data)
eviction_data <- filter_eviction_types_small(eviction_data)
eviction_data <- group_by_neighborhood(eviction_data)