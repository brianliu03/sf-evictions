library(osmdata)
library(tidyverse)
library(sf)
library(data.table)
library(viridis)

# download OpenStreetMap data for San Francisco
sf_map <- opq("San Francisco") %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

source("rfiles/algo.R")

# read in eviction data
eviction_data <- read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
eviction_data <- as_tibble(data.frame(eviction_data))

# restructure eviction data
eviction_data <- na.omit(restructure(eviction_data))

# filter to only fault or no fault
eviction_data <- filter_fault_no_fault(eviction_data)

eviction_sf <- st_as_sf(
  eviction_data, coords = c("longitude", "latitude"), crs = 4326)

# turbo_palette <- viridis(19, option = "turbo")

# # Define eviction types and assign colors
# eviction_types <- sort(unique(eviction_sf$eviction_type))
# eviction_type_colors <- setNames(turbo_palette, eviction_types)

# # Plot maps for each year
# for (year in unique(year(eviction_sf$File.Date))) {
#   evictions_year <- eviction_sf %>% filter(year(File.Date) == year)
#   sf_map_filtered <- sf_map$osm_lines %>%
#     st_transform(crs = st_crs(evictions_year))

#   # Create the map
#   p <- ggplot() +
#     geom_sf(data = sf_map_filtered, color = "grey", size = 0.2, fill = NA) +
#     geom_sf(data = evictions_year, aes(color = eviction_type), size = 1.0) +
#     scale_color_manual(values = eviction_type_colors) +
#     coord_sf(xlim = c(-122.5247, -122.3366), ylim = c(37.6983, 37.8312)) +
#     labs(title = paste("Evictions in San Francisco - Year", year),
#          caption = "Map Source: OpenStreetMap") +
#     theme_minimal() +
#     guides(color = guide_legend(override.aes = list(size = 2.5)))

#   # Save the map as an image
#   ggsave(filename = paste(
#     "evictions_map_", year, ".png", sep = ""), plot = p, width = 10, height = 8)
# }

# just want 4th index and 9th index
turbo_palette <- viridis(10, option = "viridis")[c(6, 2)]

# plot map with two types, fault and no fault
for (year in unique(year(eviction_sf$File.Date))) {
  evictions_year <- eviction_sf %>% filter(year(File.Date) == year)
  sf_map_filtered <- sf_map$osm_lines %>%
    st_transform(crs = st_crs(evictions_year))

  p <- ggplot() +
    geom_sf(data = sf_map_filtered, color = "grey", size = 0.2) +
    geom_sf(data = evictions_year, aes(color = Fault), size = 1.0) +
    scale_color_manual(values = turbo_palette, labels = c("No Fault", "Fault")) +
    coord_sf(xlim = c(-122.5247, -122.3366), ylim = c(37.6983, 37.8312)) +
    labs(title = paste("Evictions in San Francisco - Year", year),
         caption = "Map Source: OpenStreetMap") +
    theme_light() +
    guides(color = guide_legend(override.aes = list(size = 2.5)))

  # Save the map as an image
  ggsave(filename = paste(
    "evictions_map_", year, ".png", sep = ""), plot = p, width = 10, height = 8)
}