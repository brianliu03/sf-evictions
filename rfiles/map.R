library(osmdata)
library(tidyverse)
library(sf)
library(data.table)
library(viridis)
library(mapview)

# download OpenStreetMap data for San Francisco
sf_map <- opq("San Francisco") %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

source("rfiles/algo.R")

# read in eviction data
eviction_data <- read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
eviction_data <- as_tibble(data.frame(eviction_data))

# restructure eviction data
eviction_data <-
  filter_eviction_types_small(na.omit(restructure(eviction_data)))

neighborhood_shapefile <-
  st_read("neighborhood_shapes/planning_neighborhoods.shp")

# # filter to only fault or no fault
# eviction_data <- filter_fault_no_fault(eviction_data)

eviction_sf <- st_as_sf(
  eviction_data, coords = c("longitude", "latitude"), crs = 4326)

palette <- viridis(4, option = "viridis")

# Define eviction types and assign colors
eviction_types <- unique(eviction_sf$eviction_category)
eviction_type_colors <- setNames(palette, eviction_types)

neighborhood_sf <- st_as_sf(neighborhood_shapefile)

# Plot maps for each year
for (year in unique(year(eviction_sf$File.Date))) {
  evictions_year <- eviction_sf %>% filter(year(File.Date) == year) %>%
    mutate(eviction_category = factor(eviction_category, levels = eviction_types))
  sf_map_filtered <- sf_map$osm_lines %>%
    st_transform(crs = st_crs(evictions_year))

  # Create the map
  p <- ggplot() +
    geom_sf(data = sf_map_filtered, color = "darkgrey", lwd = 0.1, fill = NA) +
    geom_sf(data = evictions_year, aes(color = eviction_category), size = 1.0) +
    geom_sf(data = neighborhood_sf, color = "black", lwd = 0.4, fill = NA) +
    scale_color_manual(name = "Eviction Type", values = eviction_type_colors, drop = FALSE) +
    coord_sf(xlim = c(-122.5247, -122.3366), ylim = c(37.6983, 37.8312)) +
    labs(title = paste("Evictions in San Francisco - Year", year),
         caption = "Map Source: OpenStreetMap; Neighborhood lines and Evictions: DataSF") +
    guides(color = guide_legend(override.aes = list(size = 2.5)))

  # Save the map as an image
  ggsave(filename = paste(
    "map_neighb_compr", year, ".png", sep = ""), plot = p, width = 10, height = 8)
}

# turbo_palette <- viridis(10, option = "viridis")

# # plot map with two types, fault and no fault
# for (year in unique(year(eviction_sf$File.Date))) {
#   evictions_year <- eviction_sf %>% filter(year(File.Date) == year)
#   sf_map_filtered <- sf_map$osm_lines %>%
#     st_transform(crs = st_crs(evictions_year))

#   p <- ggplot() +
#     geom_sf(data = sf_map_filtered, color = "grey", size = 0.2) +
#     geom_sf(data = evictions_year, aes(color = Fault), size = 1.0) +
#     scale_color_manual(values = turbo_palette, labels = c("No Fault", "Fault")) +
#     coord_sf(xlim = c(-122.5247, -122.3366), ylim = c(37.6983, 37.8312)) +
#     labs(title = paste("Evictions in San Francisco - Year", year),
#          caption = "Map Source: OpenStreetMap") +
#     theme_light() +
#     guides(color = guide_legend(override.aes = list(size = 2.5)))

#   # Save the map as an image
#   ggsave(filename = paste(
#     "evictions_map_", year, ".png", sep = ""), plot = p, width = 10, height = 8)
# }

## palette for easier readability if using all eviction types
# custom_palette <- c(
#   "dodgerblue2", # AD
#   "#E31A1C", # Br
#   "palegreen2", # CI
#   "skyblue2", # CC
#   "#FF7F00", # De
#   "orchid1", # Dv
#   "#6A3D9A", # EAW
#   "#FB9A99", # FTSR
#   "red", # GSE
#   "#CAB2D6", # IU
#   "#FDBF6F", # LP
#   "khaki2", # LR
#   "deeppink1", # NP
#   "green4", # Nu
#   "gold1", # OC
#   "blue1", # OMI
#   "darkturquoise", # RSU
#   "green1", # SR
#   "brown" # UA
# )