# Script to plot map of BC lighthouse station locations

library(tidyverse)
library(ggOceanMaps)
library(ggspatial)

# load the lat/lon for the 12 stations
BC_locations <- read.table("BC_Lightstations.csv", header = TRUE, sep = ",", col.names = c("Lightstation", "Latitude", "Longitude"))


basemap(limits = c(-133, -121, 48, 55), bathy.style = "rcb", rotate = TRUE) +
  ggspatial::geom_spatial_point(data = BC_locations, aes(x = Longitude, y = Latitude), color = "red", size = 2) +
  ggspatial::geom_spatial_label(data = BC_locations, aes(x = Longitude+c(-1.5,1.5,1.1,1.1,1,1.5,-1.25,1.5,1.5,-1.25,1.15,1.15), y = Latitude+c(0.1,0.1,0.2,0.15,0.15,-0.15,0.1,0.1,0.1,0.1,0.1,0.1), label = Lightstation), color = "black")
ggsave("BC_lightstation_map.jpg", width = 7, height = 6, units = "in", scale = 1.1, dpi = 1200)

