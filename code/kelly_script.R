library(ggplot2)
library(sf)

p <- ggplot() +
  geom_sf(data = read_sf("~/Desktop/lab2/data/ME-GIS/Coastline2.shp"), 
          colour="grey10", fill="grey90") +
  geom_sf(data = read_sf("~/Desktop/lab2/data/ME-GIS/Rivers19.shp"), 
          colour="steelblue", size=0.3) +
  geom_sf(data = read_sf("~/Desktop/lab2/data/ME-GIS/PrimaryRoads.shp"), 
          size = 0.7, colour="grey30") +
  geom_sf(data = read_sf("~/Desktop/lab2/data/ME-GIS/Cities.shp")) +
  theme_bw()

p
#get labels for the cities

#see geom_sf_text








