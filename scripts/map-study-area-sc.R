# script to create the small scale main Cariboo Watershed map with insets

library(tidyverse)
library(tmap)
library(terra)

cl_watershed <- read_sf('data/gis/boundaries/cariboo_lake_basin.gpkg')
cl_waterbodies <- read_sf('data/gis/boundaries/waterbodies.gpkg')
cl_dem <- rast('data/gis/raster/DEM_StudyArea.img') 

tmap_mode('plot')

tm_shape(cl_dem) +
  tm_raster()+
  tm_graticules(n.x = 3,
                n.y = 3) +
tm_shape(cl_watershed) +
  tm_borders(col = "black",
             lty = "dotted") +
  tm_graticules(n.x = 3,
                n.y = 3)+
  tm_shape(cl_waterbodies)+
  tm_polygons(col = "blue")
