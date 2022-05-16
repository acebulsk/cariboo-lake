# script to create the small scale main Cariboo Watershed map with insets

library(tidyverse)
library(tmap)
library(tmaptools)
# tmaptools::palette_explorer()
library(terra)
library(sf)

# cl_dem <- rast('data/gis/raster/DEM_StudyArea.img') |> 
#   project("epsg:3005")
# 
# terra::writeRaster(cl_dem, filename = 'data/gis/raster/DEM_StudyArea_bcalb.img')

cl_dem <- rast('data/gis/raster/DEM_StudyArea_bcalb.img')

bb <- st_bbox(cl_dem)

cl_watershed <- read_sf('data/gis/boundaries/cariboo_lake_basin.gpkg') |> 
  st_crop(bb)
cl_waterbodies <- read_sf('data/gis/boundaries/waterbodies.gpkg') |> 
  st_crop(bb)

cl_waterbodies_lbls <- cl_waterbodies |> 
  filter(feature_id %in% c(
    '579a1fe8540d477aa32bb75535f2d013',
    'c23a3ee54fe640f384a18c822d3c655f',
    'b5ff0613c28c47d68916ce95dd146f4d',
    '94a039c3a3864a54b8a61057311041aa' # matthew river
  ))

# glaciers <- read_sf('data/gis/glims_download_82381/glims_polygons.shp') |> st_transform(st_crs(3005))
# 
# cl_glaciers <- st_filter(glaciers, cl_watershed, .predicate = st_intersects) |> st_zm()
# 
# st_write(cl_glaciers, 'data/gis/glims_download_82381/glims_polygons_cl_clip.shp')
cl_glc <- read_sf('data/gis/glims_download_82381/glims_polygons_cl_clip.shp')

glc_hatched <- cl_glc |> 
  cartography::hatchedLayer(mode = "sfc", pattern = "right2left", density = 4) # create hashed overlay

strm_gauges <- read_csv('data/gis/StreamGageStations.csv') |> 
  select(id, ddlt, ddln) |> 
  mutate(ddln = ddln * -1) |> 
  filter(id == '08KH003') |> 
  st_as_sf(coords = c("ddln", "ddlt"), crs = 4326) |> 
  st_transform(st_crs(3005))

# canada <- spData::world %>% dplyr::filter(name_long == "Canada")

tmap_mode('plot')
# tmap_mode('view')

sc_map <- tm_shape(cl_dem) +
  tm_raster(palette=get_brewer_pal("BrBG",plot = F), title = 'Elevation (m)')+
  tm_graticules(n.x = 3,
                n.y = 3) +
  tm_shape(cl_waterbodies) +
  tm_polygons(col = "#2c7bb6") +
  # tm_shape(cl_waterbodies_lbls)+
  # tm_text(text = "name_en") +
  tm_shape(strm_gauges)+
  tm_dots(shape = 24, size = 1) +
  tm_shape(cl_glc) +
  tm_borders(lwd = 1.5) +
  tm_shape(glc_hatched) +
  tm_lines(lwd = 1.5,   legend.show = TRUE) +
  tm_shape(cl_watershed) +
  tm_borders(col = "black",
             lty = "dotted",
             lwd = 2) +
  tm_layout(legend.frame = 'black') +
  tm_add_legend(type = 'symbol', 
                shape = 24,
               col = "black", 
               labels = c("Stream Gauge"))+
  tm_add_legend(type = 'line', 
                lty = 'dotted',
                col = "black", 
                labels = c("Watershed Boundary"))+
  tm_add_legend(type = 'fill', 
                col = 'white',
                labels = c("Glacier"))
sc_map


#### inset ####

bb_sc <- bb |> 
  st_as_sfc()

bg <- read_sf('data/gis/boundaries/lpr_000b16a_e.shp') %>% filter(
  PRENAME %in% c('British Columbia', 'Yukon', 'Alberta', 'Northwest Territories')
) |> st_transform(st_crs(3005))

# bbinset <- st_bbox(c(xmin = 800000, ymin = 100000, xmax = 1150000, ymax = 2000000), crs = st_crs(3005))
bbinset <- st_bbox(c(xmin = 300000, ymin = 400000, xmax = 1800000, ymax = 2000000), crs = st_crs(3005))

inset <- 
  tm_shape(bg, bbox = bbinset) +
  tm_polygons() +
  tm_graticules(n.x = 3,
                n.y = 3,
                labels.inside.frame = T) +
  tm_shape(bb_sc) +
  tm_borders(col = 'red')
inset

library(grid)

asp_inset <- (bbinset$xmax - bbinset$xmin)/(bbinset$ymax - bbinset$ymin)

w <- 0.25
h <- asp_inset * w
vp <- viewport(x=0.98, y=.06, width = w, height = h, just= c('right', 'bottom'))
tmap::tmap_save(sc_map, filename = 'figs/maps/cl_small_scale.png', width = 7, height = 7, insets_tm = inset, insets_vp = vp)

