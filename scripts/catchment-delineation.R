# script to delineate catchment above cariboo lake

library(CSHShydRology)
library(sf)
library(terra)

wd <- 'data/gis/cshs-gis-working/'

# download DEMs from BC gov
# wget -r -np -nH --cut-dirs=3 -R index.html https://pub.data.gov.bc.ca/datasets/175624/93a/
# wget -r -np -nH --cut-dirs=3 -R index.html https://pub.data.gov.bc.ca/datasets/175624/93h/

# merge dems
# dem_file_names <- list.files('data/gis/dem', full.names = T)
# 
# dems <- lapply(dem_file_names, rast)
# 
# dems_rc <- sprc(dems)
# 
# m <- merge(dems_rc)
# 
# terra::writeRaster(m, 'data/gis/cshs-gis-working/093a_093h_merge.tif')

# crop to study area
# bb <- sf::st_read('data/gis/boundaries/cariboo_lake_basin.gpkg') |> 
#   st_transform(st_crs(m))
# 
# c <- crop(m, ext(bb) + 0.1)
# 
# terra::writeRaster(c, 'data/gis/cshs-gis-working/093a_093h_merge_crop.tif')

# start the catchment delineation 

dem <- 'data/gis/cshs-gis-working/093a_093h_merge_crop.tif'

pp <- data.frame(lon = -121.39546,lat = 52.75203) # this is above keithley creek 

pp_sf <- st_as_sf(pp, coords = c('lon', 'lat'), crs = 4326) |> 
  st_transform(st_crs(c))

CSHShydRology::ch_wbt_catchment_onestep(
  wd = wd,
  in_dem = dem,
  pp_sf = pp_sf,
  sink_method = "fill",
  threshold = 1,
  snap_dist = 0.01
)

# convert raster catchment to polygon for area calculation and boundary plotting

r_catch <- rast('data/gis/cshs-gis-working/catchment_100_th/catchment.tif')

@todo convert to vector and re calc the area and redo percent glaciated.. etc 