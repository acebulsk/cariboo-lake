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
  st_transform(st_crs(rast(dem)))

# CSHShydRology::ch_wbt_catchment_onestep(
#   wd = wd,
#   in_dem = dem,
#   pp_sf = pp_sf,
#   sink_method = "fill",
#   threshold = 1,
#   snap_dist = 0.01
# )

# create a catchment polygon from the raster poly

fn_r_catch <- 'data/gis/cshs-gis-working/catchment_100_th/catchment.tif'
fn_v_catch_bound <- 'data/gis/cshs-gis-working/catchment_100_th/catchment_boundary.shp'

whitebox::wbt_raster_to_vector_polygons(fn_r_catch, fn_v_catch_bound)

# output to kml for google earth checking

sf_catch_bound <- sf::read_sf(fn_v_catch_bound)

st_write(sf_catch_bound, 'data/gis/cshs-gis-working/catchment_100_th/catchment_boundary.kml')

# create a kml for our old boundary 

sf_catch_bound <- sf::read_sf('data/gis/boundaries/cariboo_lake_basin.gpkg')

st_write(sf_catch_bound, 'data/gis/boundaries/cariboo_watersurvey_basin.kml')
