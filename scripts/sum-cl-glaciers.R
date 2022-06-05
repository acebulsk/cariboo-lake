# calculate the glaciated area of the cariboo lake watershed from the glims database

library(sf)
library(tidyverse)
library(tmap)

ws_area <- 3242 # km2

bsn <- read_sf('data/gis/boundaries/cariboo_lake_basin.gpkg')

## glacier data from
# Raup, B.H.; A. Racoviteanu; S.J.S. Khalsa; C. Helm; R. Armstrong; Y.
# Arnaud (2007).  "The GLIMS Geospatial Glacier Database: a New Tool for
#    Studying Glacier Change".  Global and Planetary Change 56:101--110.
# (doi:10.1016/j.gloplacha.2006.07.018)

# still has some castle creek glacier which is outside of the catchmetn so need to filter 
gl <- read_sf('data/gis/glims_download_82381/glims_polygons_cl_clip.shp') |> 
  st_intersection(bsn) |> 
  filter(glac_id != 'G239539E53028N') |> 
  mutate(area_new = st_area(.))

area <- sum(st_area(gl)) / 1e6

paste('The total glaciated area of the Cariboo Lake basin is', area, 'km^2^')
paste('The percent glaciated area of the Cariboo Lake basin is', (area / ws_area)*100, '%')

tm_shape(gl)+
  tm_polygons() +
  tm_shape(bsn) +
  tm_borders()

# what is the area of the rogers peak glacier less filtered than the cariboo 

rogers_pk_gl_ids <- c('G239447E52985N', 
                      'G239451E52965N',
                      'G239170E52990N', 
                      'G239265E52904N', 
                      'G239248E52906N',
                      'G239220E52909N', 
                      'G239173E52908N', 
                      'G239165E52907N')

rpg <- gl |> filter(glac_id %in% rogers_pk_gl_ids)

area <- sum(st_area(rpg)) / 1e6

paste('The total glaciated area of the Matthew River basin is', area, 'km^2^')
paste('The percent glaciated area of the Matthew River basin is', (area / ws_area)*100, '%')

# find analysis id for proper citation

analysis_ids <- gl$anlys_id |> unique()

# For Analysis_IDs in the range 374780--392326, the appropriate citation is
# 
# Bolch, Tobias (submitter); Bolch, Tobias (analyst(s)), 2008.
# GLIMS Glacier Database. Boulder, CO.
# National Snow and Ice Data Center.  http://dx.doi.org/10.7265/N5V98602




