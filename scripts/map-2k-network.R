#### mapping 2k- NA points ####

library(tmap)
data("World")
tmap_mode('view')

nam <- readRDS('2k-network/NAm/nam.rds') 

archiveType <- map_dfc(nam, 'archiveType', .id = 'dataSetName') %>% 
  pivot_longer(everything(), names_to = 'dataSetName', values_to = 'archiveType')

minYr <- map_dfc(nam, 'minYear', .id = 'dataSetName') %>% 
  pivot_longer(everything(), names_to = 'dataSetName', values_to = 'minYear')

geo_df <- map_dfr(nam, 'geo', .id = 'dataSetName') %>% 
  left_join(archiveType) %>% 
  left_join(minYr) %>% 
  filter(minYear > -1000)

geo_sf <- sf::st_as_sf(geo_df, coords = c('longitude', 'latitude'), crs = 4326) 

tm_shape(geo_sf) +
  tm_dots(col = 'minYear') 