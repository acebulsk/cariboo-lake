# calculate lake area to watershed ratio for cariboo lake and quesnel lake

library(sf)
library(tidyverse)
library(tmap)

bsn <- read_sf('data/gis/boundaries/cariboo_lake_basin.gpkg')
wb <- sf::read_sf('data/gis/boundaries/waterbodies.gpkg')
wb$area_new <- st_area(wb)

cl_fid <- c('579a1fe8540d477aa32bb75535f2d013', 
            '0a951a71d45544ca917baab3c1945858')

ql_fid <- c('883bc2f3654a43c5a9285e2ae98791a9', 
            '3941e38da4264a2d87d1a03db59ecc50', 
            'be66fca3bde24ba8b8c10635268afc32', 
            'cee6fa88e92041fbbaa4c118549638b2', 
            'b5ff0613c28c47d68916ce95dd146f4d', 
            '624b9cbed400433a861348ebc74c8f69', 
            'cb11eeeeb9f8443f9ef6a7bdf87e6671', 
            'e4e888f8895248d6aa3a5c74cee33f33', 
            '9ab95a7510154235b6924f271e993458', 
            'd8efdfd7e410497eaa8378b839ae8e2d')

cl_area <- wb |> filter(feature_id %in% cl_fid)
ql_area <- wb |> filter(feature_id %in% ql_fid)


tm_shape(bsn)+
  tm_polygons() +
  tm_shape(cl_area) +
  tm_borders(col = 'blue') 

tm_shape(ql_area) +
  tm_borders(col = 'red')

cl_a <- sum(cl_area$area_new)/1000000 |> as.numeric()

cb_a <- bsn$area / 1000000 |> as.numeric()

ql <- sum(ql_area$area_new)/1000000 |> as.numeric()
qb <- 5845 # from giblert2012
bsn$area <- st_area(bsn)

paste('The area of the Cariboo Lake basin is', sum(cl_area$area_new)/1000000, 'km^2^')

paste('The watershed to lake ratio for the Cariboo Basin is:', (cl_a / cb_a)*100, '%')

paste('The total area of the Quesnel Lake basin is', ql, 'km^2^')

paste('The watershed ratio is', (ql / qb)*100, '%')
