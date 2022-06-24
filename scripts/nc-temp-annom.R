# look at Northern Hemisphere temperature anomalies from instrumental data 

file <- '2k-network/Moberg_et_al/CRUTEM.5.0.1.0.anomalies.nc'
library(tidync)
tidync(file)

tidync(file) |> activate()

df <- tidync(file) |> 
  hyper_filter(latitude = latitude > 0) |> 
  hyper_tibble()
mean(df$tem)
library(ncdf4)

nc <- ncdf4::nc_open(file)

print(nc)

lon <- ncvar_get(nc, 'lon') 
nlon <- dim(lon)
lat <- ncvar_get(nc, 'lat') 
nlat <- dim(lat)
time <- ncvar_get(nc, 'time')