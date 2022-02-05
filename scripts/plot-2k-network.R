# query 2k network https://www.ncei.noaa.gov/pub/data/paleo/pages2k/pages2k-temperature-v2-2017/

# wget -r -A.lpd --no-directories https://www.ncei.noaa.gov/pub/data/paleo/pages2k/pages2k-temperature-v2-2017/data-version-2.0.0/ files


library(lipdR)
library(tidyverse)

l

#nam <- readLipd() 
#saveRDS(nam, 'nam.rds')

nam <- readRDS('nam.rds') 

tst <- nam$`NAm-AlmondButterLower.D'Arrigo.2005`

df <- as.data.frame(do.call(rbind, nam))

fltr <- filterTs(nam, "archiveType == lake sediment")

df2 <- as.data.frame(do.call(rbind, fltr))


