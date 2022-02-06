# query 2k network https://www.ncei.noaa.gov/pub/data/paleo/pages2k/pages2k-temperature-v2-2017/

# wget -r -A.lpd --no-directories https://www.ncei.noaa.gov/pub/data/paleo/pages2k/pages2k-temperature-v2-2017/data-version-2.0.0/ files


library(lipdR)
library(tidyverse)

# GLOBAL TEMPERATURE RECONSTRUCTIONS
glob_temp <- read.csv('2k-network/recons/Full_ensemble_median_and 95pct_range.txt')

# BRING IN HYDRO AND TEMP RECONSTRUCTIONS for last 12 centuries
# file:///tmp/mozilla_alex0/Northern_hemisphere_hydroclima.PDF
# https://www.ncei.noaa.gov/access/paleo-search/study/19725

hydro_df <- readxl::read_xls('2k-network/Source_Data_Extended_Data_Figure_5.xls')
temp_df <- readxl::read_xls('2k-network/Source_Data_Extended_Data_Figure_6.xls')



# THE STUFF BELOW IS RAW PROXY

#nam <- readLipd() 
#saveRDS(nam, 'nam.rds')

nam <- readRDS('2k-network/NAm/nam.rds') 

##### plot good tree data ####
select_tree_sites <- c('NAm-UpperWrightLakes.Graumlich.2005', 
                       'NAm-YellowMountainRidge.King.2002', 
                       'NAm-FrenchGlacier.Colenutt.1995', 
                       'NAm-Bennington.Luckman.2013', 
                       'NAm-SmallRiver.Luckman.2001', 
                       'NAm-Athabasca.Schweingruber.1996', 
                       'NAm-BellMountain.Schweingruber.1996')

select_lake_site <- c('NAm-HellsKitchenLake.Gajewski.1988', 'NAm-GreenLake.Menounos.2006')

tbl <- tibble(name = nam) %>% 
  unnest_wider(name) %>% 
  filter(dataSetName %in% select_tree_sites) 

init <- data.frame(
  name =  tbl$dataSetName[1],
  year = tbl$paleoData[[1]][[1]]$measurementTable[[1]]$year$values,
  #rw = dat[[1]]$measurementTable[[1]]$ringWidth$values,
  value = tbl$paleoData[[1]][[1]]$measurementTable[[1]]$trsgi$values)

for (i in 1:length(select_tree_sites)-1) {
  
    bind <- data.frame(
      name =  tbl$dataSetName[i+1],
      year = tbl$paleoData[[i+1]][[1]]$measurementTable[[1]]$year$values,
      #rw = dat[[1]]$measurementTable[[1]]$ringWidth$values,
      value = tbl$paleoData[[i+1]][[1]]$measurementTable[[1]]$trsgi$values) 
    
    init <- rbind(init, bind)
}

init <- init %>% mutate(
  proxy = "tree-ring"
)

init %>% 
  ggplot(aes(x = year, y = value, colour = name, group = name)) + 
  geom_line()

plotly::ggplotly()

#### plot lake data ####

select_lake_site <- c('NAm-HellsKitchenLake.Gajewski.1988', 'NAm-GreenLake.Menounos.2006')

tbl <- tibble(name = nam) %>% 
  unnest_wider(name) %>% 
  filter(dataSetName %in% select_lake_site) 

greenLake <- data.frame(
  name =  tbl$dataSetName[1],
  year = tbl$paleoData[[1]][[1]]$measurementTable[[1]]$year$values,
  #rw = dat[[1]]$measurementTable[[1]]$ringWidth$values,
  value = tbl$paleoData[[1]][[1]]$measurementTable[[1]]$thickness$values,
  proxy = 'varve-thickness')

hellsLake <- data.frame(
  name =  tbl$dataSetName[2],
  year = tbl$paleoData[[2]][[1]]$measurementTable[[1]]$year$values,
  #rw = dat[[1]]$measurementTable[[1]]$ringWidth$values,
  value = tbl$paleoData[[2]][[1]]$measurementTable[[1]]$temperature$values,
  proxy = 'pollen')

lakes <- rbind(greenLake, hellsLake)

all <- rbind(init, lakes)

all %>% 
  ggplot(aes(x = year, y = value, colour = name, group = name)) + 
  # geom_hline(yintercept = 0) +
 # geom_smooth(method = "lm", formula = y ~ 1, colour = "black", se=F, linetype="dashed") +
  facet_wrap(~proxy, nrow = 3, scale = 'free_y')+
  geom_line()

plotly::ggplotly()
