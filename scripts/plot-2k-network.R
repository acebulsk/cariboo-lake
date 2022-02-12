# some climate reconstructions https://pastglobalchanges.org/science/data/databases

# query 2k network https://www.ncei.noaa.gov/pub/data/paleo/pages2k/pages2k-temperature-v2-2017/

# wget -r -A.lpd --no-directories https://www.ncei.noaa.gov/pub/data/paleo/pages2k/pages2k-temperature-v2-2017/data-version-2.0.0/ files


library(lipdR)
library(tidyverse)
library(sf)
library(tmap)
library(gridExtra)

tmap_mode("view")

mid_rescaler <- function(mid = 0) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}

# Trouet, et al temperature reconstruction for North America aka A 1500-year reconstruction of annual mean temperature for temperate North America on decadal-to-multidecadal time scales
# North America Last 2ka Decadal Temperature Reconstructions 
# A 1500-year reconstruction of annual mean temperature for temperate North America on decadal-to-multidecadal time scales

trouet <- read.delim('https://www.ncei.noaa.gov/pub/data/paleo/pages2k/trouet2013nam480pollen.txt', 
                     comment.char = '#', skip = 1) %>% 
  mutate(annom = temp2m.ann - median(temp2m.ann, na.rm = T))
# save locally in case link goes offline
# saveRDS(trouet, '2k-network/Trouet_et_al/NAM480.rds')

tr <- ggplot(trouet, aes(x = age_AD, y = annom)) +
  geom_smooth(aes(y = 0), method = "lm", formula = y ~ 1, colour = "black", se=F, linetype="dashed", size = .5)+
  geom_line() +
  geom_ribbon(aes(ymin = temp2m.ann.1SE.1 - median(temp2m.ann, na.rm = T), 
                  ymax = temp2m.ann.1SE - median(temp2m.ann, na.rm = T), 
                  fill = '1SE',), 
              alpha = 0.2)+
  geom_ribbon(aes(ymin = temp2m.ann.2SE.1 - median(temp2m.ann, na.rm = T), 
                  ymax = temp2m.ann.2SE - median(temp2m.ann, na.rm = T), 
                  fill = '2SE'), 
              alpha = 0.2) +
  scale_fill_manual(values = c("#08519C", "#9ECAE1"))+
  xlim(c(0, 2000)) +
  ylab('Air Temp. Anomaly °C') +
  theme_bw() +
  theme(legend.position = 'right',
        axis.title.x = element_blank(),
        legend.title = element_blank())
tr

# GLOBAL TEMPERATURE RECONSTRUCTIONS full 2k
# https://www.ncei.noaa.gov/access/paleo-search/study/26872
# manually calculated anomalies to match hard coded anomalies in the below procies 
nms <- c('year', 'inst_data', 'median', 'perc_2.5', 'perc_97.5', 'avg_31_yr_raw', 'avg_31_yr_median', 'avg_31_yr_2.5', 'avg_31_yr_97.5')

# ensemble_means from their R script this is what they use to make Fig. 1 in pub
# fullens.bp.30.200 is what we want (not relative to instrument data)

load('2k-network/2k-recon/recons_filtered.RData')

experiment.names<-c("CPS","PCR","M08","PAI","OIE","BHM","DA")

fort <- zoo::fortify.zoo(fullens.bp.30.200)

ensemble_long <- data.frame()
for(i in rev(seq_along(experiment.names))){
  ts <- fullens.bp.30.200[,i]
  df <- zoo::fortify.zoo(ts, names = 'year_ce')
  df$model <- experiment.names[i]
  
  ensemble_long <- rbind(ensemble_long, df)
}

ensemble_long %>% ggplot(aes(x = year_ce, y = ts, group = model, colour = model)) + geom_line()

# checked and is same as '2k-network/2k-recon/Fig_1.pdf'

ensemble_med <- ensemble_long %>% 
  group_by(year_ce) %>% 
  summarise(ts = median(ts),
            model = 'ensemble')

ggplot(ensemble_long, aes(x = year_ce, y = ts, group = model, colour = model)) + 
  geom_line() +
  geom_line(data = ensemble_med, color = "black")

glob_temp <- ensemble_med
glob_temp$grp <- c(rep(seq(0,1950, 50), each = 50)) + 25
glob_median <- median(glob_temp$ts, na.rm = T)
glob_median_abs <- abs(glob_median)
glob_std <- sd(glob_temp$ts, na.rm = T)

glob_temp_50 <- glob_temp %>% 
  group_by(grp) %>% 
  summarise(med = median(ts)) %>% 
  mutate(anomaly = med - glob_median,
         stdep = (med - glob_median)/ glob_std) %>% 
  filter(grp < 2000)

glob_temp_50_p <- glob_temp_50 %>% 
  ggplot(aes(x=grp, y = 'Temperature Anomaly (°C)', fill = anomaly)) +
  geom_tile() +
  #scale_fill_distiller(palette = "BrBG", direction = 1, rescaler = mid_rescaler()) +
  scale_fill_distiller(palette = "RdBu", direction = -1, rescaler = mid_rescaler()) +
  # scale_x_reverse() +
  xlab('Year (CE)') +
  theme_bw() +
  theme(legend.position = 'right',
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.3, 'cm')) +
  xlim(-50, 2000)

glob_temp_50_p

ggsave('figs/2k-network/global_anomalies_2k.jpg', width = 8, height = 1.5)

# BRING IN HYDRO AND TEMP RECONSTRUCTIONS for last 12 centuries
# Fredrik Charpentier Ljungqvist, Paul J. Krusic, Hanna S. Sundqvist, Eduardo Zorita, Gudrun Brattström, and David Frank. 2016. Northern Hemisphere hydroclimatic variability over the past twelve centuries. Nature, 532(7597), 94-98. doi: 10.1038/nature17418 
# file:///tmp/mozilla_alex0/Northern_hemisphere_hydroclima.PDF
# https://www.ncei.noaa.gov/access/paleo-search/study/19725
# Source_Data_Extended_Data_Figure_6.xls: Centennial temperature proxy anomalies updated from ref. 15.
# Source_Data_Extended_Data_Figure_5.xls: Simulated median values of annual precipitation from six atmosphere–ocean coupled general circulation models.
# ANOMOLIES time series value - median

map_df <- readxl::read_xls('2k-network/hydro-recons/Source_Data_Extended_Data_Figure_1.xls')
hydro_df <- readxl::read_xls('2k-network/hydro-recons/Source_Data_Extended_Data_Figure_5.xls')
temp_df <- readxl::read_xls('2k-network/hydro-recons/Source_Data_Extended_Data_Figure_6.xls')

hydro_sf <- hydro_df %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)
tm_shape(hydro_sf) +
  tm_dots()

temp_sf <- temp_df %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)
tm_shape(temp_sf) +
  tm_dots()

#### hydro plots ####
# find grid cell closest to cariboo

needle_lt <- 53
needle_ln <- -121

match <- data.frame(Longitude = c(-117.15 , -116.45),
                    Label = c("Sask. Glacier", "Sask. River"))

blanks <- data.frame(name = rep(NA, 9),
                     value = rep(NA, 9),
                     Year = seq(50, 850, 100),
                     Label = rep('Hydroclimate Anomaly (mm)',9))

cariboo_hydro <- hydro_df %>% 
  mutate(across(everything(), as.numeric),
         Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude),
         long_diff = abs(Longitude - needle_ln),
         lat_diff = abs(Latitude - needle_lt),
         diff_sum = long_diff + lat_diff) %>% 
  filter(diff_sum < 5.7) %>% 
  select(-contains('diff')) %>% 
  summarise(across(`800s`:`1900s`, mean)) %>% 
  pivot_longer(`800s`:`1900s`) %>% 
  mutate(Year = as.numeric(gsub("s", "", name)) + 50, # make it mid point 
         value = as.numeric(value),
         Label = 'Hydroclimate Anomaly (mm)') %>% 
  filter(value != -999.000) %>% 
  rbind(blanks)


cariboo_hydro_median <- median(cariboo_hydro$value, na.rm = T)

cariboo_hydro <- cariboo_hydro %>% 
    mutate(value = value - cariboo_hydro_median)

cariboo_hydro %>% 
  ggplot(aes(x=Year, y = Label, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", direction = 1, rescaler = mid_rescaler()) +
  scale_x_reverse() +
  xlab('Year (CE)') +
  theme(legend.position = 'right',
        axis.title.y = element_blank())
  
ggsave('figs/2k-network/hydro_anomalies_2grids_12centuries.jpg', width = 8, height = 1.5)
  
#### temp plot ####
needle_lt <- 53
needle_ln <- -121

match <- data.frame(Longitude = c(-117.15 , -116.45),
                    Label = c("Sask. Glacier", "Sask. River"))

blanks <- data.frame(name = rep(NA, 9),
                     value = rep(NA, 9),
                     Year = seq(50, 850, 100),
                     Label = rep('Temperature Anomaly (°C)',9))

cariboo_temp <- temp_df %>% 
  mutate(across(everything(), as.numeric),
         Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude),
         long_diff = abs(Longitude - needle_ln),
         lat_diff = abs(Latitude - needle_lt),
         diff_sum = long_diff + lat_diff) %>% 
  filter(diff_sum < 5) %>% 
  select(-contains('diff')) %>% 
  summarise(across(`800s`:`1900s`, mean)) %>% 
  pivot_longer(`800s`:`1900s`) %>% 
  mutate(Year = as.numeric(gsub("s", "", name)) + 50, # make it mid point 
         value = as.numeric(value),
         Label = 'Temperature Anomaly (°C)') %>% 
  filter(value != -999.000) %>% 
  rbind(blanks)



cariboo_temp %>% 
  ggplot(aes(x=Year, y = Label, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", direction = 1, rescaler = mid_rescaler()) +
  scale_x_reverse() +
  xlab('Year (CE)') +
  theme(legend.position = 'right',
        axis.title.y = element_blank())


ggsave('figs/2k-network/temp_anomalies_4grids_12centuries.jpg', width = 8, height = 1.5)


#### all ####
all <- rbind(cariboo_hydro, cariboo_temp) %>% 
  ggplot(aes(x=Year, y = Label, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", direction = -1, rescaler = mid_rescaler(), na.value = 'transparent') +
  #scale_fill_distiller(palette = "BrBG", direction = 1, rescaler = mid_rescaler(), na.value = 'transparent') +
  # scale_x_reverse() +
  xlab('Year (CE)') +
  theme_bw() +
  theme(legend.position = 'right',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.3, 'cm')) +
  xlim(-50, 2000)

all
ggsave('figs/2k-network/both_anomalies_12centuries.jpg', width = 8, height = 2)

#### combine 2k and 12 cent ####

p <- list(all, tr, glob_temp_50_p)
cowplot::plot_grid(plotlist = p, nrow=3, align = 'v', labels = c("A", "B"))

#### plot cariboo long core on climate proxy ####

# gain size 

gs_v1 <- readRDS('figs/grain_size_v1.rds')
gs_v2 <- readRDS('figs/grain_size_v2.rds')

gs <- readRDS('data/Sediment/Grain Size/grain_size_v1_v2_combined.RDS') %>% 
  mutate(year_ce_avg = round(year_ce_new))

gs_plot <- 
  gs %>% 
  mutate(
    smooth = smoother::smth(x = stdep, method = 'gaussian', window = 6),
    mvavg = zoo::rollapply(stdep, 3, mean, align = 'center', fill = NA)
  ) %>% 
  ggplot(aes(x = year_ce_avg, color = core)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 1, colour = "black", se=F, linetype="dashed", size = .5) +
  geom_point(aes(y = stdep), alpha = 1) +
  #geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE) +
  geom_line(aes(y = mvavg), colour = "gray") +
  ylab("D50 Std. Dept.") +
  # xlab("Year (CE)") +  
  theme_bw()+
  theme(legend.position = 'right',
        axis.title.x = element_blank(),
        legend.title = element_blank())

# varve thickness 

vt <- readRDS('data/long_cores/varve_thickness_v1_v2_working.RDS')

vt_plot <- 
  vt %>% 
  mutate(
    mvavg = zoo::rollapply(lyr_mm_stdep_fltr, width = 100, by = 1, FUN = mean, na.rm = T, align = "center", partial = T), # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
  ) %>% 
  ggplot(aes(x = year_ce_lin_interp, colour = core)) +
  geom_smooth(aes(y = lyr_mm_stdep_fltr), method = "lm", formula = y ~ 1, colour = "black", se=F, linetype="dashed", size = .5)+
  geom_line(aes(y = lyr_mm_stdep_fltr), alpha = 1/4) +
  #geom_line(aes(y = smooth)) +
  geom_line(aes(y = ma_30)) +
  ylab("VT Std. Dept.") +
  ylim(c(-2.5, 5))+
  theme_bw()+
  theme(legend.position = 'right',
        axis.title.x = element_blank(),
        legend.title = element_blank())
vt_plot

plotly::ggplotly()

# LOI

loi <- readRDS('data/Sediment/LOI/loi_v1_v2_working.RDS')

loi_plot <- 
  loi %>% 
  mutate(
    #  smooth = smoother::smth(x = stdep, method = 'gaussian', window = 3),
    mvavg = zoo::rollapply(stdep, 3, mean, align = 'center', fill = NA)
  ) %>% 
  ggplot(aes(x = year_ce_new, colour = core)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 1, colour = "black", se=F, linetype="dashed", size = .5) +
  geom_point(aes(y = stdep), alpha = 1) +
  #geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE) +
  geom_line(aes(y = mvavg), colour = "gray") +
  ylab("LOI Std. Dept.") +
  theme_bw() +
  theme(legend.position = 'right',
        axis.title.x = element_blank(),
        legend.title = element_blank())


p <- list(vt_plot, gs_plot, loi_plot, tr, all, glob_temp_50_p)
cp <- cowplot::plot_grid(plotlist = p, nrow=6, labels = c("A", "B", "C", "D", "E", "F"), align = 'v', rel_heights = c(2,2,2,1.25,1.25))

cp
cowplot::save_plot('figs/2k-network/all_core_stats_2k_anomalies.jpg', cp, base_width = 7, base_height = 8)

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
