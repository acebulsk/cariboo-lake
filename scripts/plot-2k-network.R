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

# GLOBAL TEMPERATURE RECONSTRUCTIONS
nms <- c('year', 'inst_data', 'median', 'perc_2.5', 'perc_97.5', 'avg_31_yr_raw', 'avg_31_yr_median', 'avg_31_yr_2.5', 'avg_31_yr_97.5')

glob_temp <- read.delim('2k-network/recons/Full_ensemble_median_and 95pct_range.txt', skip = 4, col.names = nms) %>% 
  mutate(grp = c(rep(seq(0,1950, 50), each = 50),  rbind(rep(2000, 17))))

glob_median <- median(glob_temp$median, na.rm = T)
glob_median_abs <- abs(glob_median)
glob_std <- sd(glob_temp$median, na.rm = T)

glob_temp_50 <- glob_temp %>% 
  group_by(grp) %>% 
  summarise(med = mean(median)) %>% 
  mutate(grp = grp + 25,
         anomaly = med + glob_median_abs,
         stdep = (med - glob_median)/ glob_std) %>% 
  filter(grp < 2000)

glob_temp_50_p <- glob_temp_50 %>% 
  ggplot(aes(x=grp, y = 'Temperature Anomaly', fill = stdep)) +
  geom_tile() +
  #scale_fill_distiller(palette = "BrBG", direction = 1, rescaler = mid_rescaler()) +
  scale_fill_distiller(palette = "RdBu", direction = -1, rescaler = mid_rescaler()) +
  # scale_x_reverse() +
  xlab('Year (CE)') +
  theme_bw() +
  theme(legend.position = 'right',
        axis.title.y = element_blank(),
        legend.title = element_blank()) +
  xlim(-50, 2000)
glob_temp_50_p
ggsave('figs/2k-network/global_anomalies_2k.jpg', width = 8, height = 1.5)



glob_temp %>% 
  ggplot(aes(year, median)) +
  geom_line()

# BRING IN HYDRO AND TEMP RECONSTRUCTIONS for last 12 centuries
# file:///tmp/mozilla_alex0/Northern_hemisphere_hydroclima.PDF
# https://www.ncei.noaa.gov/access/paleo-search/study/19725

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
                     Label = rep('Hydroclimate Anomaly',9))

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
         Label = 'Hydroclimate Anomaly') %>% 
  filter(value != -999.000) %>% 
  rbind(blanks)

cariboo_hydro %>% 
  ggplot(aes(x=Year, y = Label, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", direction = 1, rescaler = mid_rescaler()) +
  scale_x_reverse() +
  xlab('Year (CE)') +
  theme(legend.position = 'right',
        axis.title.y = element_blank())
  legent_position
  
ggsave('figs/2k-network/hydro_anomalies_2grids_12centuries.jpg', width = 8, height = 1.5)
  
#### temp plot ####
needle_lt <- 53
needle_ln <- -121

match <- data.frame(Longitude = c(-117.15 , -116.45),
                    Label = c("Sask. Glacier", "Sask. River"))

blanks <- data.frame(name = rep(NA, 9),
                     value = rep(NA, 9),
                     Year = seq(50, 850, 100),
                     Label = rep('Temperature Anomaly',9))

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
         Label = 'Temperature Anomaly') %>% 
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
        legend.title = element_blank()) +
  xlim(-50, 2000)

all
ggsave('figs/2k-network/both_anomalies_12centuries.jpg', width = 8, height = 2)

#### combine 2k and 12 cent ####

p <- list(all, glob_temp_50_p)
cowplot::plot_grid(plotlist = p, nrow=2, align = 'v', labels = c("A", "B"))

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


p <- list(vt_plot, gs_plot, loi_plot, all, glob_temp_50_p)
cp <- cowplot::plot_grid(plotlist = p, nrow=5, labels = c("A", "B", "C", "D", "E"), align = 'v', rel_heights = c(2,2,2,1.25,1.25))

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
