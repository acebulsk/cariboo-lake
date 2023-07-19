# a script that pulls data from the WSC hydat database for stations important in this project

library(tidyhydat)
library(tidyverse)
library(sf)
library(plyr)
library(weathercan)
library(ggthemes)

# create a hydrograph for cariboo river at the forks 1971-2000 to correspond to climate normals --------------------

# this is the gauge shown on the map 
gauge_id <- '08KH003'

# choose what set of gauge data to use i.e. for 1981-2010 compare or 1990-2020 compare
which_year_max <- 2020
which_year_min <- 1971

stn_reg <- hy_stn_regulation() |> 
  dplyr::rename(reg_year_from = Year_from,
                reg_year_to = Year_to)

stn_range <- hy_stn_data_range() 

stn_meta_all <- hy_stations() |> 
  left_join(stn_reg) |> 
  left_join(stn_range) 

stn_meta_fltr <- stn_meta_all |> 
  left_join(stn_reg) |> 
  left_join(stn_range) |> 
  filter(
    STATION_NUMBER %in% gauge_id,
    # REGULATED == F, #this removed the englishman and zeballos which still have 10 yrs flow dat before reg. 
    Year_from <= which_year_max - 10, # stations starting after which_year - 10 will not have 10 yrs within range
    RECORD_LENGTH >= 10, # need at least 10 yrs will constrain within 1981-2010 later  
    DATA_TYPE == "Q",
    DRAINAGE_AREA_GROSS > 0,
    is.na(DRAINAGE_AREA_GROSS)==F,
    is.na(STATION_NUMBER)==F,
    is.na(REGULATED) == F,
    REGULATED == F | (REGULATED == T & reg_year_from >= which_year_min + 10)) # keep stations that were regulated after 1990. Might have 10 years of data prior to regulation. Need to clip individually later by each stns reg from date. 

# grab monthly flow normals but still need to filter by at least 10 yrs data
monthly_normals <- ldply(stn_meta_fltr$STATION_NUMBER, function(x) hy_monthly_flows(station_number = x)) 

# apply more filters

normal_fltr <- monthly_normals |> 
  filter(
    Year >= which_year_min,
    Year <= which_year_max,
    Full_Month == T,
    Sum_stat == "MEAN"
  ) 

month_count <- normal_fltr |> 
  group_by(STATION_NUMBER) |> 
  tally() |> 
  filter(n >= 120) # need at least 10 years of data

# its okay if most years have all months but notokay if all years are missing months 
month_in_yr_count <- normal_fltr |> 
  filter(STATION_NUMBER %in% month_count$STATION_NUMBER) |> 
  dplyr::group_by(STATION_NUMBER, Year) |> 
  tally() |> 
  filter(n == 12)

# show start and end year for each station
start_end_meta <- normal_fltr |> 
  dplyr::group_by(STATION_NUMBER) |> 
  dplyr::summarise(
    start_year = min(Year),
    end_year = max(Year),
    record_length = (end_year - start_year) +1)

flows_out <- normal_fltr |> 
  filter(Year %in% month_in_yr_count$Year)

start_end_meta <- flows_out |> 
  dplyr::group_by(STATION_NUMBER) |> 
  dplyr::summarise(
    start_year = min(Year),
    end_year = max(Year),
    record_length = (end_year - start_year) +1)

ggplot(flows_out, aes(Month, Value, group = Year, colour = factor(Year))) +
  #geom_point() +
  # geom_jitter()+
  geom_line() +
  ylab(expression(paste("Discharge"~(m^3~s^{-1})))) +
  facet_wrap(~STATION_NUMBER, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE), ncol = 2, nrow = 6)  

# convert to mm 

mdays = data.frame(month = c(1:12), mdays = c(31,28.25,31,30,31,30,31,31,30,31,30,31))

area <- stn_meta_fltr |> 
  select(STATION_NUMBER, DRAINAGE_AREA_GROSS) |> 
  mutate(area_m2 = DRAINAGE_AREA_GROSS * 1000000)


record_length <- flows_out |> 
  dplyr::group_by(STATION_NUMBER) |> 
  dplyr::summarise(
    start_year = min(Year),
    end_year = max(Year),
    record_length = (end_year - start_year) +1
  ) 

month_dict <- data.frame(
  Month = 1:12,
  month_abb = month.abb
)

flowdepth_unreg <- flows_out |> 
  dplyr::left_join(area) |> 
  dplyr::mutate(runoff = Value * 1/area_m2 *60*60*24*No_days *1000) |> 
  dplyr::group_by(STATION_NUMBER, Month) |> 
  dplyr::summarise(
    area_km2 = first(DRAINAGE_AREA_GROSS),
    mean = mean(runoff),
    # min = min(runoff),
    # max = max(runoff),
    median = median(runoff),
    upper_quantile = quantile(runoff,0.95),
    lower_quantile = quantile(runoff, 0.05),
    Q = mean(Value)
  ) |> 
  left_join(record_length) |> 
  left_join(stn_meta_fltr) |> 
  tidyr::pivot_longer(mean:median) |> 
  left_join(month_dict)

flowdepth_unreg$month_abb <- factor(flowdepth_unreg$month_abb, levels = c(month.abb[10:12], month.abb[1:9]))

hg <- ggplot(flowdepth_unreg, aes(x = month_abb, y= value, group = name)) +
  #geom_point() +
  # geom_jitter()+
  geom_ribbon(aes(ymin=lower_quantile, ymax=upper_quantile, fill = '5th to 95th\npercentile'), alpha=0.5) +
  geom_line(aes(colour = name)) +
  ylab("Runoff (mm)") +
  # xlab("Month") +
  xlab(NULL) +
  theme_bw() +
  scale_color_colorblind() +
  scale_fill_manual(values = c('5th to 95th\npercentile' = 'grey')) +
  theme(legend.title = element_blank())

hg

saveRDS(hg, 'figs/climate/08KH003_1971_1994_runoff_normals.rds')

ggsave('figs/climate/08KH003_1971_1994_runoff_normals.png', width = 5, height = 4)

# likely bc climate data ------------------------------

stn_meta <- stations_search("likely", normals_years = "1971-2000")

lil <- weather_dl(stn_meta$station_id, start = '1900-01-01', end = '2024-01-02')

likely_normals <- normals_dl(stn_meta$climate_id, normals_years = "1971-2000")$normals[[1]] |> 
  filter(period != 'Year')

likely_normals$period <- factor(likely_normals$period, levels = c(month.abb[10:12], month.abb[1:9]))

likely_normals_avg_tmps <- likely_normals |> 
  select(period, 
         mean = temp_daily_average, 
         max = temp_daily_max,
         min = temp_daily_min) |> 
  tidyr::pivot_longer(mean:min)

t <- ggplot(likely_normals_avg_tmps, aes(x = period, y= value, group = name, colour = name)) +
  #geom_point() +
  # geom_jitter()+
  geom_line() +
  ylab("Temperature (°C)") +
  # xlab("Month") +
  xlab(NULL) +
  theme_bw() +
  scale_color_colorblind() +
  theme(legend.title = element_blank())

t

saveRDS(t, 'figs/climate/likely_1094616_1971_2000_climate_normals_temp.rds')

ggsave('figs/climate/likely_1094616_1971_2000_climate_normals_temp.png', width = 5, height = 3)

likely_normals_avg_pcp <- likely_normals |> 
  select(
    period,
    precip,
    rain,
    snow) |> 
  tidyr::pivot_longer(precip:snow) |> 
  filter(name != 'precip')

p <- ggplot(likely_normals_avg_pcp, aes(x = period, y= value, fill = name)) +
  geom_bar(position="stack", stat="identity")  +
  ylab("Precipitation (mm)") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_fill_viridis_d(option = "E") +
  xlab(NULL)

p

saveRDS(p, 'figs/climate/likely_1094616_1971_2000_climate_normals_snow.rds')

ggsave('figs/climate/likely_1094616_1971_2000_climate_normals_snow.png', width = 5, height = 3)

plist <- list(hg, t, p)

right_side <- cowplot::plot_grid(t, p, nrow = 2, labels = c('B', 'C'), label_size = 12)

cp <- cowplot::plot_grid(hg, right_side, labels = c('A', ''), label_size = 12, ncol = 2, nrow = 1,rel_widths = c(.52,.48))

cp

ggsave('figs/climate/cariboo_combine_climate_hydro.png', width = 9, height = 4)
ggsave('journal-submission/markdown/figs/cariboo_combine_climate_hydro.png', width = 9, height = 4)

