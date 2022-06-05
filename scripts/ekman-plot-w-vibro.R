# this script cleans up the ekman varve thicknesses so we can plot with the long core
# Selected E13 as it is the longest and matches up well with V2
# Could also add E11 and V1
# Option to calculate ekman stdep using Vibro core stats or ekman, probably makes more sense to use 

library(tidyverse)

vc_stats <- readRDS('data/long_cores/core_stats.rds')

v2_mean <- vc_stats$value[vc_stats$stat == 'mean_no_flood' & vc_stats$core == 'V2']
v2_sd <- vc_stats$value[vc_stats$stat == 'sd_no_flood' & vc_stats$core == 'V2']
ek <- read_csv('data/ekman/EK_varveCounting_orig_long_analysis.csv') |> 
  group_by(core_num) |> 
  mutate(
          year_CE = Year + 1, # miscalculated year originally top layer should be 2017 as that when we cored.. 
          mean_thickness_mm = mean(layer_thickness_mm, na.rm = T),
         sd_thickness_mm = sd(layer_thickness_mm, na.rm = T), 
         stdep_mm_e = (layer_thickness_mm - mean_thickness_mm)/sd_thickness_mm,
         stdep_mm = (layer_thickness_mm - v2_mean)/v2_sd,
         ma_30 = zoo::rollapply(stdep_mm, width = 10, by = 1, FUN = mean, na.rm = T, align = "center", partial = T, fill = NA), # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
  ) |> 
  filter(core_num %in% c('EK13', 'EK11')) |> 
  ungroup() |> 
  select(year_CE, lyr_mm = layer_thickness_mm, stdep_mm, core_num)

ek

ggplot(ek, aes(year_CE, stdep_mm, colour = core_num)) +
  geom_line() 
plotly::ggplotly()

vc <- readRDS('data/long_cores/varve_thickness_v1_v2_working.RDS') |> 
  filter(core == 'V2') |> 
  select(core_num = core, year_CE, lyr_mm = lyr_mm_cln, stdep_mm = lyr_mm_stdep_fltr)

all <- rbind(ek, vc) |> 
  mutate(core_num = gsub('K', '', core_num)) |> 
  rename(`Core Name` = core_num) 

ggplot(all, aes(year_CE, lyr_mm, colour = `Core Name`)) +
  geom_line()
plotly::ggplotly()

ggplot(all |> filter(year_CE > 1800), aes(year_CE, stdep_mm, colour = `Core Name`)) +
  geom_line() +
  ylab('Laminae Thickness (mm)') +
  xlab('Year (CE)')

ggsave('figs/long_core/V2_E13_varve_thickness_stdep.png', width = 8, height = 3)

ggplot(all |> filter(year_CE > 1800), aes(year_CE, lyr_mm, colour = `Core Name`)) +
  geom_line()  +
  ylab('Laminae Thickness (mm)') +
  xlab('Year (CE)')

ggsave('figs/long_core/V2_E13_varve_thickness.png', width = 8, height = 3)


