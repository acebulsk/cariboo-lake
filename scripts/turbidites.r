library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(tidyr)

# core stats

core_stats <- readRDS('data/long_cores/core_stats.rds')

# select from original visual inspection circa 2018, excel notes (all have mode == 1)

v1_tb <- c(44, 124, 120, 196, 251) # rm 257 bc is duplicate of 251
v2_tb <- c(88, 92, 168, 172, 224, 228)

## carbon dates ##
# A small twig from V1 at 347 cm results in a date of 1899-1819 cal BP. 
# A 4 cm long twig from V2 at 222 cm results in a date of 490-316 cal BP. 
# Since the first date from V2 was much younger than than expected, 
# a second sample from V2 was analyzed by combining a small twig at 286 cm and pine needle at 294 cm. 
# A date of 2045-1895 cal BP was determined.

v1_date <- (1899 + 1819) / 2 # mid point yr for v1 @ 347 cm

v1_C14 <- data.frame(core = 'V1', ams_depth_cm = 347, ams_year = v1_date) # n = 1

v2b_depth <- (286 + 294) / 2 # avg depth for combined V2 sample

v2_date_b <- (2045 + 1895) / 2 # mid point yr for v2 @ 286 + 294 cm
v2_date_a <- (490 + 316) / 2 # mid point yr for v2 @ 222 cm

v2_C14 <- data.frame(core = 'V2', ams_depth_cm = 286, ams_year = v2_date_b) # n = 2

ams_df <- tibble(
  year = c(0, v1_date, 0, v2_date_a, 0, v2_date_b),
  depth = c(0, 347, 0, 222, 0, v2b_depth),
  ams_sample = c('V1', 'V1', 'V2a', 'V2a', 'V2b', 'V2b')
)

ams_select <- rbind(v1_C14, v2_C14)

ams_df <- tibble(
  year = c(0, v1_date, 0, v2_date_a, 0, v2_date_b),
  depth = c(0, 347, 0, 222, 0, v2b_depth),
  ams_sample = c('V1', 'V1', 'V2a', 'V2a', 'V2b', 'V2b')
)

# turbidites from varve thickness 

vt <- readRDS('data/long_cores/V1_V2_turbidite_deposits.rds') %>% 
  select(core_depth,
         year_ce_lin_interp,
         lyr_mm_stdep,
         lyr_mm,
         sd_flag,
         core) 

# add in turbidite not origionally measured because I thought it was distrubed
# the grain size for v1 measured at 12 cm is 7.5 mm thick, manually measured using 226 A+ .png
v1_12 <- data.frame(
  core_depth = 12,
  year_ce_lin_interp = NA,
  lyr_mm_stdep = NA,
  lyr_mm = 7.5, 
  sd_flag = TRUE,
  core = 'V1'
)

v1_12$year_ce_lin_interp = 2017 - (v1_12$core_depth * (ams_select$ams_year[1] / ams_select$ams_depth_cm[1]))
v1_12$core_depth <- v1_12$core_depth * 10
v1_vt_mean <- core_stats$value[core_stats$stat == 'mean_no_flood' & core_stats$core == 'V1' & core_stats$metric == 'varve_thickness']
v1_vt_sd <- core_stats$value[core_stats$stat == 'sd_no_flood' & core_stats$core == 'V1' & core_stats$metric == 'varve_thickness']
v1_12$lyr_mm_stdep = (v1_12$lyr_mm - v1_vt_mean)/v1_vt_sd

vt <- rbind(vt, v1_12) %>% 
  select(depth = core_depth,
         year = year_ce_lin_interp,
         stdep = lyr_mm_stdep,
         value = lyr_mm,
         core) %>% 
  mutate(metric = 'Varve Thickness')

# average varve thickness for turbidites

v1_tb_vt_avg <- mean(vt$value[vt$core == 'V1'])

v2_tb_vt_avg <- mean(vt$value[vt$core == 'V2'])


# Look at turbidites from grain-size analysis

gs <- readRDS('data/long_cores/gain_size_w_floods.rds') %>% 
  rename(record_number = `Record Number`, d50 = `Dx (50)`) %>% 
  mutate(
    turbidite = case_when(
      core == 'V1' & record_number %in% v1_tb ~ T,
      core == 'V2' & record_number %in% v2_tb ~ T
    )
  )

gs <- gs %>%  filter(core == 'V1' & record_number %in% v1_tb |
                     core == 'V2' & record_number %in% v2_tb) %>% 
  select(depth = core_depth, year = year_ce_new, stdep, value = d50, core) %>% 
  mutate(metric = 'Grain Size')

gs %>% 
  group_by(core) %>% 
  summarise(mean_d50 = mean(value),
            sd_d50 = sd(value),
            count = n())

gs_tb_new %>% 
  group_by(core) %>% 
  summarise(mean_d50 = mean(d50),
            sd_d50 = sd(d50),
            count = n())

tb_insp <- gs %>% 
  filter(core == 'V1' & record_number %in% v1_tb |
         core == 'V2' & record_number %in% v2_tb) %>% 
  distinct()

ggplot(tb_insp, aes(x = year_ce_new, y = d50, colour = core)) +geom_point()

turbs %>% 
  group_by(core) %>% 
  summarise(mean_d50 = mean(Dx..50.),
            sd_d50 = sd(Dx..50.)) 

no_turb <- gs %>% 
  filter(Mode.Count != 1)

no_turb %>% 
  group_by(core) %>% 
  summarise(mean_d50 = mean(Dx..50.),
            sd_d50 = sd(Dx..50.))

# LOI
loi <- readRDS('data/Sediment/LOI/loi_v1_v2_working.RDS')

tb_loi <- loi %>% 
  filter(turbidite == T) %>% 
  select(depth,
         year = year_ce_new,
         stdep,
         value = LOI,
         core) %>% 
  mutate(metric = 'LOI')


# plot gs and vt

# tb <- rbind(vt, gs, tb_loi)
# 
# saveRDS(tb, 'data/long_cores/turbidite_metrics.rds')

tb <- readRDS('data/long_cores/turbidite_metrics.rds')

p <- ggplot(tb, aes(x = year, y = stdep, colour = core)) +
  geom_point() +
  geom_bar(stat = 'identity') +
  facet_grid(rows = vars(metric), scales="free_y") +
  xlab('Year (CE)') +
  ylab('Standardized Departure')+
  theme_bw() 

saveRDS(p, 'figs/turbidite_plot.rds')

