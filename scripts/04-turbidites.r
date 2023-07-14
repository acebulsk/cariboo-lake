library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(tidyr)

v1_lm <- readRDS('data/long_cores/chronology/v1_c14_age_depth_model.rds')
v2_lm <- readRDS('data/long_cores/chronology/v2_c14_age_depth_model.rds')
# core stats

core_stats <- readRDS('data/long_cores/core_stats.rds')

# select from original visual inspection circa 2018, excel notes (all have mode == 1)

v1_tb <- c(44, 124, 120, 196, 251) # rm 257 bc is duplicate of 251
v2_tb <- c(88, 92, 168, 172, 224, 228)

# turbidites from varve thickness 

# calculate ams year so we can plot against the other metrics

vt <- readRDS('data/long_cores/V1_V2_turbidite_deposits.rds') %>%
  mutate(year_bp_ams = case_when(
           core == "V1" ~ predict(v1_lm, cur_data()),
           core == "V2" ~ predict(v2_lm, cur_data())
         ),
         year_ce_ams = standard_yr_bp-year_bp_ams) |> 
  select(depth, year = year_ce_ams, stdep, value, core, metric)

t_ids <- c('T1', 'T2', 'T3', 'T4', 'T5')

# vt_v1 <- vt |> 
#   filter(core == 'V1') |> 
#   arrange(year_bp) |> 
#   mutate(t_id = t_ids)
# 
# vt_v2 <- vt |> 
#   filter(core == 'V2') |> 
#   arrange(year_bp) |> 
#   mutate(t_id = t_ids)
# 
# vt_check <- vt |> 
#   select(depth, year_bp, core, value) |> 
#   pivot_wider(names_from = core, names_prefix = 'year_bp_', values_from = year_bp) |> 
#   arrange(depth)

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
  select(depth = core_depth, stdep, value = d50, core) %>% 
  mutate(metric = 'Grain Size',
         year_bp_ams = case_when(
           core == "V1" ~ predict(v1_lm, cur_data()),
           core == "V2" ~ predict(v2_lm, cur_data())
         ),
         year_ce_ams = standard_yr_bp-year_bp_ams) |> 
  select(depth, year = year_ce_ams, stdep, value, core, metric)

gs %>% 
  group_by(core) %>% 
  summarise(mean_d50 = mean(value),
            sd_d50 = sd(value),
            count = n())

# gs_tb_new %>% 
#   group_by(core) %>% 
#   summarise(mean_d50 = mean(d50),
#             sd_d50 = sd(d50),
#             count = n())

tb_insp <- gs %>%
  distinct()

ggplot(tb_insp, aes(x = year, y = value, colour = core)) +geom_point()

# LOI
loi <- readRDS('data/Sediment/LOI/loi_v1_v2_working.RDS')

tb_loi <- loi %>% 
  filter(turbidite == T) %>% 
  select(depth,
         stdep,
         value = LOI,
         core) %>% 
  mutate(metric = 'LOI', 
         year_bp_ams = case_when(
           core == "V1" ~ predict(v1_lm, cur_data()),
           core == "V2" ~ predict(v2_lm, cur_data())
         ),
         year_ce_ams = standard_yr_bp-year_bp_ams) |> 
  select(depth, year = year_ce_ams, stdep, value, core, metric)


# plot gs and vt

tb <- rbind(vt, gs, tb_loi)

saveRDS(tb, 'data/long_cores/turbidite_metrics.rds')

# tb <- readRDS('data/long_cores/turbidite_metrics.rds')

p <- ggplot(tb, aes(x = year, y = stdep, colour = core, fill = core)) +
  geom_point() +
  geom_bar(stat = 'identity') +
  facet_grid(rows = vars(metric), scales="free_y") +
  xlab('Year (CE)') +
  ylab('Standardized Departure')+
  theme_bw() +
  scale_color_manual(values = viridis::viridis(3)) +
  scale_fill_manual(values = viridis::viridis(3))
p
ggsave('journal-submission/markdown/figs/turbidite_plot.jpg', width = 7, height = 4)
saveRDS(p, 'figs/turbidite_plot.rds')

