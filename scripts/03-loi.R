library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(tidyr)

#### age depth model and c14 meta ####

v1_lm <- readRDS('data/long_cores/chronology/v1_c14_age_depth_model.rds')
v2_lm <- readRDS('data/long_cores/chronology/v2_c14_age_depth_model.rds')

ams_meta <- readRDS('data/long_cores/chronology/long_core_ams_meta.rds')

standard_yr_bp <- 1950 # the year used in the literature as BP datum
yr_core_ce <- 2017 # this is the year we took the core
yr_core_bp <- standard_yr_bp-yr_core_ce

#### sed data ####

v1_all <- read.csv("data/Sediment/LOI/v1_LOI_most_recent_update.csv") %>% 
  # filter(is.na(turbidite) == TRUE) %>% 
  select(depth = Depth..cm.,
         excel_year = Year..AD.,
         LOI = LOI....) 

v1 <- read.csv("data/Sediment/LOI/v1_LOI_most_recent_update.csv") %>% 
  # filter(is.na(turbidite) == TRUE) %>%
  select(depth = Depth..cm.,
         excel_year = Year..AD.,
         LOI = LOI....,
         turbidite) |>  
  mutate(year_bp_new = predict(v1_lm,cur_data()),
         year_ce_new = standard_yr_bp - year_bp_new,
         diff_time = lag(year_ce_new) - year_ce_new)  # linear interpolation on AMS date

v2_all <- read.csv("data/Sediment/LOI/v2_LOI_most_recent_update.csv") %>% 
  # filter(is.na(turbidite) == TRUE) %>% 
  select(depth = Depth..cm.,
         LOI = LOI..)

v2 <- read.csv("data/Sediment/LOI/v2_LOI_most_recent_update.csv") %>% 
  # filter(is.na(turbidite) == TRUE) %>%
  select(depth = Depth..cm.,
         LOI = LOI..,
         turbidite) |>  
  mutate(year_bp_new = predict(v2_lm,cur_data()),
         year_ce_new = standard_yr_bp - year_bp_new,
         diff_time = lag(year_ce_new) - year_ce_new)  # linear interpolation on AMS date

v1.sd <- sd(v1_all$LOI, na.rm = T)
v1.mean <- mean(v1_all$LOI, na.rm = T)

v1.sd.fltr <- sd(v1$LOI, na.rm = T)
v1.mean.fltr <- mean(v1$LOI, na.rm = T)

v1$stdep <- (v1$LOI - v1.mean.fltr)/v1.sd.fltr

v2.sd <- sd(v2_all$LOI, na.rm = T)
v2.mean <- mean(v2_all$LOI, na.rm = T)

v2.sd.fltr <- sd(v2$LOI, na.rm = T)
v2.mean.fltr <- mean(v2$LOI, na.rm = T)

v2$stdep <- (v2$LOI - v2.mean.fltr)/v2.sd.fltr

core_stats <- readRDS('data/long_cores/core_stats.rds')

stats <- data.frame(
  stat = rep(c('mean', 'mean_no_flood', 'sd', 'sd_no_flood'), 2),
  value = c(v1.mean, v1.mean.fltr, v1.sd, v1.sd.fltr, v2.mean, v2.mean.fltr, v2.sd, v2.sd.fltr),
  core = c(rep('V1', 4), rep("V2", 4)),
  metric = rep('loi', 8)
)

stats_out <- rbind(core_stats, stats)

saveRDS(stats_out, 'data/long_cores/core_stats.rds')

# save loi to rds

out <- rbind(v1 %>% select(depth, year_ce_new, LOI, stdep, turbidite) %>% mutate(core = "V1"), 
             v2 %>% select(depth, year_ce_new, LOI, stdep, turbidite) %>% mutate(core = "V2"))

saveRDS(out, 'data/Sediment/LOI/loi_v1_v2_working.RDS')

# plot on depth + estimated year
out <- readRDS('data/Sediment/LOI/loi_v1_v2_working.RDS')

v1$depth <- v1$depth*10
v2$depth <- v2$depth*10

v1_ax_trans <- lm(depth ~ year_ce_new, data = v1)
v2_ax_trans <- lm(depth ~ year_ce_new, data = v2)
