library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(tidyr)

v1_tb <- c(44, 124, 120, 196, 251, 257)
v2_tb <- c(88, 92, 168, 172, 224, 228)

v2_discard <- c(120)

# v1_xl <- readxl::read_xlsx('data/Sediment/Grain Size/CB17_Jan_GranSize_mar23.xlsx',sheet = '226_Raw') %>% 
#   filter(stringr::str_starts(`Sample Name`, 'Average')) %>% 
#   mutate(core = 'V1')
# 
# v2_xl <- readxl::read_xlsx('data/Sediment/Grain Size/CB17_Jan_GranSize_mar23.xlsx',sheet = '224_Raw') %>% 
#   filter(stringr::str_starts(`Sample Name`, 'Average'),
#          !stringr::str_detect(`Sample Name`, 'Sonic')) %>% 
#   mutate(core = 'V2')
# 
# gs <- rbind(v1_xl, v2_xl)
# 
# strs <- 
#   'Average of \'CB17_V2_|Average of \'CB17_V1_|Average of \'CB17_V2B_|Average of \'CB17_V2C_|cm|\''
# 
# gs$core_depth <- gsub(strs, "", gs$`Sample Name`)
# 
# tb <- gs %>% 
#   filter(stringr::str_detect(core_depth, 'Flood') | stringr::str_detect(core_depth, 'flood')) 
# 
# tb$core_depth <- gsub('_.*', "", tb$core_depth)
# 
# tb$core_depth[1] <- 12 
# 
# gs_no_tb <- gs %>% 
#   filter(!stringr::str_detect(core_depth, '[F]'),
#          !stringr::str_detect(core_depth, '[f]')
#          ) 
# 
# gs_no_tb$core_depth <- gsub('.*_', "", gs_no_tb$core_depth)
# 
# 
# gs_new <- rbind(gs_no_tb, tb) %>% 
#   mutate(core_depth = as.numeric(core_depth))

# saveRDS(gs_new, 'data/long_cores/gain_size_w_floods.rds')

## carbon dates ##
# A small twig from V1 at 347 cm results in a date of 1899-1819 cal BP. 
# A 4 cm long twig from V2 at 222 cm results in a date of 490-316 cal BP. 
# Since the first date from V2 was much younger than than expected, 
# a second sample from V2 was analyzed by combining a small twig at 286 cm and pine needle at 294 cm. 
# A date of 2045-1895 cal BP was determined.

v1_date <- (1899 + 1819) / 2 # mid point yr for v1 @ 347 cm

v1_C14 <- data.frame(depth_cm = 347, year = v1_date) # n = 1

v2b_depth <- (286 + 294) / 2 # avg depth for combined V2 sample

v2_date_b <- (2045 + 1895) / 2 # mid point yr for v2 @ 286 + 294 cm
v2_date_a <- (490 + 316) / 2 # mid point yr for v2 @ 222 cm

v2_C14 <- data.frame(depth_cm = 286, year = 1970) # n = 2

ams_df <- tibble(
  year = c(0, v1_date, 0, v2_date_a, 0, v2_date_b),
  depth = c(0, 347, 0, 222, 0, v2b_depth),
  ams_sample = c('V1', 'V1', 'V2a', 'V2a', 'V2b', 'V2b')
)
v1 <- readRDS("data/long_cores/gain_size_w_floods.rds") %>% 
  filter(core == 'V1') %>% 
  rename(depth = core_depth, 
         D50 = `Dx (50)`, 
         perc_clay = `Result In Range  (0.01,2) μm`, 
         perc_silt = `Result In Range  (2,63) μm`, 
         perc_sand = `Result In Range  (63,2000) μm`) 
# %>% 
#   filter(depth != 51)

v1_D50 <- v1 %>% 
  filter(!`Record Number` %in% v1_tb) %>% 
  select(depth, D50) %>% 
  group_by(depth) %>% 
  summarize(D50 = mean(D50)) %>% # averge double samples that were done to confirm smaller grain size at 259 cm
  ungroup() %>% 
  pivot_longer(D50, names_to = "group", values_to = "D50") %>% 
  mutate(year_bp_new = depth * (v1_C14$year/(v1_C14$depth_cm)),
         year_ce_new = 2017 - year_bp_new,
         diff_time = lag(year_ce_new) - year_ce_new)  # linear interpolation on AMS date

v1_percentages <- v1 %>% 
  filter(!`Record Number` %in% v1_tb) %>% 
  select(depth, perc_clay:perc_sand) %>% 
  group_by(depth) %>% 
  summarize(across(perc_clay:perc_sand, mean)) %>%  # averge double samples that were done to confirm smaller grain size at 259 cm
  ungroup() %>% 
  pivot_longer(perc_clay:perc_sand, names_to = "group", values_to = "perc") %>% 
  mutate(year_bp_new = depth * (v1_C14$year/(v1_C14$depth_cm)),
         year_ce_new = 2017 - year_bp_new)  # linear interpolation on AMS date

v1_percentages$group <- factor(v1_percentages$group, c("perc_sand", "perc_silt", "perc_clay"))



v1Plot <- ggplot(data = v1, aes(y = depth, x = D50)) +
  geom_point() +
  xlab("D50 (μm)") +
  ylab("Depth (cm)") +
  scale_y_continuous(trans = "reverse")

v1Plot_yr <- ggplot(data = v1_D50, aes(y = year_ce_new, x = D50)) +
  geom_point() +
  xlab("D50 (μm)") +
  ylab("Year (CE)") #+
  #scale_y_continuous(trans = "reverse")


v1Perc <- ggplot() +
  geom_area(data = v1_percentages, aes(x = year_ce_new, y = perc, group = group, fill = group), position = "fill") +
  # geom_point(data = v1_D50, aes(y = D50, x = depth)) +
  coord_flip() +
  ylab("Percentage") +
  scale_x_continuous(trans = "reverse") +
  scale_fill_manual(values=c("black", NA, "grey60")) +
  scale_y_continuous(
    labels = scales::percent
    
  ) + theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y=element_blank()) 

# v1Perc <- ggplot() +
#   geom_line(data = v1_percentages, aes(x = depth, y = perc)) +
#   # geom_point(data = v1_D50, aes(y = D50, x = depth)) +
#   coord_flip() +
#   scale_x_continuous(trans = "reverse") +
#   facet_wrap(~group)

p <- grid.arrange(v1Plot_yr, v1Perc, ncol=2, widths = c(3, 7))


ggsave("figs/grain-size/v1_D50_particlePercent.png", p,  width = 6, height = 8)

##### V2 #####

v2 <- readRDS("data/long_cores/gain_size_w_floods.rds") %>% 
  filter(core == 'V2') %>% 
  rename(depth = core_depth, 
         D50 = `Dx (50)`, 
         perc_clay = `Result In Range  (0.01,2) μm`, 
         perc_silt = `Result In Range  (2,63) μm`, 
         perc_sand = `Result In Range  (63,2000) μm`) %>% 
  filter(`Record Number` != 120) # skewed by floods either side

v2_D50 <- v2 %>% 
  filter(!`Record Number` %in% v2_tb) %>% 
  select(depth, D50) %>% 
  group_by(depth) %>% 
  summarize(D50 = mean(D50)) %>% # averge double samples that were done to confirm smaller grain size at 259 cm
  ungroup() %>% 
  pivot_longer(D50, names_to = "group", values_to = "D50") %>% 
  mutate(year_bp_new = depth * (v2_C14$year/(v2_C14$depth_cm)),
         year_ce_new = 2017 - year_bp_new,
         diff_time = lag(year_ce_new) - year_ce_new)  # linear interpolation on AMS date

v2_percentages <- v2 %>% 
  filter(!`Record Number` %in% v2_tb) %>% 
  select(depth, perc_clay:perc_sand) %>% 
  group_by(depth) %>% 
  summarize(across(perc_clay:perc_sand, mean)) %>%  # averge double samples that were done to confirm smaller grain size at 259 cm
  ungroup() %>% 
  pivot_longer(perc_clay:perc_sand, names_to = "group", values_to = "perc")  %>% 
  mutate(year_bp_new = depth * (v2_C14$year/(v2_C14$depth_cm)),
         year_ce_new = 2017 - year_bp_new) # linear interpolation on AMS date

v2_percentages$group <- factor(v2_percentages$group, c("perc_sand", "perc_silt", "perc_clay"))



v2Plot <- ggplot(data = v2_D50, aes(y = depth, x = D50)) +
  geom_point() +
  xlab("D50 (μm)") +
  ylab("Depth (cm)") +
  scale_y_continuous(trans = "reverse")

v2Plot_yr <- ggplot(data = v2_D50, aes(y = year_ce_new, x = D50)) +
  geom_point() +
  xlab("D50 (μm)") +
  # scale_y_continuous(trans = "reverse") +
  ylab("Year (CE)") 

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


v2Perc <- ggplot() +
  geom_area(data = v2_percentages, aes(x = year_ce_new, y = perc, group = group, fill = group), position = "fill") +
  # geom_point(data = v2_D50, aes(y = D50, x = depth)) +
  coord_flip() +
  ylab("Percent") +
  scale_x_continuous(trans = "reverse") +
  scale_fill_manual(values=c("black", NA, "grey60")) +
  scale_y_continuous(
    labels = scales::percent
    
  ) + theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y=element_blank()) 

# v2Perc <- ggplot() +
#   geom_line(data = v2_percentages, aes(x = depth, y = perc)) +
#   # geom_point(data = v2_D50, aes(y = D50, x = depth)) +
#   coord_flip() +
#   scale_x_continuous(trans = "reverse") +
#   facet_wrap(~group)

p <- grid.arrange(v2Plot_yr, v2Perc, ncol=2, widths = c(3, 7))
p

ggsave("figs/v2_D50_particlePercent.png",p,  width = 4, height = 8)

## Long plots Std. Departure

# averages 
v1.sd <- sd(v1$D50, na.rm = T)
v1.mean <- mean(v1$D50, na.rm = T)

v1.sd.fltr <- sd(v1_D50$D50, na.rm = T)
v1.mean.fltr <- mean(v1_D50$D50, na.rm = T)

v1_D50$stdep <- (v1_D50$D50 - v1.mean.fltr)/v1.sd.fltr

v2.sd <- sd(v2$D50, na.rm = T)
v2.mean <- mean(v2$D50, na.rm = T)

v2.sd.fltr <- sd(v2_D50$D50, na.rm = T)
v2.mean.fltr <- mean(v2_D50$D50, na.rm = T)

v2_D50$stdep <- (v2_D50$D50 - v2.mean.fltr)/v2.sd.fltr

core_stats <- readRDS('data/long_cores/core_stats.rds')


gs_stats <- data.frame(
  stat = rep(c('mean', 'mean_no_flood', 'sd', 'sd_no_flood'), 2),
  value = c(v1.mean, v1.mean.fltr, v1.sd, v1.sd.fltr, v2.mean, v2.mean.fltr, v2.sd, v2.sd.fltr),
  core = c(rep('V1', 4), rep("V2", 4)),
  metric = rep('grain_size', 8)
)

core_stats_out <- rbind(core_stats, gs_stats)

saveRDS(core_stats_out, 'data/long_cores/core_stats.rds')

# plot on depth 
v1_D50$depth <- v1_D50$depth * 10 
v2_D50$depth <- v2_D50$depth * 10 

grain_df <- rbind(v1_D50 %>% select(depth, year_ce_new, D50, stdep) %>% mutate(core = "V1"), 
                  v2_D50 %>% select(depth, year_ce_new, D50, stdep) %>% mutate(core = "V2"))

saveRDS(grain_df, 'data/long_cores/grain_size_v1_v2_combined.RDS')
