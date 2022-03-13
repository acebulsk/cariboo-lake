library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(tidyr)

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

v1 <- read.csv("data/Sediment/LOI/v1_LOI_most_recent_update.csv") %>% 
  select(depth = Depth..cm.,
         excel_year = Year..AD.,
         LOI = LOI....) %>% 
  mutate(year_bp_new = depth * (v1_C14$year/(v1_C14$depth_cm)),
         year_ce_new = 2017 - year_bp_new,
         diff_time = lag(year_ce_new) - year_ce_new)  # linear interpolation on AMS date

v2 <- read.csv("data/Sediment/LOI/v2_LOI_most_recent_update.csv") %>% 
  select(depth = Depth..cm.,
         LOI = LOI..) %>% 
  mutate(year_bp_new = depth * (v2_C14$year/(v2_C14$depth_cm)),
         year_ce_new = 2017 - year_bp_new,
         diff_time = lag(year_ce_new) - year_ce_new)  # linear interpolation on AMS date

v1.sd.fltr <- sd(v1$LOI, na.rm = T)
v1.mean.fltr <- mean(v1$LOI, na.rm = T)

v1$stdep <- (v1$LOI - v1.mean.fltr)/v1.sd.fltr


v2.sd.fltr <- sd(v2$LOI, na.rm = T)
v2.mean.fltr <- mean(v2$LOI, na.rm = T)

v2$stdep <- (v2$LOI - v2.mean.fltr)/v2.sd.fltr

# save loi to rds

out <- rbind(v1 %>% select(depth, year_ce_new, LOI, stdep) %>% mutate(core = "V1"), 
             v2 %>% select(depth, year_ce_new, LOI, stdep) %>% mutate(core = "V2"))

saveRDS(out, 'data/Sediment/LOI/loi_v1_v2_working.RDS')

# plot on depth + estimated year
out <- readRDS('data/Sediment/LOI/loi_v1_v2_working.RDS')

v1$depth <- v1$depth*10
v2$depth <- v2$depth*10

v1_ax_trans <- lm(depth ~ year_ce_new, data = v1)
v2_ax_trans <- lm(depth ~ year_ce_new, data = v2)
