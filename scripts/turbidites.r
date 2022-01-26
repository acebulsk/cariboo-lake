library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(tidyr)

# look at turbidite events at both V1 and V2

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

turb <- read.csv('data/long_cores/V1_V2_turbidite_deposits.csv') %>% 
  select(core = Core, excel_yr = AD, thickness = Couplet.Thickness..mm., depth = Core.Depth..cm.) %>% 
  left_join(ams_select) %>% 
  mutate(year_bp_new = depth * (ams_year/(ams_depth_cm)),
         year_ce_new = 2017 - year_bp_new)

ggplot(turb, aes(x = year_ce_new, y = thickness, colour = core)) + 
  geom_point() +
  scale_x_reverse() +
  ylab('Turbidite Thickness (mm)') +
  xlab('Year (CE)') +
  theme_bw()

ggsave("figs/V1_V2_turbidite_vs_time.png",  width = 6, height = 3)

turb_cln <- turb %>% 
  mutate(depth = depth * 10) %>% 
  dplyr::arrange(core, depth) %>% 
  select(Core = core, `Depth (mm)` = depth, `Est. Year (CE)` = year_ce_new, `Lam. Thickness (mm)` = thickness)

saveRDS(turb_cln, file = "figs/turbidite_tbl.rds")
