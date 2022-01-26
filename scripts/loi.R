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

# plot on depth + estimated year

v1_plot <- 
  v1 %>% 
  mutate(
    smooth = smoother::smth(x = stdep, method = 'gaussian', window = 5),
    mvavg = zoo::rollapply(stdep, width = 3, by = 1, FUN = mean, na.rm = T, align = "center", partial = T)
  ) %>% 
  ggplot(aes(x = depth*10)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 1, colour = "black", se=F, linetype="dashed", size = .5) +
  # geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE, colour = "gray") +
  geom_point(aes(y = stdep), alpha = 1) +
  #geom_line(aes(y = smooth)) +
  geom_line(aes(y = mvavg), colour = "gray") +
  geom_point(aes(x = v1_C14$depth_cm * 10, y = -1.5), shape = 4) +
  geom_text(aes(x = v1_C14$depth_cm * 10, y = -1.70), label = "47 ± 75 yr. CE", vjust = 1) +
  ylab("LOI Std. Dept.") +
  xlab("Core Depth (mm)") + 
  ylim(c(-3.5, 3))+
  ggtitle("V1") +
  scale_x_continuous( sec.axis=sec_axis(trans=~ 2017 - (. * (v1_C14$year/(v1_C14$depth_cm*10))), name="Year (CE)")) + # scale sec y axis based on c14
  theme_classic()
v1_plot

v2_plot <- 
  v2 %>% 
  mutate(
  #  smooth = smoother::smth(x = stdep, method = 'gaussian', window = 3),
    mvavg = zoo::rollapply(stdep, 5, mean, align = 'center', fill = NA)
  ) %>% 
  ggplot(aes(x = depth*10)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 1, colour = "black", se=F, linetype="dashed", size = .5) +
  geom_point(aes(y = stdep), alpha = 1) +
  #geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE) +
  geom_line(aes(y = mvavg), colour = "gray") +
  geom_point(aes(x = v2_C14$depth_cm * 10, y = -2), shape = 4) +
  geom_text(aes(x = v2_C14$depth_cm * 10, y = -2.25), label = "158 ± 40 yr. CE", vjust = 1) +
  ylab("LOI Std. Dept.") +
  xlab("Core Depth (mm)") +
  ylim(c(-3.5, 3))+
  ggtitle("V2") +
  scale_x_continuous( sec.axis=sec_axis(trans=~ 2017 - (. * (v2_C14$year/(v2_C14$depth_cm*10))), name="Year (CE)")) + # scale sec y axis based on c14
  theme_classic()
v2_plot

v2_plot2 <- 
  v2 %>% 
  mutate(
  #  smooth = smoother::smth(x = stdep, method = 'gaussian', window = 3),
    mvavg = zoo::rollapply(stdep, 5, mean, align = 'center', fill = NA)
  ) %>% 
  ggplot(aes(x = year_ce_new)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 1, colour = "black", se=F, linetype="dashed", size = .5) +
  geom_point(aes(y = stdep), alpha = 1) +
  scale_x_reverse()


ggplotly(v2_plot2)
p <- grid.arrange(v1_plot, v2_plot, nrow=2)

ggsave("figs/V1_V2_LOI_vs_depth_and_C14_est_yr.png", p,  width = 11, height = 6)
