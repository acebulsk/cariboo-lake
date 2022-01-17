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
v1 <- read.csv("data/Sediment/Grain Size/CB17_Jan_GranSize_mar23_v1_noFloods.csv") %>% 
  rename(depth = Depth..cm., 
         year = Year..AD., 
         D50 = Dx..50., 
         perc_clay = Result.In.Range....01.2...m, 
         perc_silt = Result.In.Range...2.63...m, 
         perc_sand = Result.In.Range...63.2000...m) %>% 
  filter(depth != 51) %>% 
  mutate(year_bp = abs(year - 2017))

v1_D50 <- v1 %>% 
  select(depth, year, D50) %>% 
  group_by(depth) %>% 
  summarize(across(year:D50, mean)) %>% # averge double samples that were done to confirm smaller grain size at 259 cm
  ungroup() %>% 
  pivot_longer(D50, names_to = "group", values_to = "D50") %>% 
  mutate(year_bp_new = depth * (v1_C14$year/(v1_C14$depth_cm)),
         year_ce_new = 2017 - year_bp_new)  # linear interpolation on AMS date

v1_percentages <- v1 %>% 
  select(depth, year_bp, perc_clay:perc_sand) %>% 
  group_by(depth) %>% 
  summarize(across(year_bp:perc_sand, mean)) %>%  # averge double samples that were done to confirm smaller grain size at 259 cm
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

v2 <- read.csv("data/Sediment/Grain Size/CB17_Jan_GranSize_mar23_v2_noFloods.csv")%>% 
  rename(depth = Depth..cm., 
         year = Year, 
         D50 = Dx..50., 
         perc_clay = Result.In.Range....01.2...m, 
         perc_silt = Result.In.Range...2.63...m, 
         perc_sand = Result.In.Range...63.2000...m) %>% 
  mutate(year_bp = abs(year - 2017)) %>% 
  filter(notes != "skewed by floods on either side")

v2 %>% 
  pivot_longer(c(D50, perc_clay:perc_sand)) %>% 
  ggplot(aes(year_bp, x = value)) +
  geom_point() +
  facet_grid(~name, scales = 'free') +
  xlab("D50 (μm)") +
  scale_y_continuous(trans = "reverse") +
  ylab("Year (CE)") 

v2_D50 <- v2 %>% 
  select(depth, year_bp, D50) %>% 
  group_by(depth) %>% 
  summarize(across(year_bp:D50, mean)) %>% # averge double samples that were done to confirm smaller grain size at 259 cm
  ungroup() %>% 
  pivot_longer(D50, names_to = "group", values_to = "D50") %>% 
  mutate(year_bp_new = depth * (v2_C14$year/(v2_C14$depth_cm)),
         year_ce_new = 2017 - year_bp_new) # linear interpolation on AMS date

v2_percentages <- v2 %>% 
  select(depth, year_bp, perc_clay:perc_sand) %>% 
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

v1.sd.fltr <- sd(v1_D50$D50, na.rm = T)
v1.mean.fltr <- mean(v1_D50$D50, na.rm = T)

v1_D50$stdep <- (v1_D50$D50 - v1.mean.fltr)/v1.sd.fltr


v2.sd.fltr <- sd(v2_D50$D50, na.rm = T)
v2.mean.fltr <- mean(v2_D50$D50, na.rm = T)

v2_D50$stdep <- (v2_D50$D50 - v2.mean.fltr)/v2.sd.fltr


# plot on depth 

v1_plot <- 
  v1_D50 %>% 
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
  geom_text(aes(x = v1_C14$depth_cm * 10, y = -1.70), label = "1819-1899 BP", vjust = 1) +
  ylab("D50") +
  xlab("Core Depth (mm)") +  
  ggtitle("V1") +
  scale_x_continuous( sec.axis=sec_axis(trans=~ 2017 - (. * (v1_C14$year/(v1_C14$depth_cm*10))), name="Year (CE)")) # scale sec y axis based on c14
v1_plot

v2_plot <- 
  v2_D50 %>% 
  mutate(
    smooth = smoother::smth(x = stdep, method = 'gaussian', window = 3),
    mvavg = zoo::rollapply(stdep, 5, mean, align = 'center', fill = NA)
  ) %>% 
  ggplot(aes(x = depth*10)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 1, colour = "black", se=F, linetype="dashed", size = .5) +
  geom_point(aes(y = stdep), alpha = 1) +
  #geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE) +
  geom_line(aes(y = mvavg), colour = "gray") +
  geom_point(aes(x = v2_C14$depth_cm * 10, y = -1.5), shape = 4) +
  geom_text(aes(x = v2_C14$depth_cm * 10, y = -1.75), label = "1895-2045 BP", vjust = 1) +
  ylab("D50") +
  xlab("Core Depth (mm)") +  
  ggtitle("V2") +
  scale_x_continuous( sec.axis=sec_axis(trans=~ 2017 - (. * (v2_C14$year/(v2_C14$depth_cm*10))), name="Year (CE)")) # scale sec y axis based on c14

v2_plot

p <- grid.arrange(v1_plot, v2_plot, nrow=2)

ggsave("figs/grain-size/V1_V2_grainsize_vs_depth_and_C14_est_yr.png", p,  width = 11, height = 6)

#### combined ####

rbind(v1_D50, v2_D50) %>% 
  mutate(
    group = case_when(
    between(year_bp_new, 1500, 2100) ~ "2100 - 1500",
    between(year_bp_new, 500, 1499) ~ "1499 - 500",
    between(year_bp_new, 0, 499) ~ "499 - 0"
  ))
