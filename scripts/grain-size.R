library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(tidyr)

setwd("D:/AlexC/OneDrive - University of Toronto/School/Grad School/Cariboo/Analysis/Sediment/Grain Size")

v1 <- read.csv("CB17_Jan_GranSize_mar23_v1_noFloods.csv") %>% 
  rename(depth = Depth..cm., 
         year = Year..AD., 
         D50 = Dx..50., 
         perc_clay = Result.In.Range....01.2...m, 
         perc_silt = Result.In.Range...2.63...m, 
         perc_sand = Result.In.Range...63.2000...m) %>% 
  filter(depth != 51) %>% 
  mutate(year_bp = abs(year - 2017))

v1_D50 <- v1 %>% 
  select(depth, year_bp, D50) %>% 
  group_by(depth) %>% 
  summarize(across(year_bp:D50, mean)) %>% # averge double samples that were done to confirm smaller grain size at 259 cm
  ungroup() %>% 
  pivot_longer(D50, names_to = "group", values_to = "D50") 

v1_percentages <- v1 %>% 
  select(depth, year_bp, perc_clay:perc_sand) %>% 
  group_by(depth) %>% 
  summarize(across(year_bp:perc_sand, mean)) %>%  # averge double samples that were done to confirm smaller grain size at 259 cm
  ungroup() %>% 
  pivot_longer(perc_clay:perc_sand, names_to = "group", values_to = "perc") 
v1_percentages$group <- factor(v1_percentages$group, c("perc_sand", "perc_silt", "perc_clay"))



v1Plot <- ggplot(data = v1, aes(y = depth, x = D50)) +
  geom_point() +
  xlab("D50 (μm)") +
  ylab("Depth (cm)") +
  scale_y_continuous(trans = "reverse")

v1Plot_yr <- ggplot(data = v1_D50, aes(y = year_bp, x = D50)) +
  geom_point() +
  xlab("D50 (μm)") +
  ylab("Year (BP)") +
  scale_y_continuous(trans = "reverse")


v1Perc <- ggplot() +
  geom_area(data = v1_percentages, aes(x = year_bp, y = perc, group = group, fill = group), position = "fill") +
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

p <- grid.arrange(v1Plot_yr, v1Perc, ncol=2)


ggsave("figs/v1_D50_particlePercent.png",p,  width = 4, height = 8)

##### V2 #####

v2 <- read.csv("CB17_Jan_GranSize_mar23_v2_noFloods.csv")%>% 
  rename(depth = Depth..cm., 
         year = Year, 
         D50 = Dx..50., 
         perc_clay = Result.In.Range....01.2...m, 
         perc_silt = Result.In.Range...2.63...m, 
         perc_sand = Result.In.Range...63.2000...m) %>% 
  mutate(year_bp = abs(year - 2017))

v2_D50 <- v2 %>% 
  select(depth, year_bp, D50) %>% 
  group_by(depth) %>% 
  summarize(across(year_bp:D50, mean)) %>% # averge double samples that were done to confirm smaller grain size at 259 cm
  ungroup() %>% 
  pivot_longer(D50, names_to = "group", values_to = "D50") 

v2_percentages <- v2 %>% 
  select(depth, year_bp, perc_clay:perc_sand) %>% 
  group_by(depth) %>% 
  summarize(across(perc_clay:perc_sand, mean)) %>%  # averge double samples that were done to confirm smaller grain size at 259 cm
  ungroup() %>% 
  pivot_longer(perc_clay:perc_sand, names_to = "group", values_to = "perc") 
v2_percentages$group <- factor(v2_percentages$group, c("perc_sand", "perc_silt", "perc_clay"))



v2Plot <- ggplot(data = v2_D50, aes(y = depth, x = D50)) +
  geom_point() +
  xlab("D50 (μm)") +
  ylab("Depth (cm)") +
  scale_y_continuous(trans = "reverse")

v2Plot_yr <- ggplot(data = v2_D50, aes(y = year_bp, x = D50)) +
  geom_point() +
  xlab("D50 (μm)") +
  scale_y_continuous(trans = "reverse") +
  ylab("Year (BP)") 

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


v2Perc <- ggplot() +
  geom_area(data = v2_percentages, aes(x = depth, y = perc, group = group, fill = group), position = "fill") +
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

p <- grid.arrange(v2Plot_yr, v2Perc, ncol=2)
p

ggsave("figs/v2_D50_particlePercent.png",p,  width = 4, height = 8)


