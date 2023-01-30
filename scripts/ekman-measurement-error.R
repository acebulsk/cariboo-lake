# what is the varve thickness error
# since we dont have triplicate cores in one location the best we can do is use 
# the difference between 4 ekman cores which have a 2 km spread down lake much of the error here is
# likely attributed more to spatial variability than counting error. However there still was some subjectivity
# in this sample when counting 

library(tidyverse)

ek <- read_csv('data/ekman/EK_varveCounting_orig_long_analysis.csv') |> 
  group_by(core_num) |> 
  mutate(
    year_CE = Year + 1, # miscalculated year originally top layer should be 2017 as that when we cored.. 
    # mean_thickness_mm = mean(layer_thickness_mm, na.rm = T),
    # sd_thickness_mm = sd(layer_thickness_mm, na.rm = T), 
    # stdep_mm_e = (layer_thickness_mm - mean_thickness_mm)/sd_thickness_mm,
    # stdep_mm = (layer_thickness_mm - v2_mean)/v2_sd,
    # ma_30 = zoo::rollapply(stdep_mm, width = 10, by = 1, FUN = mean, na.rm = T, align = "center", partial = T, fill = NA), # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
  ) |> 
  filter(core_num %in% c('EK13', 'EK14', 'EK12', 'EK11')) |> 
  ungroup() |> 
  select(year_CE, lyr_mm = layer_thickness_mm, core_num)

ek

ggplot(ek, aes(year_CE, lyr_mm, colour = core_num)) +
  geom_line() 
plotly::ggplotly()

ek |> 
  filter(!year_CE %in% c(2001, 2006, 2017)) |> 
  group_by(year_CE) |> 
  summarise(mean = mean(lyr_mm, na.rm = T),
            sd = sd(lyr_mm, na.rm = T),
            n = n()) |> 
  filter(n > 2)

mean(smry$sd, na.rm = T)

ek |> 
  group_by(core_num) |> 
  filter(!year_CE %in% c(2001, 2006, 2017)) |> 
  summarise(sd = sd(lyr_mm, na.rm = T))
