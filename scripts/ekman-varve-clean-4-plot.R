# this script cleans up the ekman varve thicknesses so we can plot with the long core 

ek <- read_csv('data/ekman/EK_varveCounting_orig_long_analysis.csv') |> 
  select(core_num, Year, layer_thickness_mm) |> 
  group_by(core_num) |> 
  mutate(mean_thickness_mm = mean(layer_thickness_mm, na.rm = T),
         sd_thickness_mm = sd(layer_thickness_mm, na.rm = T), 
         stdep_mm = (layer_thickness_mm - mean_thickness_mm)/sd_thickness_mm,
         ma_30 = zoo::rollapply(stdep_mm, width = 10, by = 1, FUN = mean, na.rm = T, align = "center", partial = T, fill = NA), # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
  ) |> 
  filter(core_num %in% c('EK13'))

ek

ggplot(ek, aes(Year, stdep_mm, colour = core_num, group = core_num)) +
  geom_line() +
  geom_line(aes(y = ma_30))
plotly::ggplotly()


