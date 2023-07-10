library(tidyverse)
library(tidyhydat)

# brind in ekman varve thickness data 

ek <- read.csv('data/ekman/EK_varveCounting_orig_long_analysis.csv')

# look at gauges near cariboo https://wateroffice.ec.gc.ca/google_map/google_map_e.html?map_type=historical&search_type=province&province=BC

# select gauges

# still active
qn_id <- '08KH006'

# ends 1974 but useful for making hydrograph
cb_id <- '08KH013'

# hydrographs
mon <- data.frame(Month = seq(1:12), month_name = factor(month.abb, ordered = T, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")))

cb <- hy_monthly_flows(cb_id) %>% 
  filter(Sum_stat == 'MEAN') %>% 
  group_by(Month) %>% 
  summarise(Value = mean(Value))  %>% 
  left_join(mon)
  
ggplot(cb, aes(x = month_name, y = Value, group = 1)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  ylab(expression(paste("Discharge ", m^{3},s^{-1}))) +
  theme(axis.title.x = element_blank())

qn <- hy_monthly_flows(qn_id) %>% 
  filter(Sum_stat == 'MEAN',
         Full_Month == T) %>% 
  group_by(Month) %>% 
  summarise(Value = mean(Value))  %>% 
  left_join(mon)

ggplot(qn, aes(x = month_name, y = Value, group = 1)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  ylab(expression(paste("Discharge ", m^{3},s^{-1}))) +
  theme(axis.title.x = element_blank())

# maximum mean daily discharge as in Menounos2008
qn_dl <- hy_daily_flows(qn_id) %>% 
  # filter(is.na(Symbol) == T) %>% 
  mutate(
    Date = as.Date(Date),
    year = lubridate::year(Date)) %>% 
  group_by(year) %>% 
  summarise(Value = max(Value))

tidyhydat::hy_data_symbols

# ekman stuff 

# Bring In Raw Ekman Data 9 original counting by alex MSc times 
ek <- ek %>%  
  select(1:6) %>% 
  group_by(core_num) %>% 
  mutate(sd = sd(layer_thickness_mm, na.rm = T),
         mean = mean(layer_thickness_mm, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  mutate(sd_flag = case_when(
    layer_thickness_mm > 3*sd ~ T,
    TRUE ~ F
  )) 

# plot good ones 

good <- c('EK13', 'EK11', 'EK17', 'EK19', 'EK20')

# remove some outliers

ek$layer_thickness_mm[ek$core_num == 'EK11' & ek$layer_thickness_mm == 3.9601] <- NA
ek$layer_thickness_mm[ek$core_num == 'EK13' & ek$layer_thickness_mm == 2.5639] <- NA
ek$layer_thickness_mm[ek$core_num == 'EK13' & ek$layer_thickness_mm == 4.3897] <- NA
ek$layer_thickness_mm[ek$core_num == 'EK13' & ek$layer_thickness_mm == 4.7821] <- NA

ek_fltr <- ek |> 
  # filter(core_num %in% good) %>% 
  select(year = Year, layer_thickness_mm, core_num) |> 
  mutate(year = 
           case_when(core_num == 'EK11' ~ year + 1,
                     TRUE ~ year)) |> 
  left_join(qn_dl) |> 
  mutate(core_num = as.numeric(gsub(".*?([0-9]+).*" , "\\1", core_num))) 
  
# ggplot(ek_fltr, aes(x = year, y = layer_thickness_mm, colour = core_num)) +
#   geom_line()

ggplot(ek_fltr, aes(layer_thickness_mm, Value)) +
  facet_wrap(~core_num, scales = 'free_x') +
  ylab(expression(paste("Mean Daily Discharge ", m^{3},s^{-1}))) +
  xlab('Varve Thickness (mm)') +
  geom_smooth(method = 'lm', se = F) +
  ggpubr::stat_cor(aes(label = after_stat(r.label)), geom = "label", show.legend = F) +
  geom_point()

ggsave('figs/ekman/ekman_varve_q_regress.bmp', width = 8.5, height = 8.5)
