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
  filter(is.na(Symbol) == T) %>% 
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

good <- c('EK13', 'EK11')

ek %>% 
  filter(core_num %in% good) %>% 
  select(year = Year, layer_thickness_mm, core_num) %>% 
  ggplot(aes(x = year, y = layer_thickness_mm, colour = core_num)) +
  geom_line()
  

ek13 <- ek %>%
  filter(core_num == "EK13") %>% 
  select(year = Year, layer_thickness_mm) %>% 
  mutate(year = year + 1)

jn <- left_join(qn_dl, ek13)

plot(jn$layer_thickness_mm, jn$Value)

ek13_lm <- lm(Value ~ layer_thickness_mm, data = jn)

summary(ek13_lm)$r.squared

ek11 <- ek %>%
  filter(core_num == "EK11") %>% 
  select(year = Year, layer_thickness_mm) %>% 
  mutate(year = year + 1)

jn <- left_join(qn_dl, ek11)

plot(jn$layer_thickness_mm, jn$Value)

ek11_lm <- lm(Value ~ layer_thickness_mm, data = jn)

summary(ek11_lm)$r.squared

ek8 <- ek %>%
  filter(core_num == "EK8") %>% 
  select(year = Year, layer_thickness_mm) %>% 
  mutate(year = year)

jn <- left_join(qn_dl, ek8)

ek8_redo <- data.frame(
  layer_number = c(0:7),
  thickness = c(2, 1.7,1.2,1.8,1.3,1.7,0.7,1.5)) %>% 
  mutate(
    thickness = thickness / 2, # measured at 2x scale with ruler 
    year = 2017 - layer_number
  )

jn <- left_join(qn_dl, ek8_redo)


plot(jn$thickness, jn$Value)
