library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(tidyr)
library(zoo) 
library(tsibble)
library(fpp2)
library(pracma)

## Ekmans 
df.ek <- read.csv('data/ekman/EK_varveCounting_orig_long_analysis.csv')

# Bring In Raw Ekman Data 9 original counting by alex MSc times 
ek <- df.ek %>%  
  select(1:6) %>% 
  group_by(core_num) %>% 
  mutate(sd = sd(layer_thickness_mm, na.rm = T),
            mean = mean(layer_thickness_mm, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  mutate(sd_flag = case_when(
    layer_thickness_mm > 3*sd ~ T,
    TRUE ~ F
  )) 

# Lets look at just EK13 first, closest core to V2, need to discount the 222 cm date
#filter out sd_flags  doesnt really make a difference to overall model fit 

ek13 <- ek %>%
  filter(core_num == "EK13", 
         sd_flag == F)

ggplot(ek13, aes(yr_bp, cumul_depth_mm)) +
  geom_point() +
  scale_y_continuous(trans = 'reverse') +
  geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x)

ek13_lm <- lm(cumul_depth_mm ~ 0 + yr_bp, data = ek13)

ek13_sed_rate <- summary(ek13_lm)$coeff[1] # filtered to less than 3 sd's and forced through origin 

ek13_sed_rate 

# Look at multiple Ekman sed rates close by 

prox_v1 <- c('EK11', 'EK12') # ekmans close to V2 in order of proximity 
prox_v2 <- c('EK13', 'EK14', 'EK15') # ekmans close to V2 in order of proximity

# ekman cores close to v1 
ek_v1 <- df.ek %>%  
  select(1:6) %>% 
  group_by(core_num) %>% 
  mutate(sd = sd(layer_thickness_mm, na.rm = T),
         mean = mean(layer_thickness_mm, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  mutate(sd_flag = case_when(
    layer_thickness_mm > 3*sd ~ T,
    TRUE ~ F
  )) %>% 
  filter(core_num %in% prox_v1,
         sd_flag == F)

ek_v1_sed_rates <- ek_v1 %>% 
  group_by(core_num) %>% 
  do(model = lm(cumul_depth_mm ~ 0 + yr_bp, data = .)) %>% 
  mutate(sed_rates = summary(model)$coeff[1]) %>% # cm to mm
  select(-model)

ggplot(ek_v1, aes(yr_bp, cumul_depth_mm, colour = core_num)) +
  geom_point() +
  scale_y_continuous(trans = 'reverse') +
  geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x)
  
# ekman cores close to v2

ek_v2 <- df.ek %>%  
  select(1:6) %>% 
  group_by(core_num) %>% 
  mutate(sd = sd(layer_thickness_mm, na.rm = T),
         mean = mean(layer_thickness_mm, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  mutate(sd_flag = case_when(
    layer_thickness_mm > 3*sd ~ T,
    TRUE ~ F
  )) %>% 
  filter(core_num %in% prox_v2,
         sd_flag == F)

ek_v2_sed_rates <- ek_v2 %>% 
  group_by(core_num) %>% 
  do(model = lm(cumul_depth_mm ~ 0 + yr_bp, data = .)) %>% 
  mutate(sed_rates = summary(model)$coeff[1]) %>% # cm to mm
  select(-model)

ggplot(ek_v2, aes(yr_bp, cumul_depth_mm/10, colour = core_num)) +
  geom_point() +
  scale_y_continuous(trans = 'reverse') +
  geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x, fullrange=TRUE, linetype = "dashed", size= 0.5) +
  xlab("Couplet Number") +
  ylab("Core Depth (cm)")

ggsave('figs/ekman_sed_rate_near_V2.png', width = 6, height = 4.5)

## carbon dates ##
# A small twig from V1 at 347 cm results in a date of 1899-1819 cal BP. 
# A 4 cm long twig from V2 at 222 cm results in a date of 490-316 cal BP. 
# Since the first date from V2 was much younger than than expected, 
# a second sample from V2 was analyzed by combining a small twig at 286 cm and pine needle at 294 cm. 
# A date of 2045-1895 cal BP was determined.

# plot on depth 
v1_date <- (1899 + 1819) / 2 # mid point yr for v1 @ 347 cm

v1_C14 <- data.frame(depth_cm = 347, year = v1_date) # n = 1

v2_date_286 <- (2045 + 1895) / 2 # mid point yr for v2 @ 286 cm
v2_date_222 <- (490 + 316) / 2 # mid point yr for v2 @ 222 cm

v2_C14 <- data.frame(depth_cm = 286, year = 1970) # n = 2

ams_df <- tibble(
  year = c(0, v1_date, 0, v2_date_222, 0, v2_date_286),
  depth = c(0, 347, 0, 222, 0, 286),
  ams_sample = c('V1', 'V1', 'V2a', 'V2a', 'V2b', 'V2b')
)

# Look at regression not through origin 

ams_scatter <- ams_df %>% 
  filter(depth != 0)

scatter <- lm(depth ~ year, data = ams_scatter) # vibro ams point not through the origin 

ggplot(ams_df, aes(year, depth, colour = ams_sample)) +
  geom_point() +
  scale_y_continuous(trans = 'reverse') +
  geom_smooth(method = 'lm', se = F, linetype = "dashed", size= 0.5) +
  geom_abline(slope = -summary(scatter)$coeff[2], intercept = -summary(scatter)$coeff[1]) +
  scale_y_continuous(trans = 'reverse') +
  xlab("Mid-Point Cal Year (BP)") +
  ylab("Core Depth (cm)")

ggsave('figs/ams_sed_rates.png', width = 6, height = 4.5)


vibro_sed_rates <- ams_df %>% 
  group_by(ams_sample) %>% 
  do(model = lm(depth ~ year, data = .)) %>% 
  mutate(sed_rates = summary(model)$coeff[2] * 10) %>% # cm to mm
  select(-model)

# make our ek_v2 df compatible so we can plot all together 
ek_v2_cln <- ek_v2 %>% 
  select(year = yr_bp, depth = cumul_depth_mm, core_num) %>% 
  mutate(depth = depth / 10)

# make our ek_v1 df compatible so we can plot all together 
ek_v1_cln <- ek_v1 %>% 
  select(year = yr_bp, depth = cumul_depth_mm, core_num) %>% 
  mutate(depth = depth / 10)

all_df <- rbind(ams_df %>% rename(core_num = ams_sample), ek_v2_cln) #%>% rbind(ek_v1_cln)

ggplot(all_df, aes(year, depth, colour = core_num)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x, fullrange=TRUE, linetype = "dashed", size= 0.5) +
  geom_abline(slope = -summary(scatter)$coeff[2], intercept = -summary(scatter)$coeff[1]) +
  scale_y_continuous(trans = 'reverse') +
  xlab("Mid-Point Cal Year (BP)") +
  ylab("Core Depth (cm)")

ggsave('figs/sed_rates_V1_V2_ekmans.png', width = 6, height = 4.5)


# Look at Long Core Varve Thickness Now 

v1 <- read.csv("../Varve_counting/226/226_VarveThickness_5_no_disturbed.csv") %>% 
  select(ssy = Sus.Sed.Yield..T.km.2.a.1., depth_cm = Cumulative.Depth..cm., year_bp = Interpolated.Year, mm = Couplet.Thickness..mm., stdep = Standardized.Departure) %>% 
  as_tsibble(index = year_bp) %>% 
  #filter(year > 50) %>% # keep only two data points at begining of record
  filter(mm != 0) %>% 
  fill_gaps() %>% 
  mutate(ASD = ((mm-mean(mm, na.rm = T))/sd(mm, na.rm = T))) %>% 
  mutate(mvAvg = rollapply(ASD, width = 16, by = 1, FUN = mean, na.rm = T, fill = NA, align = "center", partial = F)) %>% # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
  #mutate(wma = ma(ASD, order = 5)) %>% 
  # mutate(wt = pracma::movavg(ASD, 2, "w")) %>% 
  mutate(ma4 = pracma::movavg(ASD, 4, "w")) %>% 
  mutate(ma16 = pracma::movavg(mvAvg, 24, "w")) %>% 
 pivot_longer(c(ma4, ma16)) 

ggplot(v1, aes(year_bp, depth_cm)) +
  geom_point() +
  scale_y_continuous(trans = 'reverse') +
  geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x, fullrange=F, linetype = "dashed", size= 0.5) +
  xlab("Estimated Year (BP)") +
  ylab("Core Depth (cm)")

# ggsave('../Varve_counting/figs/v1_cumulative_depth_vs_estimated_year.png', width = 6, height = 4.5)

v1_lm <- lm(depth_cm ~ 0 + year_bp, data = v1)

v1_sed_rate <- summary(v1_lm)$coeff[1] # filtered to less than 3 sd's and forced through origin 

v1_sed_rate * 10

v1_plot <- ggplot(v1, aes(x = depth_cm)) +
  geom_line(aes(y = ASD), alpha = 1/4) +
  geom_point(aes(x = v1_C14$depth_cm, y = -1.5)) +
  geom_text(aes(x = v1_C14$depth_cm, y = -1.75), label = "1819-1899 BP", vjust = 1) +
  geom_line(aes(y = mvAvg), label = "20 Year Moving Average") +
  xlim(0, 200) +
  ylab("Varve Thickness ASD") +
  ggtitle("V1") +
  scale_x_continuous(sec.axis=sec_axis(trans=~ . * (v1_C14$year/v1_C14$depth_cm), name="Estimated Year (Linear Interpolation)"))+ # scale sec y axis based on c14
  geom_smooth(aes(depth_cm, ASD), method = "lm", formula = y ~ 1, colour = "red", se=F, linetype="dashed")
v1_plot

# this one not sure how we got years, recreate process below 
v2 <- read.csv("../Varve_counting/224/224_varveCounting_4_no_disturbed.csv") %>% 
  select(ssy = SSY , depth_cm = Cumulative.Depth..mm., year_bp = Interpolated.Year..BP., mm = Couplet.Thickness..mm., stdep = Standardized.departure) %>% 
  filter(year_bp != 676) %>% # remove outlier causing big spike only backed up by one point
  as_tsibble(index = year_bp) %>% 
  tsibble::fill_gaps() %>% 
  mutate(ASD = ((mm-mean(mm, na.rm = T))/sd(mm, na.rm = T)),
         depth_cm = depth_cm /10,
         mvAvg = rollapply(ASD, width = 16, by = 1, FUN = mean, na.rm = T, fill = NA, align = "center", partial = F)  # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
         )  

# try to recreate years using raw varve thickness 

v2 <- read.csv("data/long_cores/224_varveCounting_4_RAW.csv") %>% 
  select(year_bp_excel = Interpolated.Year..BP., # this was the original interpolated years by Alex 2017, need to recreate this 
         core_depth_excel = Cumulative.Depth..mm., 
         lyr_mm = Couplet.Thickness..mm., # original layer thicknesses by alex 2017
         notes = Notes) 

v2.ct <- v2[-1,] # dont include top disturbed bit in stats calculations 

plot(rownames(v2.ct), v2.ct$lyr_mm)

summary(v2.ct$lyr_mm)

v2.sd <- sd(v2.ct$lyr_mm)

v2.mean <- mean(v2.ct$lyr_mm)

v2.fvl <- v2.mean + (3*v2.sd) # rm couplets with thicknesses greater than 3 std above the mean 

sd_flag <- v2$lyr_mm > v2.fvl

manual_flags <- nchar(v2$notes) > 0 # any cell that has text is disturbed or flood or tephra (1)

v2$lyr_flag <- sd_flag | manual_flags

sum(v2$lyr_flag, na.rm = TRUE) # how many flags ? 


# apply above stats to V2, essentially treats every varve as 1 yr, unless flaged, 
# then take the moving average (100 yr) to calculate the time elapsed for the flagged layer.
v2 <- v2 %>% 
  mutate(
    core_depth = cumsum(lyr_mm), # same as cumsum_excel but recalc for completeness 
    # need this so we dont take into account floods / disturbed when doing the moving avg
    lyr_mm_cln = case_when(
      lyr_flag == F ~ lyr_mm
    ),
    avg_sed_rate = rollapply(lyr_mm_cln, width = 100, by = 1, FUN = mean, na.rm = T, align = "center", partial = T), # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
    interp_yr = round(lyr_mm / avg_sed_rate),
    year_for_add = case_when(
      lyr_flag == T ~ interp_yr,
      TRUE ~ 1
    ),
    year_BP = cumsum(year_for_add)
  )

v2_mod <- data.frame(
  year_BP = seq(1:2000)
) %>% 
  left_join(v2)
  
# see how the new interpolated years compares
v2_mod %>% 
  rename(new_yr = year_BP,
         old_yr = year_bp_excel) %>%
  pivot_longer(c(new_yr, old_yr)) %>% 
ggplot(aes(value, core_depth, colour = name)) +
  geom_point() +
  scale_y_continuous(trans = 'reverse') +
  geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x, fullrange=F, linetype = "dashed", size= 0.5) +
  xlab("Estimated Year (BP)") +
  ylab("Core Depth (cm)")

# see old sed rate
v2_lm_old <- lm(core_depth_excel ~ 0 + year_bp_excel, data = v2)

v2_sed_rate_old <- summary(v2_lm_old)$coeff[1] # filtered to less than 3 sd's and forced through origin 

v2_sed_rate_old

# see new sed rate 
v2_lm <- lm(core_depth ~ 0 + year_BP, data = v2_mod)

v2_sed_rate <- summary(v2_lm)$coeff[1] # filtered to less than 3 sd's and forced through origin 

v2_sed_rate 

v2 <- v2 %>% 
  mutate(mod_mm = predict(v2_lm, .))




ggsave('../Varve_counting/figs/v2_cumulative_depth_vs_estimated_year.png', width = 6, height = 4.5)




v2_plot <- ggplot(v2) + 
  geom_line(aes(depth_cm, ASD), alpha = 1/4) +
  geom_line(aes(x = depth_cm, y = mvAvg), label = "20 Year Moving Average") +
  geom_point(aes(x = v2_C14$depth_cm, y = -1.5)) +
  geom_text(aes(x = v2_C14$depth_cm, y = -1.75), label = "1895-2045 BP", vjust = 1) +
  xlim(0, 290) +
  ylab("Varve Thickness ASD") +
  ggtitle("V2") +
  scale_x_continuous(sec.axis=sec_axis(trans=~ . * (v2_C14$year/v2_C14$depth_cm), name="Estimated Year (Linear Interpolation)"))+ # scale sec y axis based on c14
  geom_smooth(aes(depth_cm, ASD), method = "lm", formula = y ~ 1, colour = "red", se=F, linetype="dashed")
v2_plot

p <- grid.arrange(v1_plot, v2_plot, nrow=2)

ggsave("figs/V1_V2_varvethickness_vs_depth_show_C14_est_yr.png", p,  width = 11, height = 6)

# plot by alex interpolated year

v1 <- read.csv("226/226_VarveThickness_5_no_disturbed.csv") %>% 
  select(ssy = Sus.Sed.Yield..T.km.2.a.1., depth_cm = Cumulative.Depth..cm., year_bp = Interpolated.Year, mm = Couplet.Thickness..mm., stdep = Standardized.Departure) %>% 
  as_tsibble(index = year_bp) %>% 
  #filter(year > 50) %>% # keep only two data points at begining of record
  filter(mm != 0) %>% 
  fill_gaps() %>% 
  mutate(ASD = ((mm-mean(mm, na.rm = T))/sd(mm, na.rm = T))) %>% 
  mutate(mvAvg = rollapply(ASD, width = 16, by = 1, FUN = mean, na.rm = T, fill = NA, align = "center", partial = T)) %>% # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
  #mutate(wma = ma(ASD, order = 5)) %>% 
  # mutate(wt = pracma::movavg(ASD, 2, "w")) %>% 
  mutate(ma4 = pracma::movavg(ASD, 4, "w")) %>% 
  mutate(ma16 = pracma::movavg(mvAvg, 24, "w")) #%>% 
  pivot_longer(c(ma4, ma16)) 

ggplot(v1, aes(x = year)) +
  geom_line(aes(y = ASD), alpha = 1/4) +
  #geom_line(aes(y = mvAvg), label = "20 Year Moving Average") +
  #geom_line(aes(y = value, color = name)) +
  # geom_line(aes(y = wma)) +
  geom_line(aes(y = ma16)) +
  geom_smooth(aes(year, ASD), method = "lm", formula = y ~ 1, colour = "red", se=F, linetype="dashed")


ggplot

ggplot(v1_wma, aes(year, wma)) +
  geom_line()

v1_roll10 <- v1 %>% 
  mutate(mvAvg = rollapply(ASD, width = 10, by = 1, FUN = mean, na.rm = T, fill = NA, align = "center", partial = F)) %>% # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
  filter(is.na(mvAvg) == F)# %>% 
  filter(year > 50) #%>% 
  mutate(mvAvg = ifelse(is.na(mm), NA, mvAvg))
  
v1_roll5 <- v1 %>% 
  mutate(mvAvg = rollapply(ASD, width = 5, by = 1, FUN = mean, na.rm = T, fill = NA, align = "center", partial = F)) %>% # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
  filter(is.na(mvAvg) == F)# %>% 
filter(year > 50) #%>% 
mutate(mvAvg = ifelse(is.na(mm), NA, mvAvg))
  
v1_roll25 <- v1 %>% 
  mutate(mvAvg = rollapply(ASD, width = 25, by = 5, FUN = mean, na.rm = T, fill = NA, align = "center", partial = F)) %>% 
  filter(is.na(mvAvg) == F) %>% 
  filter(year > 50) #%>% 
mutate(mvAvg = ifelse(is.na(mm), NA, mvAvg))

v1plot.5 <- ggplot(v1) + 
  geom_point(aes(year, ASD), alpha = 1/6) +
  geom_path(inherit.aes = FALSE, data = v1_roll5, aes(year, mvAvg, group =1), na.rm = T) +
  xlim(0, 2000) +
  # ylim(-4, 4) +
  #ylab(bquote('Suspended Sediment Yeild '~Mg·km^-2·a^-1)) +
  ylab("Standardized Accumulation") +
  ggtitle("5 Year Moving Average 1 Year Step") +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_smooth(aes(year, ASD), method = "lm", formula = y ~ 1, colour = "red", se=F, linetype="dashed")
#geom_smooth(aes(year, ssy), method = "loess", se = FALSE) 

v1plot <- ggplot(v1) + 
  geom_point(aes(year, ASD), alpha = 1/6) +
  geom_path(inherit.aes = FALSE, data = v1_roll10, aes(year, mvAvg, group =1), na.rm = T) +
  xlim(0, 2000) +
 # ylim(0, 30) +
  #ylab(bquote('Suspended Sediment Yeild '~Mg·km^-2·a^-1)) +
  ylab("Standardized Accumulation") +
  ggtitle("10 Year Moving Average 1 Year Step") +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_smooth(aes(year, ASD), method = "lm", formula = y ~ 1, colour = "red", se=F, linetype="dashed")
#geom_smooth(aes(year, ssy), method = "loess", se = FALSE) 

v1plot2 <- ggplot(v1) + 
  geom_point(aes(year, ASD), alpha = 1/6) +
  geom_path(inherit.aes = FALSE, data = v1_roll25, aes(year, mvAvg, group =1), na.rm = T) +
  xlim(0, 2000) +
  ggtitle("25 Year Window With 5 year Step") +
  #ylim(0, 30) +
  #ylab(bquote('Suspended Sediment Yeild '~Mg·km^-2·a^-1)) +
  ylab("Standardized Accumulation") +
  xlab("Year (BP)") +
  geom_smooth(aes(year, ASD), method = "lm", formula = y ~ 1, colour = "red", se=F, linetype="dashed")
#geom_smooth(aes(year, ssy), method = "loess", se = FALSE) 

  
ptest <- grid.arrange(v1plot.5, v1plot, v1plot2, nrow=3)
  
ggsave("figs/V1_Point_25to10to5yrmovingaveragecomparison.png", ptest,  width = 10, height = 8)

  




v2 <- read.csv("224/224_varveCounting_4_no_disturbed.csv") %>% 
  select(ssy = SSY, year = Interpolated.Year..BP., mm = Couplet.Thickness..mm., stdep = Standardized.departure) %>% 
  filter(year != 676) %>% # remove outlier causing big spike only backed up by one point
  as_tsibble(index = year) %>% 
  tsibble::fill_gaps() %>% 
  mutate(ASD = ((mm-mean(mm, na.rm = T))/sd(mm, na.rm = T)))  

v2_roll10 <- v2 %>% 
  mutate(mvAvg = rollapply(ASD, width = 10, by = 1, FUN = mean, na.rm = T, fill = NA, align = "center", partial = F)) %>% # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
  filter(is.na(mvAvg) == F)# %>% 
filter(year > 50) #%>% 
mutate(mvAvg = ifelse(is.na(mm), NA, mvAvg))

v2_roll5 <- v2 %>% 
  mutate(mvAvg = rollapply(ASD, width = 5, by = 1, FUN = mean, na.rm = T, fill = NA, align = "center", partial = F)) %>% # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
  filter(is.na(mvAvg) == F)# %>% 
filter(year > 50) #%>% 
mutate(mvAvg = ifelse(is.na(mm), NA, mvAvg))

v2_roll25 <- v2 %>% 
  mutate(mvAvg = rollapply(ASD, width = 25, by = 5, FUN = mean, na.rm = T, fill = NA, align = "center", partial = F)) %>% 
  filter(is.na(mvAvg) == F) %>% 
  filter(year > 50) #%>% 
mutate(mvAvg = ifelse(is.na(mm), NA, mvAvg))

v2_mvAvg <- v2 %>% 
  mutate(mvAvg = rollapply(ssy, width = 25, by = 5, FUN = mean, na.rm = T, fill = NA, align = "center", partial = 3)) %>% 
  filter(is.na(mvAvg) == F)# %>% 
  mutate(mvAvg = ifelse(is.na(mm), NA, mvAvg))

  v2plot.5 <- ggplot(v2) + 
    geom_point(aes(year, ASD), alpha = 1/6) +
    geom_path(inherit.aes = FALSE, data = v2_roll5, aes(year, mvAvg, group =1), na.rm = T) +
    xlim(0, 2000) +
    # ylim(-4, 4) +
    #ylab(bquote('Suspended Sediment Yeild '~Mg·km^-2·a^-1)) +
    ylab("Standardized Accumulation") +
    ggtitle("5 Year Moving Average 1 Year Step") +
    theme(axis.title.x=element_blank(), 
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    geom_smooth(aes(year, ASD), method = "lm", formula = y ~ 1, colour = "red", se=F, linetype="dashed")
  #geom_smooth(aes(year, ssy), method = "loess", se = FALSE) 
  
  v2plot <- ggplot(v2) + 
    geom_point(aes(year, ASD), alpha = 1/6) +
    geom_path(inherit.aes = FALSE, data = v2_roll10, aes(year, mvAvg, group =1), na.rm = T) +
    xlim(0, 2000) +
    # ylim(0, 30) +
    #ylab(bquote('Suspended Sediment Yeild '~Mg·km^-2·a^-1)) +
    ylab("Standardized Accumulation") +
    ggtitle("10 Year Moving Average 1 Year Step") +
    theme(axis.title.x=element_blank(), 
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    geom_smooth(aes(year, ASD), method = "lm", formula = y ~ 1, colour = "red", se=F, linetype="dashed")
  #geom_smooth(aes(year, ssy), method = "loess", se = FALSE) 
  
  v2plot2 <- ggplot(v2) + 
    geom_point(aes(year, ASD), alpha = 1/6) +
    geom_path(inherit.aes = FALSE, data = v2_roll25, aes(year, mvAvg, group =1), na.rm = T) +
    xlim(0, 2000) +
    ggtitle("25 Year Window With 5 year Step") +
    #ylim(0, 30) +
    #ylab(bquote('Suspended Sediment Yeild '~Mg·km^-2·a^-1)) +
    ylab("Standardized Accumulation") +
    xlab("Year (BP)") +
    geom_smooth(aes(year, ASD), method = "lm", formula = y ~ 1, colour = "red", se=F, linetype="dashed")
  #geom_smooth(aes(year, ssy), method = "loess", se = FALSE) 
  
  
  ptest2 <- grid.arrange(v2plot.5, v2plot, v2plot2, nrow=3)
  
  ggsave("figs/V2_Points_25to10to5yrmovingaveragecomparison.png", ptest2,  width = 10, height = 8)


p <- grid.arrange(v1plot, v2plot, nrow=2)

ggsave("figs/v1_v2_SSY_25yrAvg_By5yr.png", p,  width = 8.5, height = 5)


plot_ly(v2, x= ~year, y = ~ssy)

plot_ly(v1, x= ~year, y = ~ssy)
