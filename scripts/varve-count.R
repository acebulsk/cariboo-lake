library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(tidyr)
library(zoo) 
library(tsibble)
library(pracma)

#### Ekmans ####
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

#ggsave('figs/ekman_sed_rate_near_V2.png', width = 6, height = 4.5)

#### carbon dates ####
# A small twig from V1 at 347 cm results in a date of 1899-1819 cal BP. 
# A 4 cm long twig from V2 at 222 cm results in a date of 490-316 cal BP. 
# Since the first date from V2 was much younger than than expected, 
# a second sample from V2 was analyzed by combining a small twig at 286 cm and pine needle at 294 cm. 
# A date of 2045-1895 cal BP was determined.

# plot on depth 
v1_date <- (1899 + 1819) / 2 # mid point yr for v1 @ 347 cm

v1_C14 <- data.frame(depth_cm = 347, year = v1_date) # n = 1

v2b_depth <- (286 + 294) / 2 # avg depth for combined V2 sample

v2_date_b <- (2045 + 1895) / 2 # mid point yr for v2 @ 286 + 294 cm
v2_date_a <- (490 + 316) / 2 # mid point yr for v2 @ 222 cm

v2_C14 <- data.frame(depth_cm = 286, year = 1970) # n = 2

ams_df <- tibble(
  year_bp = c(0, v1_date, 0, v2_date_a, 0, v2_date_b),
  depth = c(0, 347, 0, 222, 0, v2b_depth),
  ams_sample = c('V1', 'V1', 'V2a', 'V2a', 'V2b', 'V2b')
) %>% 
  mutate(year_ce = 2017 - year_bp)

# Look at regression not through origin 

ams_scatter <- ams_df %>% 
  filter(depth != 0)

scatter <- lm(depth ~ year_bp, data = ams_scatter) # vibro ams point not through the origin 

ggplot(ams_df, aes(year_bp, depth, colour = ams_sample)) +
  geom_point() +
  scale_y_continuous(trans = 'reverse') +
  geom_smooth(method = 'lm', se = F, linetype = "dashed", size= 0.5) +
  # geom_abline(slope = -summary(scatter)$coeff[2], intercept = -summary(scatter)$coeff[1]) +
  xlab("Mid-Point Cal Year (BP)") +
  ylab("Core Depth (cm)")

ggsave('figs/ams_sed_rates.png', width = 6, height = 4.5)


vibro_sed_rates <- ams_df %>% 
  group_by(ams_sample) %>% 
  do(model = lm(depth ~ year_bp, data = .)) %>% 
  mutate(sed_rates = summary(model)$coeff[2] * 10) %>% # cm to mm
  select(-model)

# make our ek_v2 df compatible so we can plot all together 
ek_v2_cln <- ek_v2 %>% 
  select(year_bp = yr_bp, depth = cumul_depth_mm, core_num) %>% 
  mutate(depth = depth / 10)

# make our ek_v1 df compatible so we can plot all together 
ek_v1_cln <- ek_v1 %>% 
  select(year_bp = yr_bp, depth = cumul_depth_mm, core_num) %>% 
  mutate(depth = depth / 10)

all_df <- rbind(ams_df %>% 
                  rename(core_num = ams_sample) %>% 
                  select(year_bp, depth, core_num), 
                  ek_v2_cln)

ggplot(all_df, aes(year_bp, depth, colour = core_num)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x, fullrange=T, linetype = "dashed", size= 0.5) +
  # geom_abline(slope = -summary(scatter)$coeff[2], intercept = -summary(scatter)$coeff[1]) +
  scale_y_continuous(trans = 'reverse') +
  xlab("Estimated Year (BP)") +
  ylab("Core Depth (cm)")

ggsave('figs/sed_rates_V1_V2_ekmans.png', width = 6, height = 4.5)


# Look at Long Core Varve Thickness Now 

#### V1 ####

v1 <- read.csv("data/long_cores/226_VarveThickness_5_varveCalc_RAW.csv") %>% 
  select(year_bp_excel = Interpolated.Year, # this was the original interpolated years by Alex 2017, need to recreate this 
         core_depth_excel = Cumulative.Depth..cm., 
         lyr_mm = Couplet.Thickness..cm., # original layer thicknesses by alex 2017
         notes = Notes) %>% 
  mutate(core_depth_excel = core_depth_excel * 10,
         lyr_mm = lyr_mm * 10) %>% 
  slice(1:1141) # dont include last lone varve or disturbed, ending with last sequence of multiple measured varves

v1.ct <- v1[-1,] # dont include top disturbed bit in stats calculations 

plot(rownames(v1.ct), v1.ct$lyr_mm)

summary(v1.ct$lyr_mm)

v1.sd <- v1 %>% 
  filter(!notes %in% c('disturbed', 'Disturbed')) %>% 
  summarise(sd(lyr_mm, na.rm = T)) %>% 
  as.numeric()

v1.mean <- v1 %>% 
  filter(!notes %in% c('disturbed', 'Disturbed')) %>% 
  summarise(mean(lyr_mm, na.rm = T)) %>% 
  as.numeric()

v1.fvl <- v1.mean + (6*v1.sd) # rm couplets with thicknesses greater than 3 std above the mean 

sd_flag <- v1$lyr_mm > v1.fvl

manual_flags <- v1$notes %in% c('disturbed', 'Disturbed')

# manual_flags <- nchar(v1$notes) > 3 # any cell that has more than 3 chars is disturbed or flood or tephra (1)

v1$lyr_flag <- sd_flag | manual_flags

v1$sd_flag <- sd_flag

sum(v1$lyr_flag, na.rm = TRUE) # how many flags ? 


# apply above stats to v1, essentially treats every varve as 1 yr, unless flaged, 
# then take the moving average (100 yr) to calculate the time elapsed for the flagged layer.
v1 <- v1 %>% 
  mutate(
    core_depth = cumsum(lyr_mm), # same as cumsum_excel but recalc for completeness 
    # need this so we dont take into account floods / disturbed when doing the moving avg
    lyr_mm_cln = case_when(
      lyr_flag == F ~ lyr_mm
    ),
    avg_sed_rate = rollapply(lyr_mm_cln, width = 30, by = 1, FUN = mean, na.rm = T, align = "center", partial = T), # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
    interp_yr = round(lyr_mm / avg_sed_rate),
    year_for_add = case_when(
      lyr_flag == T ~ interp_yr,
      TRUE ~ 1
    ),
    year_BP = cumsum(year_for_add),
    year_CE = 2017 - year_BP,
    year_bp_lin_interp = core_depth * ((v1_C14$year)/(v1_C14$depth_cm*10)),
    year_ce_lin_interp = 2017 - year_bp_lin_interp
  )

v1.sd.fltr <- sd(v1$lyr_mm_cln, na.rm = T)
v1.mean.fltr <- mean(v1$lyr_mm_cln, na.rm = T)

v1_stats <- data.frame(
  stat = c(
    'mean', 
    'mean_no_flood',
    'sd',
    'sd_no_flood'
  ),
  value = c(
    v1.mean,
    v1.mean.fltr,
    v1.sd,
    v1.sd.fltr
  ),
  core = c(rep('V1', 4))
)

v1$lyr_mm_stdep <- (v1$lyr_mm - v1.mean.fltr)/v1.sd.fltr

# create df of the floods 
v1_751 <- v1[751,] # find flood at 229.0 cm which is in the grain size analysis
v1_turbidite <- v1 %>% 
  filter(!notes %in% c('disturbed', 'Disturbed'), 
         sd_flag == T) %>% 
  rbind(v1_751)

v1_mod <- data.frame(
  year_BP = seq(1:1643)
) %>% 
  left_join(v1) 

# see how the new interpolated years compares
v1_mod %>% 
  rename(new_yr = year_BP,
         old_yr = year_bp_excel) %>%
  pivot_longer(c(new_yr, old_yr)) %>% 
  ggplot(aes(value, core_depth, colour = name)) +
  geom_point() +
  scale_y_continuous(trans = 'reverse') +
  geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x, fullrange=F, linetype = "dashed", size= 0.5) +
  xlab("Estimated Year (BP)") +
  ylab("Core Depth (cm)")

v1_mod %>% 
  filter(!notes %in% c('disturbed', 'Disturbed')) %>% 
ggplot(aes(year_ce_lin_interp, lyr_mm)) +geom_point()

ggplotly()

# see old sed rate
v1_lm_old <- lm(core_depth_excel ~ 0 + year_bp_excel, data = v1)

v1_sed_rate_old <- summary(v1_lm_old)$coeff[1] # filtered to less than 3 sd's and forced through origin 

v1_sed_rate_old

# see new sed rate 
v1_lm <- lm(core_depth ~ 0 + year_BP, data = v1_mod)

v1_sed_rate <- summary(v1_lm)$coeff[1] # filtered to less than 3 sd's and forced through origin 

v1_sed_rate 

# v1 <- v1 %>% 
#   mutate(mod_mm = predict(v1_lm, .))

#### V2 ####

v2 <- read.csv("data/long_cores/224_varveCounting_4_RAW.csv") %>% 
  select(year_bp_excel = Interpolated.Year..BP., # this was the original interpolated years by Alex 2017, need to recreate this 
         core_depth_excel = Cumulative.Depth..mm., 
         lyr_mm = Couplet.Thickness..mm., # original layer thicknesses by alex 2017
         notes = Notes) 

v2.ct <- v2[-1,] # dont include top disturbed bit in stats calculations 

plot(rownames(v2.ct), v2.ct$lyr_mm)

summary(v2.ct$lyr_mm)

notes <- unique(v2$notes)
fltr_notes <- notes[!notes %in% c('', 'flood', 'Flood', 'TEPHRA')]

v2.sd <- v2 %>% 
  filter(!notes %in% fltr_notes) %>% 
  summarise(sd(lyr_mm, na.rm = T)) %>% 
  as.numeric()

v2.mean <- v2 %>% 
  filter(!notes %in% fltr_notes) %>% 
  summarise(mean(lyr_mm, na.rm = T)) %>% 
  as.numeric()

# v2.sd <- sd(v2.ct$lyr_mm)
# 
# v2.mean <- mean(v2.ct$lyr_mm)

v2.fvl <- v2.mean + (2*v2.sd) # rm couplets with thicknesses greater than 3 std above the mean

v2.fvl <- 3.9 # based off visual inspection

sd_flag <- v2$lyr_mm > v2.fvl

manual_flags <- v2$notes %in% fltr_notes

# manual_flags <- nchar(v2$notes) > 0 # any cell that has text is disturbed or flood or tephra (1)

v2$lyr_flag <- sd_flag | manual_flags

v2$sd_flag <- sd_flag

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
    avg_sed_rate = rollapply(lyr_mm_cln, width = 30, by = 1, FUN = mean, na.rm = T, align = "center", partial = T), # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
    interp_yr = round(lyr_mm / avg_sed_rate),
    year_for_add = case_when(
      lyr_flag == T ~ interp_yr,
      TRUE ~ 1
    ),
    year_BP = cumsum(year_for_add),
    year_CE = 2017 - year_BP,
    year_bp_lin_interp = core_depth * ((v2_C14$year)/(v2_C14$depth_cm*10)),
    year_ce_lin_interp = 2017 - year_bp_lin_interp
  )

v2.sd.fltr <- sd(v2$lyr_mm_cln, na.rm = T)
v2.mean.fltr <- mean(v2$lyr_mm_cln, na.rm = T)

v2_stats <- data.frame(
  stat = c(
    'mean', 
    'mean_no_flood',
    'sd',
    'sd_no_flood'
  ),
  value = c(
    v2.mean,
    v2.mean.fltr,
    v2.sd,
    v2.sd.fltr
  ),
  core = c(rep('V2', 4))
)

varve_stats <- rbind(v1_stats, v2_stats) %>% 
  mutate(metric = 'varve_thickness') %>% 
  saveRDS('data/long_cores/core_stats.rds')

varve_stats <- rbind(v1_stats, v2_stats) %>% 
  mutate(metric = 'varve_thickness') |> 
  write.csv('data/long_cores/core_stats.csv')


v2$lyr_mm_stdep <- (v2$lyr_mm - v2.mean.fltr)/v2.sd.fltr

# create df of the floods 
v2_turbidite <- v2 %>% 
  filter(sd_flag == T,
         notes %in% c('flood', 'Flood', ''),
         year_ce_lin_interp > 300 # only keep the ones we have grain size for and were visually inspected as floods 
         )

turbidites <- rbind(
  v1_turbidite %>% mutate(core = "V1"),
  v2_turbidite %>% mutate(core = "V2")
) %>% 
  saveRDS('data/long_cores/V1_V2_turbidite_deposits.rds')

v2_mod <- data.frame(
  year_BP = seq(1:1913)
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

v2_mod %>% 
  filter(notes %in% c('', 'flood', 'Flood', 'TEPHRA')) %>% 
  ggplot(aes(year_ce_lin_interp, lyr_mm)) +geom_point()

ggplotly()

# see old sed rate
v2_lm_old <- lm(core_depth_excel ~ 0 + year_bp_excel, data = v2)

v2_sed_rate_old <- summary(v2_lm_old)$coeff[1] # filtered to less than 3 sd's and forced through origin 

v2_sed_rate_old

# see new sed rate 
v2_lm <- lm(core_depth ~ 0 + year_BP, data = v2_mod)

v2_sed_rate <- summary(v2_lm)$coeff[1] # filtered to less than 3 sd's and forced through origin 

v2_sed_rate 

# v2 <- v2 %>% 
#   mutate(mod_mm = predict(v2_lm, .))



#### V1 + V2 Combined ####
v1_mod$core <- "V1_varve"
v2_mod$core <- "V2_varve"

comb <- rbind(v1_mod, v2_mod)

comb %>% 
  ggplot(aes(year_BP, core_depth, colour = core, group = core)) +
  geom_point()+
  #geom_line(size = ) +
  scale_y_continuous(trans = 'reverse') +
  geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x, fullrange=F, linetype = "dashed", size= 0.5) +
  xlab("Estimated Year (BP)") +
  ylab("Core Depth (cm)")


#ggsave('figs/longcore_cumulative_depth_vs_estimated_year.png', width = 6, height = 4.5)


# compare to AMS
ams_df_cln <- ams_df %>% 
  rename(core = ams_sample)

ams_df_cln$core[ams_df_cln$core == "V1"] <- "V1_ams"
ams_df_cln$core[ams_df_cln$core == "V2a"] <- "V2a_ams"
ams_df_cln$core[ams_df_cln$core == "V2b"] <- "V2b_ams"

long_core_cln <- comb %>% 
  select(year = year_BP, depth = core_depth, core) %>% 
  mutate(depth = depth / 10)

all_df <- rbind(ams_df_cln %>%  select(year = year_bp, depth, core), long_core_cln) %>%
  mutate(year_CE = 2017 - year)

ggplot(all_df, aes(year, depth, colour = core)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x, linetype = "dashed", size= 0.5) +
  #geom_abline(slope = -summary(scatter)$coeff[2], intercept = -summary(scatter)$coeff[1]) +
  scale_y_continuous(trans = 'reverse') +
  xlab("Estimated Year (BP)") +
  ylab("Core Depth (cm)")

ggsave('figs/longcore_cumulative_depth_vs_estimated_year_w_ams_and_varve.png', width = 6, height = 4.5)

#### Smoothing Functions ####
comb$core[comb$core == 'V1_varve'] = "V1"
comb$core[comb$core == 'V2_varve'] = "V2"

gaus <- readRDS('data/long_cores/varve_thickness_v1_v2_working.RDS')

gaus <- comb %>% 
  # filter(core == "V1") %>% 
  group_by(core) %>% 
  mutate(
    lyr_mm_stdep_fltr = 
      case_when(
        lyr_flag == F ~ lyr_mm_stdep
      ),
    lyr_mm_stdep_fill = imputeTS::na_interpolation(lyr_mm_stdep_fltr),
    ma_30 = rollapply(lyr_mm_stdep_fltr, width = 30, by = 1, FUN = mean, na.rm = T, align = "center", partial = T), # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
    smooth = smoother::smth(x = lyr_mm_stdep_fill, method = 'gaussian', window = 30)
  ) %>% 
  dplyr::ungroup() %>% 
  mutate(
    ma_30 = case_when(
      is.na(lyr_mm_stdep_fltr) ~ lyr_mm_stdep_fltr,
      TRUE ~ ma_30
    )
  )

saveRDS(gaus, 'data/long_cores/varve_thickness_v1_v2_working.RDS')

# remove some outlier points on the graph 

# gaus$ma_30[gaus$core_depth == 1029.454] = NA
# gaus$ma_30[gaus$core_depth == 1031.924] = NA
# gaus$ma_30[gaus$core_depth == 479.264 & gaus$core == "V2"] = NA
