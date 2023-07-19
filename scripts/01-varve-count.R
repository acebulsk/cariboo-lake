library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(tidyr)
library(zoo) 
library(tsibble)
library(pracma)

options(ggplot2.discrete.colour= c("#E69F00", "#000000", "#56B4E9"))


ams_meta <- readRDS('data/long_cores/chronology/long_core_ams_meta.rds')

standard_yr_bp <- 1950 # the year used in the literature as BP datum
yr_core_ce <- 2017 # this is the year we took the core
yr_core_bp <- standard_yr_bp-yr_core_ce

#### Ekmans ####

df.ek <- read.csv('data/ekman/EK_varveCounting_orig_long_analysis.csv')

# Bring In Raw Ekman Data 9 original counting by alex MSc times 
ek <- df.ek %>%  
  select(1:6) %>% 
  mutate(core_num = gsub('K', '', core_num)) |> 
  group_by(core_num) %>% 
  mutate(
    year_CE = Year + 1, # error in initial sheet, cores were collected in july 2017 so surface is 2017
    year_bp = standard_yr_bp - year_CE, # standard is BP equals before 1950
    sd = sd(layer_thickness_mm, na.rm = T),
            mean = mean(layer_thickness_mm, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  mutate(sd_flag = case_when(
    layer_thickness_mm > 3*sd ~ T,
    TRUE ~ F
  )) |> 
  select(-c(img_J_numb:Year))

# Lets look at just EK13 first, closest core to V2, need to discount the 222 cm date
#filter out sd_flags  doesnt really make a difference to overall model fit 

ek13 <- ek %>%
  filter(core_num == "E13", 
         sd_flag == F)

ggplot(ek13, aes(year_bp, cumul_depth_mm)) +
  geom_point() +
  scale_y_continuous(trans = 'reverse') +
  geom_smooth(method = 'lm', se = F, fullrange=TRUE, linetype = "dashed", size= 0.5) 
  

ek13_lm <- lm(cumul_depth_mm ~ 0 + year_bp, data = ek13)

ek13_sed_rate <- summary(ek13_lm)$coeff[1] # filtered to less than 3 sd's and forced through origin 

ek13_sed_rate 

# Look at multiple Ekman sed rates close by 

prox_v1 <- c('E11', 'E12') # ekmans close to V2 in order of proximity 
prox_v2 <- c('E13') # ekmans close to V2 in order of proximity

# ekman cores close to v1 
ek_v1 <- ek %>%  
  filter(core_num %in% prox_v1,
         sd_flag == F)

saveRDS(ek_v1, 'data/ekman/ekman_11_12_v1_proximal_select.rds')

ggplot(ek_v1, aes(year_bp, cumul_depth_mm, colour = core_num)) +
  geom_point() +
  scale_y_continuous(trans = 'reverse') 
  
# ekman cores close to v2

ek_v2 <- ek %>%  
  filter(core_num %in% prox_v2,
         sd_flag == F)

ggplot(ek_v2, aes(year_bp, cumul_depth_mm/10, colour = core_num)) +
  geom_point() +
  scale_y_continuous(trans = 'reverse') +
  geom_smooth(method = 'lm', se = F, fullrange=TRUE, linetype = "dashed", size= 0.5) +
  # xlab("Couplet Number") +
  ylab("Core Depth (cm)")

#ggsave('figs/ekman_sed_rate_near_V2.png', width = 6, height = 4.5)

#### age depth model and c14 meta ####

v1_lm <- readRDS('data/long_cores/chronology/v1_c14_age_depth_model.rds')
v2_lm <- readRDS('data/long_cores/chronology/v2_c14_age_depth_model.rds')

ams_meta <- readRDS('data/long_cores/chronology/long_core_ams_meta.rds')

standard_yr_bp <- 1950 # the year used in the literature as BP datum
yr_core_ce <- 2017 # this is the year we took the core
yr_core_bp <- standard_yr_bp-yr_core_ce

ggplot(ams_meta, aes(median_age, depth, colour = core)) +
  geom_point() +
  geom_errorbarh(data = subset(ams_meta, ams_cal_se != 0), aes(xmin=median_age-ams_cal_se, xmax=median_age+ams_cal_se), width=.1) +
  scale_y_continuous(trans = 'reverse') +
  geom_smooth(method = 'lm', linetype = "dashed", size= 0.5) +
  # geom_abline(slope = -summary(scatter)$coeff[2], intercept = -summary(scatter)$coeff[1]) +
  xlab("Age (cal yr BP)") +
  ylab("Core Depth (cm)")

ggsave('figs/ams_cal_sed_rates.png', width = 6, height = 4.5)


vibro_sed_rates <- ams_meta %>% 
  group_by(core) %>% 
  do(model = lm(depth ~ median_age, data = .)) %>% 
  mutate(sed_rates = summary(model)$coeff[2] * 10) %>% # cm to mm
  select(-model)

# make our ek_v2 df compatible so we can plot all together 
ek_v2_cln <- ek_v2 %>% 
  select(year_bp, depth = cumul_depth_mm, `Core ID` = core_num) %>% 
  mutate(depth = depth / 10)

# make our ek_v1 df compatible so we can plot all together 
ek_v1_cln <- ek_v1 %>% 
  select(year_bp, depth = cumul_depth_mm, `Core ID` = core_num) %>% 
  mutate(depth = depth / 10)

all_df <- rbind(ams_meta %>% 
                  rename(`Core ID` = core) %>% 
                  select(year_bp = median_age, depth, `Core ID`), 
                  ek_v2_cln)

ggplot(all_df, aes(year_bp, depth, colour = `Core ID`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, fullrange=T, linetype = "dashed", size= 0.5) +
  # geom_abline(slope = -summary(scatter)$coeff[2], intercept = -summary(scatter)$coeff[1]) +
  scale_y_continuous(trans = 'reverse') +
  xlab("Estimated Year (BP)") +
  ylab("Core Depth (cm)") +
  theme_bw() +
  scale_color_brewer(palette = 'Set2')
  

# ggsave('journal-submission/markdown/figs/sed_rates_V1_V2_ekmans.png', width = 6, height = 4.5)


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

v1$sd_flag <- sd_flag

manual_flags <- v1$notes %in% c('disturbed', 'Disturbed')

# we need to define the layers considered to be turbidites 

v1$turbidite_flag <- !manual_flags & sd_flag

v1$turbidite_flag[751] <- T

# this is used to only add years for the disturbed layers and not the turbs

v1$disturbed_flag <- manual_flags | sd_flag & !v1$turbidite_flag

v1$lyr_flag <- v1$disturbed_flag | v1$turbidite_flag

sum(v1$turbidite_flag, na.rm = TRUE) # how many flags ? 
sum(v1$disturbed_flag, na.rm = TRUE) # how many flags ? 

# apply above stats to v1, essentially treats every varve as 1 yr, unless flaged, 
# then take the moving average (100 yr) to calculate the time elapsed for the flagged layer.
v1 <- v1 %>% 
  mutate(
    core_depth = cumsum(lyr_mm), # same as cumsum_excel but recalc for completeness 
    # need this so we dont take into account floods / disturbed when doing the moving avg
    lyr_mm_cln = case_when(
      lyr_flag == F ~ lyr_mm
    ),
    lyr_mm_no_turbs = case_when(
      turbidite_flag == F ~ lyr_mm,
      TRUE ~ 0 # need this so cumsum doesnt break
    ),
    core_depth_no_turb = cumsum(lyr_mm_no_turbs), # need this for age depth model
    depth = core_depth_no_turb/10, # this is to pass on to our predict function which takes col names depth
    avg_sed_rate = rollapply(lyr_mm_cln, width = 30, by = 1, FUN = mean, na.rm = T, align = "center", partial = T), # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
    interp_yr = round(lyr_mm / avg_sed_rate),
    year_for_add = case_when(
      disturbed_flag == T ~ interp_yr,
      turbidite_flag == T ~ 0,
      TRUE ~ 1
    ),
    year_b4_2017 = cumsum(year_for_add),
    year_CE = yr_core_ce - year_b4_2017,
    year_bp = standard_yr_bp-year_CE,
    year_bp_ams = predict(v1_lm,cur_data()),
    year_ce_ams = standard_yr_bp - year_bp_ams
  ) |> 
  select(core_depth, core_depth_no_turb, year_bp, year_CE, year_bp_ams, year_ce_ams, ends_with('flag'), lyr_mm, lyr_mm_cln, avg_sed_rate)

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

v1_turbidite <- v1 %>% 
  filter(turbidite_flag == T) 

v1_mod <- data.frame(
  year_bp = seq(from = min(v1$year_bp), to = max(v1$year_bp), by = 1)
) %>% 
  left_join(v1) 

saveRDS(v1_mod, 'data/long_cores/v1_226_processed.rds')

# see how the new interpolated years compares
# v1_mod %>% 
#   rename(new_yr = median_age,
#          old_yr = median_age_excel) %>%
#   pivot_longer(c(new_yr, old_yr)) %>% 
#   ggplot(aes(value, core_depth, colour = name)) +
#   geom_point() +
#   scale_y_continuous(trans = 'reverse') +
#   geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x, fullrange=T, linetype = "dashed", size= 0.5) +
#   xlab("Estimated Year (BP)") +
#   ylab("Core Depth (cm)")

v1_mod %>% 
  filter(lyr_flag == F) %>% 
ggplot(aes(year_ce_ams, lyr_mm)) +geom_point()

# ggplotly()

# see old sed rate
# v1_lm_old <- lm(core_depth_excel ~ 0 + median_age_excel, data = v1)

# v1_sed_rate_old <- summary(v1_lm_old)$coeff[1] # filtered to less than 3 sd's and forced through origin 
# 
# v1_sed_rate_old

# see new sed rate 
v1_varve_lm <- lm(core_depth_no_turb ~ 0 + year_bp, data = v1_mod)

v1_sed_rate <- summary(v1_varve_lm)$coeff[1] # filtered to less than 3 sd's and forced through origin 

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

disturbed_flag <- v2$notes %in% fltr_notes

# we need to define the layers considered to be turbidites 

turbidite_flag <- v2$notes %in% c('flood', 'Flood', '')

v2$turbidite_flag <- turbidite_flag & sd_flag

# this is used to only add years for the disturbed layers and not the turbs

v2$disturbed_flag <- disturbed_flag | sd_flag & !v2$turbidite_flag

v2$lyr_flag <- v2$disturbed_flag | v2$turbidite_flag

v2$sd_flag <- sd_flag

sum(v2$lyr_flag, na.rm = TRUE) # how many flags ? 
sum(v2$turbidite_flag, na.rm = TRUE) # how many flags ? 
sum(v2$disturbed_flag, na.rm = TRUE) # how many flags ? 

# apply above stats to V2, essentially treats every varve as 1 yr, unless flaged, 
# then take the moving average (100 yr) to calculate the time elapsed for the flagged layer.
v2 <- v2 %>% 
  mutate(
    core_depth = cumsum(lyr_mm), # same as cumsum_excel but recalc for completeness 
    # need this so we dont take into account floods / disturbed when doing the moving avg
    lyr_mm_cln = case_when(
      lyr_flag == F ~ lyr_mm
    ),
    lyr_mm_no_turbs = case_when(
      turbidite_flag == F ~ lyr_mm,
      TRUE ~ 0 # need this so cumsum doesnt break
    ),
    core_depth_no_turb = cumsum(lyr_mm_no_turbs), # need this for age depth model
    depth = core_depth_no_turb/10, # this is to pass on to our predict function which takes col names depth
    avg_sed_rate = rollapply(lyr_mm_cln, width = 30, by = 1, FUN = mean, na.rm = T, align = "center", partial = T), # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
    interp_yr = round(lyr_mm / avg_sed_rate),
    year_for_add = case_when(
      disturbed_flag == T ~ interp_yr,
      turbidite_flag == T ~ 0,
      TRUE ~ 1
    ),
    year_b4_2017 = cumsum(year_for_add),
    year_CE = yr_core_ce - year_b4_2017,
   year_bp = standard_yr_bp-year_CE,
    year_bp_ams = predict(v2_lm,cur_data()),
    year_ce_ams = standard_yr_bp - year_bp_ams
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
  mutate(metric = 'varve_thickness') 

saveRDS(varve_stats, 'data/long_cores/core_stats.rds')

write.csv(varve_stats, 'data/long_cores/core_stats.csv')


v2$lyr_mm_stdep <- (v2$lyr_mm - v2.mean.fltr)/v2.sd.fltr

v2 <- v2 |> 
  select(names(v1))

# create df of the floods 
v2_turbidite <- v2 %>% 
  filter(turbidite_flag == T
         )

turbidites <- rbind(
  v1_turbidite %>% mutate(core = "V1"),
  v2_turbidite %>% mutate(core = "V2")
) |> 
  select(core_depth, 
         core_depth_no_turb,
         year_bp, 
         lyr_mm_stdep,
         lyr_mm,
         sd_flag,
         core)

# add in turbidite not origionally measured because I thought it was distrubed
# the grain size for v1 measured at 12 cm is 7.5 mm thick, manually measured using 226 A+ .png

v1_12 <- data.frame(
  core_depth = 120,
  core_depth_no_turb = 120,
  year_bp = NA,
  lyr_mm_stdep = NA,
  lyr_mm = 7.5, 
  sd_flag = TRUE,
  core = 'V1'
)

# next we need an average sed rate for the top of V1 to calculate the time elapsed over 12 cm

v1_top_sed_rate <- mean(v1$avg_sed_rate[4]) # this is pretty close to the varve we are after it was just lumped into a large disturbed measurement

v1_12_year_bp_2017 <- v1_12$core_depth / v1_top_sed_rate

v1_12_year_bp <- standard_yr_bp - (yr_core_ce - v1_12_year_bp_2017)

v1_12$year_bp <- v1_12_year_bp

v1_vt_mean <- varve_stats$value[varve_stats$stat == 'mean_no_flood' & varve_stats$core == 'V1' & varve_stats$metric == 'varve_thickness']
v1_vt_sd <- varve_stats$value[varve_stats$stat == 'sd_no_flood' & varve_stats$core == 'V1' & varve_stats$metric == 'varve_thickness']

v1_12$lyr_mm_stdep = (v1_12$lyr_mm - v1_vt_mean)/v1_vt_sd

turb_join <- rbind(turbidites, v1_12) %>% 
  select(depth = core_depth,
         core_depth_no_turb,
         year_bp,
         stdep = lyr_mm_stdep,
         value = lyr_mm,
         core) %>% 
  mutate(metric = 'Varve Thickness',
         depth = depth / 10,
         core_depth_no_turb = core_depth_no_turb/10)

saveRDS(turb_join, 'data/long_cores/V1_V2_turbidite_deposits.rds')

v2_mod <- data.frame(
  year_bp = seq(from = min(v2$year_bp), to = max(v2$year_bp), by = 1)
) %>% 
  left_join(v2) 

saveRDS(v2_mod, 'data/long_cores/v2_224_processed.rds')

# see how the new interpolated years compares
# v2_mod %>% 
#   rename(new_yr = year_bp,
#          old_yr = year_bp_excel) %>%
#   pivot_longer(c(new_yr, old_yr)) %>% 
# ggplot(aes(value, core_depth, colour = name)) +
#   geom_point() +
#   scale_y_continuous(trans = 'reverse') +
#   geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x, fullrange=F, linetype = "dashed", size= 0.5) +
#   xlab("Estimated Year (BP)") +
#   ylab("Core Depth (cm)")

# v2_mod %>% 
#   filter(notes %in% c('', 'flood', 'Flood', 'TEPHRA')) %>% 
#   ggplot(aes(year_ce_ams, lyr_mm)) +geom_point()

# ggplotly()

# see old sed rate
# v2_lm_old <- lm(core_depth_excel ~ 0 + year_bp_excel, data = v2)
# 
# v2_sed_rate_old <- summary(v2_lm_old)$coeff[1] # filtered to less than 3 sd's and forced through origin 
# 
# v2_sed_rate_old

# see new sed rate 
v2_varve_lm <- lm(core_depth_no_turb ~ 0 + year_bp, data = v2_mod)

v2_sed_rate <- summary(v2_varve_lm)$coeff[1] # filtered to less than 3 sd's and forced through origin 

v2_sed_rate 

# v2 <- v2 %>% 
#   mutate(mod_mm = predict(v2_lm, .))



#### V1 + V2 Combined ####
v1_mod$core <- "V1"
v2_mod$core <- "V2"
v1_mod$date_type <- "varve"
v2_mod$date_type <- "varve"

comb <- rbind(v1_mod, v2_mod)

comb %>% 
  ggplot(aes(year_bp, core_depth_no_turb, colour = core, group = core)) +
  geom_point()+
  #geom_line(size = ) +
  scale_y_continuous(trans = 'reverse') +
  geom_smooth(method = 'lm', 
              orientation = "y", 
              formula = y ~ x + 0, 
              se = FALSE, 
              n = nrow(comb),
              method.args=list(offset=rep(100, nrow(comb))),
              fullrange = TRUE,
              linetype = "dashed", 
              size= 0.5) +
  xlab("Estimated Year (BP)") +
  ylab("Core Depth (cm)")

# ggplotly()

#ggsave('figs/longcore_cumulative_depth_vs_estimated_year.png', width = 6, height = 4.5)

# counting error was not possible to attribute since there were no clear marker 
# varves or tephras so we use the average of reported varve counting uncertainties 
# in the literature 0.7 - 6 % from @Menenous2008 and @Birlo2022 respectively
counting_error <- 0.1 # fraction of a year

# clean up ekman to compare

ek_v2_cln <- ek_v2 %>% 
  select(year = year_bp, depth = cumul_depth_mm, core = core_num) %>% 
  mutate(depth = depth / 10,
         varve_se = abs(year * counting_error))

# compare to AMS

ams_meta$date_type <- 'C14'

long_core_cln <- comb %>% 
  select(year = year_bp, depth = core_depth_no_turb, core, date_type) %>% 
  mutate(depth = depth / 10,
         varve_se = abs(year * counting_error))

all_df <- rbind(ams_meta |> mutate(varve_se = NA) |> select(year = median_age, depth, core, date_type,varve_se, ams_cal_se) , 
                long_core_cln |> mutate(ams_cal_se = NA)) %>%
  rbind(ek_v2_cln|> mutate(date_type = 'varve', ams_cal_se = NA))

plot_reg_line <- c('E13', 'V1', 'V2')

# need this to hack the triangle onto the ggplot
turb_join$size_test <- 5

turbidite_shape <- 3

ggplot(all_df, aes(year, depth, colour = core, shape = date_type)) +
  geom_point(size = 1) +
  geom_ribbon(data = subset(all_df, varve_se != 0), 
              aes(xmin = year-varve_se,
                  xmax = year+varve_se),
              alpha = 0.25, linetype = 'blank') +
  geom_point(data = subset(ams_meta, depth != 0),
             aes(median_age, depth),  shape = 17, size = 2) +
  geom_errorbarh(data = subset(all_df, ams_cal_se != 0), aes(xmin=year-ams_cal_se, xmax=year+ams_cal_se, height = 25)) +
  geom_smooth(data=subset(all_df,date_type == 'C14' | core == 'E13'),
              aes(year,depth,color=core),
              method = 'lm', se = F, fullrange = T, formula = y ~ x, linetype = "dashed", size= 0.5) +
  geom_point(data = turb_join,
             aes(year_bp, core_depth_no_turb, size = 5),  shape = turbidite_shape) +
  scale_y_continuous(trans = 'reverse') +
  xlab("Estimated Age (cal yr BP)") +
  ylab("Core Depth (cm)") +
  theme_bw() +
  scale_color_manual(values = viridis::viridis(4)) +
  scale_shape_manual(values = c(17, 20))  +
  labs(shape = 'chronology', size = 'turbidite') +
  scale_size(labels = "")+
  guides(color = guide_legend(override.aes = list(shape = c(NA, NA, NA) ) ) ) # this removes the triangle from the core legend

varve_c14_df <- all_df |> 
  filter(core != 'E13')

ek_df <- all_df |> 
  filter(core == 'E13')

turb_df <- turb_join |> 
  select(depth, year = year_bp, core) |> 
  mutate(point_type = 'turbidite')

c14_df <- ams_meta |> 
  select(depth, year = median_age, core) |> 
  filter(depth != 0) |> 
  mutate(point_type = 'C14 sample')

turb_c14_df <- rbind(turb_df, c14_df)

main.plot <- ggplot(data = all_df, aes(year, depth, colour = core)) +
  geom_line(aes(linetype = date_type))  +
  geom_ribbon(aes(xmin = year-varve_se,
                  xmax = year+varve_se),
              alpha = 0.25, linetype = 'blank') +
  geom_point(data = turb_c14_df,
             aes(year, depth, shape = point_type),  size = 4) +
  geom_errorbarh(data = subset(all_df, ams_cal_se != 0), aes(xmin=year-ams_cal_se, xmax=year+ams_cal_se, height = 25)) +
  scale_y_continuous(trans = 'reverse') +
  xlab("Estimated Age (cal yr BP)") +
  ylab("Core Depth (cm)") +
  theme_bw() +
  # scale_color_manual(values = viridis::viridis(4)) +
  # scale_shape_manual(values = c(17, 3))  +
  labs(linetype = 'chronology', shape = 'sample', colour = 'core')

inset.plot <- ggplot(data = all_df, aes(year, depth, colour = core)) +
  geom_line(aes(linetype = date_type))  +
  geom_ribbon(aes(xmin = year-varve_se,
                  xmax = year+varve_se),
              alpha = 0.25, linetype = 'blank') +
  geom_point(data = turb_c14_df,
             aes(year, depth, shape = point_type),  size = 4) +
  # geom_errorbarh(data = subset(all_df, ams_cal_se != 0), aes(xmin=year-ams_cal_se, xmax=year+ams_cal_se, height = 25)) +
  xlab("Estimated Age (cal yr BP)") +
  ylab("Core Depth (cm)") +
  theme_bw() +
  # xlim(c(NA, 150)) +
  scale_y_continuous(trans = 'reverse') +
  coord_cartesian(xlim=c(-75, 50), ylim=c(35, 0)) +
  # scale_color_manual(values = viridis::viridis(4)) +
  # scale_shape_manual(values = c(17, 3))  +
  labs(linetype = 'chronology', shape = 'sample', colour = 'core') +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

inset.plot

plot.with.inset <-
  cowplot::ggdraw() +
  cowplot::draw_plot(main.plot) +
  cowplot::draw_plot(inset.plot, 
                     x = .44,
                     y = .65, 
                     width = .33, 
                     height = .33)

plot.with.inset

ggsave('journal-submission/markdown/figs/longcore_cumulative_depth_vs_estimated_year_w_ams_and_varve.png', width = 6, height = 4.5)

#### Smoothing Functions ####
comb$core[comb$core == 'V1_varve'] = "V1"
comb$core[comb$core == 'V2_varve'] = "V2"

# gaus <- readRDS('data/long_cores/varve_thickness_v1_v2_working.RDS')

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
