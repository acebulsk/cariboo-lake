# script to find the median calibrated C14 date, run some other comparisons to the varve chronology
# and finally output a age depth model for each core as a lm

library(tidyverse)

library(Bchron)

surface_cal <- 'normal'
u_ottawa_cal <- 'intcal20' # cal reference used on the u ottawa ams analysis sheet they also combined OxCal v4.2.4 too but not sure how to use both with this r package. 

# we need the varve chronology data so we can attribute a depth to the AMS
# samples that has been adjusted to remove turbidites

v1_varve_depth_model <- readRDS('data/long_cores/v1_226_processed.rds') |> 
  select(core_depth, core_depth_no_turb) |> 
  mutate(
    core_depth = core_depth/10,
    core_depth_no_turb =core_depth_no_turb/10)

v2_varve_depth_model <- readRDS('data/long_cores/v2_224_processed.rds') |> 
  select(core_depth, core_depth_no_turb) |> 
  mutate(
    core_depth = core_depth/10,
    core_depth_no_turb = core_depth_no_turb/10)


# median is 50th percentile 
prob <- 0.5

# counting error was inferred from triplicate ekman counts to 5 cm depth
# in the literature 0.7 - 6 % from @Menenous2008 and @Birlo2022 respectively

counting_error <- 0.1 # fraction of a year

standard_yr_bp <- 1950 # the year used in the literature as BP datum
yr_core_ce <- 2017 # this is the year we took the core
yr_core_bp <- standard_yr_bp-yr_core_ce

v1_c14_depth_raw <- 347 # depth of woody material before adjustment (cm)
v1_c14_depth_adj_id <- which.min(abs(v1_varve_depth_model$core_depth - v1_c14_depth_raw))
v1_c14_depth <- v1_varve_depth_model$core_depth_no_turb[v1_c14_depth_adj_id] # adjusted depth with no turbidites

v1_c14 <- 1913 # C14 bp (1950)
v1_c14_sd <- 21 # +/- yr error 

v2_c14_depth_raw <- (286 + 294) / 2 # avg depth for combined V2 sample after adjustment (cm)
v2_c14_depth_adj_id <- which.min(abs(v2_varve_depth_model$core_depth - v2_c14_depth_raw))
v2_c14_depth <- v2_varve_depth_model$core_depth_no_turb[v2_c14_depth_adj_id] # adjusted depth with no turbidites

v2_c14 <- 2020 # C14 bp (1950)
v2_c14_sd <- 28 # +/- yr error 

both_cals <- BchronCalibrate(
  ages = c(v1_c14, v2_c14),
  ageSds = c(v1_c14_sd, v2_c14_sd),
  calCurves = rep(u_ottawa_cal,2),
  ids = c("V1‐C-347" , "V2‐C-286")
)

plot(both_cals)

v1_prob <- hdr(both_cals$`V1‐C-347`, 0.95)

v1_cal_range <- c(min(do.call(cbind, v1_prob)), max(do.call(cbind, v1_prob)))

paste('We are 95% sure the V1 c14 date is within', v1_cal_range[1], 'and', v1_cal_range[2])

v2_prob <- hdr(both_cals$`V2‐C-286`, 0.95)

v2_cal_range <- c(min(do.call(cbind, v2_prob)), max(do.call(cbind, v2_prob)))

paste('We are 95% sure the v2 c14 date is within', v2_cal_range[1], 'and', v2_cal_range[2])

# method to calculate the 50th percentile aka median of the PDF

# First create age samples for each date
# From: http://andrewcparnell.github.io/Bchron/articles/Bchron.html#credible-intervals

age_samples <- sampleAges(both_cals)

median_ages <- apply(age_samples, 2, quantile, prob = c(0.5))

paste('The closest age we have to the median (50th) percentile is', median_ages[1], 'and', median_ages[2], 'respectively')

paste('We are 95% sure the V1 c14 date is within', v1_cal_range[1], 'and', v1_cal_range[2])

paste('At V1 the mean of the 2.5th and 97.5th percentile is', mean(c(1820, 1918)))

paste('We are 95% sure the v2 c14 date is within', v2_cal_range[1], 'and', v2_cal_range[2])

paste('At V2 the mean of the 2.5th and 97.5th percentile is', mean(c(1895, 2043)))

# construct age table for manuscript

id <- c(NA, 'UOC‐5416', NA, 'UOC‐5767')
core <- c('V1', 'V1', 'V2', 'V2')
material <- c(NA, 'Wood', NA, 'Wood & Spruce Needle')
type <- c('Surface', '14C', 'Surface', '14C')
depth <- c(0, v1_c14_depth, 0, v2_c14_depth)
c_14_age <- c(NA, v1_c14, NA, v2_c14)
one_sig <- c(NA, v1_c14_sd, NA, v2_c14_sd)
age_range <- c(NA, paste0(v1_cal_range[1], '-', v1_cal_range[2]), NA, paste0(v2_cal_range[1], '-', v2_cal_range[2])) 
median_age <- c(yr_core_bp, median_ages[[1]], yr_core_bp,  median_ages[[2]])
cal_age_low <- c(NA, v1_cal_range[1], NA, v2_cal_range[1])
cal_age_hi <- c(NA, v1_cal_range[2], NA, v2_cal_range[2])

ams_meta <- data.frame(
  id,
  core,
  material,
  type,
  depth,
  c_14_age,
  one_sig,
  age_range,
  median_age,
  cal_age_low,
  cal_age_hi
) |> 
  mutate(ams_cal_se = replace_na((cal_age_hi-cal_age_low)/2, 0),
         year_ce = standard_yr_bp - median_age)

saveRDS(ams_meta, 'data/long_cores/chronology/long_core_ams_meta.rds')
write.csv(ams_meta, 'data/long_cores/chronology/long_core_ams_meta.csv')

# mod here for slight diff from manuscript table whrere steinman shows NA for c14 surface date but we need to have -67 to run the model properly.. 
ams_meta$c_14_age <-  c(yr_core_bp, v1_c14, yr_core_bp, v2_c14)
ams_meta$thickness <- c(0, 1, 0, 1)

#### what are the sedimentation rates from the C14 data ####

# this bchron method doesnt appear to work 

ams_chron_v1 <- ams_meta |>
  filter(core == 'V1') |>
  mutate(cal_curve = c(surface_cal, u_ottawa_cal))

ams_chron_v2 <- ams_meta |>
  filter(core == 'V2') |>
  mutate(cal_curve = c(surface_cal, u_ottawa_cal))
# 
# v1_ams_chron_out <- Bchronology(
#   ages = ams_chron_v1$c_14_age,
#   ageSds = replace_na(ams_chron_v1$one_sig, 0),
#   positions = ams_chron_v1$depth,
#   calCurves = ams_chron_v1$cal_curve,
#   positionThickness = ams_chron_v1$thickness,
# )
# 
# v2_ams_chron_out <- Bchronology(
#   ages = ams_chron_v2$c_14_age,
#   ageSds = replace_na(ams_chron_v2$one_sig, 0),
#   positions = ams_chron_v2$depth,
#   calCurves = ams_chron_v2$cal_curve,
#   positionThickness = ams_chron_v2$thickness,
# )
# 
# # median looks ok but upper and lower bounds are wonky 
# v1_acc_rate <- summary(v1_ams_chron_out,
#                     type = "acc_rate", useExisting = FALSE,
#                     probs = c(0.250, 0.5, 0.975)
# )

# NOTE we need to add yr_core_bp since we want to be using the age Before coring
# i.e. the number of years that have elapsed since depth 0
# v1

v1_rate_hi <- v1_c14_depth / (v1_cal_range[1] + abs(yr_core_bp)) * 10
v1_rate_low <- v1_c14_depth / (v1_cal_range[2] + abs(yr_core_bp)) * 10
v1_rate_med <- v1_c14_depth / (median_ages[1] + abs(yr_core_bp)) * 10

v1_rate_med
sd(c(v1_rate_hi, v1_rate_low, v1_rate_med))
(v1_rate_hi- v1_rate_low)/2

# v2

# NOTE we need to add yr_core_bp since we want to be using the age Before coring

v2_rate_hi <- v2_c14_depth / (1895 + abs(yr_core_bp)) * 10
v2_rate_low <- v2_c14_depth / (2043 + abs(yr_core_bp)) * 10
v2_rate_med <- v2_c14_depth / (1992 + abs(yr_core_bp)) * 10

v2_rate_med
sd(c(v2_rate_hi, v2_rate_low, v2_rate_med))
(v2_rate_hi- v2_rate_low)/2

#### what is the corresponding varve counting age of the AMS sample ####

# note here the varve chronology is already in BP == 1950 

v1_varve <- readRDS('data/long_cores/v1_226_processed.rds') |> 
  select(year_bp, core_depth_no_turb) |> 
  mutate(core_depth_no_turb = core_depth_no_turb / 10)

good_v1_yr_id <- which.min(abs(v1_varve$core_depth_no_turb - v1_c14_depth))

v1_varve_yr <- v1_varve$year_bp[good_v1_yr_id]

v1_varve_yr_se <- v1_varve_yr * counting_error

v2_varve <- readRDS('data/long_cores/v2_224_processed.rds') |> 
  select(year_bp, core_depth_no_turb) |> 
  mutate(core_depth_no_turb = core_depth_no_turb / 10)

good_v2_yr_id <- which.min(abs(v2_varve$core_depth_no_turb - v2_c14_depth))

v2_varve_yr <- v2_varve$year_bp[good_v2_yr_id]

v2_varve_yr_se <- v2_varve_yr * counting_error

paste('The corresponding varve estimate year is', 
      v1_varve_yr, 'and', v2_varve_yr, 
      'for v1 and v2 respectively. And we also infer a 10% error on these dates so our SE is', 
      v1_varve_yr_se, 'and', v2_varve_yr_se, 'respectively.')

#### what is the basal age of the core using the varve chronology ####

v1_varve_basal_yr <- max(v1_varve$year_bp) 
v2_varve_basal_yr <- max(v2_varve$year_bp) 

v1_varve_basal_yr_se <- v1_varve_basal_yr * counting_error
v2_varve_basal_yr_se <- v2_varve_basal_yr * counting_error

paste('The basal age of the cores using the varve chronologyis', 
      v1_varve_basal_yr, 'and', v2_varve_basal_yr, 
      'for v1 and v2 respectively. And we also infer a 10% error on these dates so our SE is', 
      v1_varve_basal_yr_se, 'and', v2_varve_basal_yr_se, 'respectively.')

#### what is the basal age of the core using the 14C chronology ####

# NOTE: here we need to subtract 67 to get back to yr before 1950

v1_basal_depth <- max(v1_varve$core_depth_no_turb, na.rm =T)*10

v1_c14_basal_yr_low <- (v1_basal_depth / v1_rate_low) + yr_core_bp
v1_c14_basal_yr_hi <- (v1_basal_depth / v1_rate_hi) + yr_core_bp
v1_c14_basal_yr_med <- (v1_basal_depth / v1_rate_med) + yr_core_bp

v2_basal_depth <- max(v2_varve$core_depth_no_turb, na.rm =T)*10
v2_c14_basal_yr_low <- (v2_basal_depth / v2_rate_low) + yr_core_bp
v2_c14_basal_yr_hi <- (v2_basal_depth / v2_rate_hi) + yr_core_bp
v2_c14_basal_yr_med <- (v2_basal_depth / v2_rate_med) + yr_core_bp

#### age depth model from the C14 dates #### 

v1_lm <- lm(median_age ~ depth, data = ams_chron_v1)

summary(v1_lm)$coeff[1]

v1_predict <- data.frame(depth = round(v1_basal_depth/10))

v1_predict$cal_yr <- predict.lm(v1_lm, v1_predict)

# our test looks good
ggplot(ams_chron_v1, aes(depth, median_age)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = F) + # same as lm(median_age ~ depth, data = ams_chron_v1) 
  geom_point(data = v1_predict, aes(x = depth, y = cal_yr))

v2_lm <- lm(median_age ~ depth, data = ams_chron_v2)

summary(v2_lm)

v2_predict <- data.frame(depth = round(v2_basal_depth/10))

v2_predict$cal_yr <- predict.lm(v2_lm, v2_predict)

ggplot(ams_chron_v2, aes(depth, median_age)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = F)  + # same as lm(median_age ~ depth, data = ams_chron_v1) 
  geom_point(data = v2_predict, aes(x = depth, y = cal_yr))

# both tests look good now output our age models as lm objects 

saveRDS(v1_lm, 'data/long_cores/chronology/v1_c14_age_depth_model.rds')
saveRDS(v2_lm, 'data/long_cores/chronology/v2_c14_age_depth_model.rds')
