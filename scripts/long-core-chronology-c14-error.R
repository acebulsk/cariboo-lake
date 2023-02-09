# script to find the median calibrated C14 date

library(tidyverse)

library(Bchron)

u_ottawa_cal <- 'intcal13'

# counting error was not possible to attribute since there were no clear marker 
# varves or tephras so we use the average of reported varve counting uncertainties 
# in the literature 0.7 - 6 % from @Menenous2008 and @Birlo2022 respectively

counting_error <- 0.03 # fraction of a year

standard_yr_bp <- 1950 # the year used in the literature as BP datum
yr_core_ce <- 2017 # this is the year we took the core
yr_core_bp <- standard_yr_bp-yr_core_ce

v1_c14_depth <- 347 # depth of woody material after adjustment (cm)
v1_c14 <- 1913 # C14 bp (1950)
v1_c14_sd <- 21 # +/- yr error 

v2_c14_depth <- (286 + 294) / 2 # avg depth for combined V2 sample after adjustment (cm)
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

v1_prob

paste('We are 95% sure the V1 c14 date is within 1820 and 1918')

v2_prob <- hdr(both_cals$`V2‐C-286`, 0.95)

v2_prob

paste('We are 95% sure the v2 c14 date is within 1895 and 2043')

v1_df <- data.frame(
  age = both_cals$`V1‐C-347`$ageGrid,
  den = both_cals$`V1‐C-347`$densities
) |> 
  arrange(den) |> 
  mutate(den_cumsum = cumsum(den),
         diff = abs(den_cumsum - prob))

prob <- 0.5

# method to calculate the 50th percentile aka median of the PDF
# adapted from https://github.com/andrewcparnell/Bchron/blob/master/R/hdr.R

# do V1

ag <- both_cals$`V1‐C-347`$ageGrid
de <- both_cals$`V1‐C-347`$densities

# Put the probabilities in order of density
o <- order(de)
cu <- cumsum(de[o])

# Find which ones are above the threshold
good_cu <- which.min(abs(cu - prob))
good_ag <- sort(ag[o][good_cu])

paste('At V1 the closest age we have to the median (50th) percentile is', good_ag)

paste('We are 95% sure the V1 c14 date is within 1820 and 1918')

paste('At V1 the mean of the 2.5th and 97.5th percentile is', mean(c(1820, 1918)))

# do V2

ag <- both_cals$`V2‐C-286`$ageGrid
de <- both_cals$`V2‐C-286`$densities

# Put the probabilities in order of density
o <- order(de)
cu <- cumsum(de[o])

# Find which ones are above the threshold
good_cu <- which.min(abs(cu - prob))
good_ag <- sort(ag[o][good_cu])

paste('At V1 the closest age we have to the median (50th) percentile is', good_ag)

paste('We are 95% sure the v2 c14 date is within 1895 and 2043')

paste('At V1 the mean of the 2.5th and 97.5th percentile is', mean(c(1895, 2043)))



# construct age table for manuscript

core <- c('V1', 'V1', 'V2', 'V2')
material <- c(NA, 'Wood', NA, 'Wood & Spruce Needle')
type <- c('Surface', '14C', 'Surface', '14C')
depth <- c(0, v1_c14_depth, 0, v2_c14_depth)
c_14_age <- c(NA, v1_c14, NA, v2_c14)
one_sig <- c(NA, v1_c14_sd, NA, v2_c14_sd)
# computed from the long-core-chronolgy-c14-error.R script
age_range <- c(NA, '1918‐1820', NA, '2043‐1895') 
median_age <- c(yr_core_bp, 1879, yr_core_bp, 1992)

tbl <- data.frame(
  core,
  material,
  type,
  depth,
  c_14_age,
  one_sig,
  age_range,
  median_age
)

saveRDS(tbl, 'data/long_cores/chronology/long_core_ams_meta.rds')

#### what are the sedimentation rates from the C14 data ####

# v1
v1_rate_hi <- v1_c14_depth / (1820 + abs(yr_core_bp)) * 10
v1_rate_low <- v1_c14_depth / (1918 + abs(yr_core_bp)) * 10
v1_rate_med <- v1_c14_depth / (1879 + abs(yr_core_bp)) * 10

v1_rate_med
sd(c(v1_rate_hi, v1_rate_low, v1_rate_med))
(v1_rate_hi- v1_rate_low)/2

# v2

v2_rate_hi <- v2_c14_depth / (1895 + abs(yr_core_bp)) * 10
v2_rate_low <- v2_c14_depth / (2043 + abs(yr_core_bp)) * 10
v2_rate_med <- v2_c14_depth / (1992 + abs(yr_core_bp)) * 10

v2_rate_med
sd(c(v2_rate_hi, v2_rate_low, v2_rate_med))
(v2_rate_hi- v2_rate_low)/2

#### what is the corresponding varve counting age of the AMS sample ####

v1_varve <- readRDS('data/long_cores/v1_226_processed.rds') |> 
  select(year_BP, core_depth) |> 
  mutate(core_depth = core_depth / 10)

good_v1_yr_id <- which.min(abs(v1_varve$core_depth - v1_c14_depth))

v1_varve_yr <- v1_varve$year_BP[good_v1_yr_id]

v1_varve_yr_se <- v1_varve_yr * counting_error

v2_varve <- readRDS('data/long_cores/v2_224_processed.rds') |> 
  select(year_BP, core_depth) |> 
  mutate(core_depth = core_depth / 10)

good_v2_yr_id <- which.min(abs(v2_varve$core_depth - v2_c14_depth))

v2_varve_yr <- v2_varve$year_BP[good_v2_yr_id]

v2_varve_yr_se <- v2_varve_yr * counting_error

paste('The corresponding varve estimate year is', 
      v1_varve_yr, 'and', v2_varve_yr, 
      'for v1 and v2 respectively. And we also infer a 3% error on these dates so our SE is', 
      v1_varve_yr_se, 'and', v2_varve_yr_se, 'respectively.')
