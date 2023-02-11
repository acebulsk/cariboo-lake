# to address reviewer 2 (Byron Steinman) comment to strengthen chronology claims 
# we will try some R packages for age - depth models
# Byrons comment:
# The extrapolated 14C ages should include uncertainty based on use of the 2sigma 
# high and low ages (i.e. 3 age models - one based on the median 14C age, and 1 for 
# each 2sig hi and lo). Another option is to use age model software like bacon or 
# bchron and show the output of this along with the varve chronologies. 
# I think this would strengthen the paper tremendously and make this statement conclusive.
# also good write up in https://dewey.dunnington.ca/post/2018/comparing-approaches-to-age-depth-modelling-in-r/

# Some notes after trying below script: bchron seems to error out when we use
# the entire varve record... better results with just 100 ish varve samples.
# probably just stick to the error bars on the original graph and not use the below

library(tidyverse)

library(Bchron)

u_ottawa_cal <- 'intcal13' # cal reference used on the u ottawa ams analysis sheet they also combined OxCal v4.2.4 too but not sure how to use both with this r package. 

standard_yr_bp <- 1950 # the year used in the literature as BP datum
yr_core_ce <- 2017 # this is the year we took the core
yr_core_bp <- standard_yr_bp-yr_core_ce

v1_c14_depth <- 347 # depth of woody material after adjustment (cm)
v1_c14 <- 1913 # C14 bp (1950)
v1_c14_sd <- 21 # +/- yr error 

v1_c14_depth <- (286 + 294) / 2 # avg depth for combined V2 sample after adjustment
v2_c14 <- 2020 # C14 bp (1950)
v2_c14_sd <- 28 # +/- yr error 

# counting error was not possible to attribute since there were no clear marker 
# varves or tephras so we use the average of reported varve counting uncertainties 
# in the literature 0.7 - 6 % from @Menenous2008 and @Birlo2022 respectively

counting_error <- 0.03 # fraction of a year

long_cores <- readRDS('data/long_cores/varve_thickness_v1_v2_working.RDS')

# v1 --------------

v1_long <- long_cores |> 
  filter(core == 'V1') |> 
  mutate(sample_id = NA,
         depth = core_depth / 10,
         age_14C = year_BP, # A vector of ages provided in years before 1950.
         age_error = abs(year_BP * counting_error),
         thickness = lyr_mm_cln / 10) |> 
  select(sample_id:thickness) |> 
  mutate(calCurves = 'normal') |> 
  filter(is.na(thickness) == F) 

# ams data ------------------------


ams_chron_v1 <- tribble(
  ~sample_id, ~depth, ~age_14C, ~age_error, ~thickness, ~calCurves,
  "top", 0, yr_core_bp, 0, 0, 'normal',
  "V1‐C-347", 347, v1_c14, v1_c14_sd, 1, u_ottawa_cal
) 

core_chron <- tribble(
  ~sample_id, ~depth, ~age_14C, ~age_error, ~thickness, ~calCurves,
"top", 0, yr_core_bp, 0, 0, 'normal') |> 
  # rbind(v1_eks) |> 
  rbind(v1_long) 

ams_chron_out <- Bchronology(
  ages = ams_chron_v1$age_14C,
  ageSds = ams_chron_v1$age_error,
  positions = ams_chron_v1$depth,
  calCurves = ams_chron_v1$calCurves,
  positionThickness = ams_chron_v1$thickness,
)

# core_chron_out <- Bchronology(
#   ages = core_chron$age_14C,
#   ageSds = core_chron$age_error,
#   positions = core_chron$depth,
#   calCurves = core_chron$calCurves,
#   positionThickness = core_chron$thickness,
# )
# 
# saveRDS(core_chron_out, 'data/long_cores/chronology/v1_bcrhon_chronology.RDS')

core_chron_out <- readRDS('data/long_cores/chronology/v1_bcrhon_chronology.RDS')

# saveRDS(core_chron_out, 'data/long_cores/chronology/v1_bcrhon_chronology_sample100.RDS')

core_chron_out <- readRDS('data/long_cores/chronology/v1_bcrhon_chronology.RDS')

# the method to plot two of the bchrons on one plot is shown here http://andrewcparnell.github.io/Bchron/articles/Bchron.html
alpha <- 0.95

chronRange <- data.frame(
  chronLow = apply(core_chron_out$thetaPredict, 2, "quantile", probs = (1 - alpha) / 2),
  chronMed = apply(core_chron_out$thetaPredict, 2, "quantile", probs = 0.5),
  chronHigh = apply(core_chron_out$thetaPredict, 2, "quantile", probs = 1 - (1 - alpha) / 2),
  positions = core_chron_out$predictPositions
)

ageGrid <- with(chronRange, seq(min(chronLow), max(chronHigh),
                                length = nrow(chronRange)
))
chronRangeSwap <- data.frame(
  Age = ageGrid,
  positionLow = with(chronRange, approx(chronLow, positions,
                                        xout = ageGrid,
                                        rule = 2
  )$y),
  Position = with(chronRange, approx(chronMed, positions,
                                     xout = ageGrid,
                                     rule = 2
  )$y),
  positionHigh = with(chronRange, approx(chronHigh, positions,
                                         xout = ageGrid,
                                         rule = 2
  )$y),
  Date = "Bchron",
  densities = NA,
  height = NA
)

plot(ams_chron_out, 
     ageScale = 'bp',
     dateLabels = FALSE, 
     chronTransparency = 0.3) + 
  geom_ribbon(
    data = chronRangeSwap,
    aes_string(
      x = "Age",
      ymin = "positionLow",
      ymax = "positionHigh"),
    colour = "red",
    fill = "red",
    alpha = 0.3
  ) +
  ylab('Core Depth (cm)') +
  xlab('Age (cal yr BP)')
  
ggsave('figs/chronology/v1_bchron_varve_ams_compare_allvarvesamples_error.png', width = 5, height = 4)  

# v2 ----------------------

ams_chron_v2 <- tribble(
  ~sample_id, ~depth, ~age_14C, ~age_error, ~thickness, ~calCurves,
  "top", 0, yr_core_bp, 0, 0, 'normal',
  "V2‐C-286", v1_c14_depth, v2_c14, v2_c14_sd, 1, u_ottawa_cal
)

v2_long <- long_cores |> 
  filter(core == 'V2') |> 
  mutate(sample_id = NA,
         depth = core_depth / 10,
         age_14C = year_BP, # A vector of ages provided in years before 1950.
         age_error = year_BP * counting_error,
         thickness = lyr_mm_cln / 10) |> 
  select(sample_id:thickness) |> 
  mutate(calCurves = 'normal') |> 
  filter(is.na(thickness) == F) |> 
  sample_n(100)

core_chron <- tribble(
  ~sample_id, ~depth, ~age_14C, ~age_error, ~thickness, ~calCurves,
  "top", 0, yr_core_bp, 0, 0, 'normal') |> 
  # rbind(v1_eks) |> 
  rbind(v2_long)

ams_chron_out <- Bchronology(
  ages = ams_chron_v2$age_14C,
  ageSds = ams_chron_v2$age_error,
  positions = ams_chron_v2$depth,
  calCurves = ams_chron_v2$calCurves,
  positionThickness = ams_chron_v2$thickness,
)

# core_chron_out <- Bchronology(
#   ages = core_chron$age_14C,
#   ageSds = core_chron$age_error,
#   positions = core_chron$depth,
#   calCurves = core_chron$calCurves,
#   positionThickness = core_chron$thickness,
# )

# saveRDS(core_chron_out, 'data/long_cores/chronology/v1_bcrhon_chronology.RDS')

# saveRDS(core_chron_out, 'data/long_cores/chronology/v2_bcrhon_chronology_sample100.RDS')

core_chron_out <- readRDS('data/long_cores/chronology/v2_bcrhon_chronology.RDS')

# the method to plot two of the bchrons on one plot is shown here http://andrewcparnell.github.io/Bchron/articles/Bchron.html
alpha <- 0.95

chronRange <- data.frame(
  chronLow = apply(core_chron_out$thetaPredict, 2, "quantile", probs = (1 - alpha) / 2),
  chronMed = apply(core_chron_out$thetaPredict, 2, "quantile", probs = 0.5),
  chronHigh = apply(core_chron_out$thetaPredict, 2, "quantile", probs = 1 - (1 - alpha) / 2),
  positions = core_chron_out$predictPositions
)

ageGrid <- with(chronRange, seq(min(chronLow), max(chronHigh),
                                length = nrow(chronRange)
))
chronRangeSwap <- data.frame(
  Age = ageGrid,
  positionLow = with(chronRange, approx(chronLow, positions,
                                        xout = ageGrid,
                                        rule = 2
  )$y),
  Position = with(chronRange, approx(chronMed, positions,
                                     xout = ageGrid,
                                     rule = 2
  )$y),
  positionHigh = with(chronRange, approx(chronHigh, positions,
                                         xout = ageGrid,
                                         rule = 2
  )$y),
  Date = "Bchron",
  densities = NA,
  height = NA
)

plot(ams_chron_out, 
     ageScale = 'bp',
     dateLabels = FALSE, 
     chronTransparency = 0.3) + 
  geom_ribbon(
    data = chronRangeSwap,
    aes_string(
      x = "Age",
      ymin = "positionLow",
      ymax = "positionHigh"),
    colour = "red",
    fill = "red",
    alpha = 0.3
  ) +
  ylab('Core Depth (cm)') +
  xlab('Age (cal yr BP)')

ggsave('figs/chronology/v1_bchron_varve_ams_compare.png', width = 4, height = 3)  

ams_df_out <- rbind(
  ams_chron_v1 |> mutate(core = 'V1'),
  ams_chron_v2 |> mutate(core = 'V2')
)


