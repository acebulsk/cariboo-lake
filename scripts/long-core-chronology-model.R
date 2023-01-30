# to address reviewer 2 (Byron Steinman) comment to strengthen chronology claims 
# we will try some R packages for age - depth models
# Byrons comment:
# The extrapolated 14C ages should include uncertainty based on use of the 2sigma 
# high and low ages (i.e. 3 age models - one based on the median 14C age, and 1 for 
# each 2sig hi and lo). Another option is to use age model software like bacon or 
# bchron and show the output of this along with the varve chronologies. 
# I think this would strengthen the paper tremendously and make this statement conclusive.
# also good write up in https://dewey.dunnington.ca/post/2018/comparing-approaches-to-age-depth-modelling-in-r/

library(tidyverse)

library(Bchron)

# counting error was not possible to attribute since there were no clear marker 
# varves or tephras so we use the average of reported varve counting uncertainties 
# in the literature 0.7 - 6 % from @Menenous2008 and @Birlo2022 respectively

counting_error <- 0.03

long_cores <- readRDS('data/long_cores/varve_thickness_v1_v2_working.RDS')

v1_long <- long_cores |> 
  filter(core == 'V1') |> 
  mutate(sample_id = NA,
         depth = core_depth / 10,
         age_14C = 1950 - year_CE, # A vector of ages provided in years before 1950.
         age_error = year_BP * counting_error,
         thickness = lyr_mm_cln / 10) |> 
  select(sample_id:thickness) |> 
  mutate(calCurves = 'normal') |> 
  filter(is.na(thickness) == F) |> 
  sample_n(100)

v1_eks <- readRDS('data/ekman/ekman_11_12_v1_proximal_select.rds') |> 
  mutate(cumul_depth_mm = cumul_depth_mm/10,
         layer_thickness_mm = layer_thickness_mm/10,
         sd = sd/10,
         age_error = yr_bp * counting_error
         ) |> 
  select(sample_id = core_num, 
         depth = cumul_depth_mm, 
         age_14C = yr_bp, 
         age_error = age_error,
         thickness = layer_thickness_mm) |> 
  mutate(calCurves = 'normal') |> 
  filter(is.na(thickness) == F)

ams_chron <- tribble(
  ~sample_id, ~depth, ~age_14C, ~age_error, ~thickness, ~calCurves,
  "top", 0, 0, 0, 0, 'normal',
  "V2‐C-347", 347, 1913, 21, 1, "intcal13"
) 

core_chron <- tribble(
  ~sample_id, ~depth, ~age_14C, ~age_error, ~thickness, ~calCurves,
"top", 0, 0, 0, 0, 'normal') |> 
  rbind(v1_eks) |> 
  rbind(v1_long)

ams_chron_out <- Bchronology(
  ages = ams_chron$age_14C,
  ageSds = ams_chron$age_error,
  positions = ams_chron$depth,
  calCurves = ams_chron$calCurves,
  positionThickness = ams_chron$thickness,
)

core_chron_out <- Bchronology(
  ages = core_chron$age_14C,
  ageSds = core_chron$age_error,
  positions = core_chron$depth,
  calCurves = core_chron$calCurves,
  positionThickness = core_chron$thickness,
)

saveRDS(core_chron_out, 'data/long_cores/chronology/v1_bcrhon_chronology.RDS')

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

plot(ams_chron_out, dateLabels = FALSE, chronTransparency = 0.3) + 
  geom_ribbon(
    data = chronRangeSwap,
    aes_string(
      x = "Age",
      ymin = "positionLow",
      ymax = "positionHigh"
    ),
    colour = "red",
    fill = "red",
    alpha = 0.3
  ) +
  ylab('Core Depth (cm)')

dates <- tribble(
  ~sample_id, ~depth, ~age_14C, ~age_error, ~thickness, ~calCurves,
  "V2‐C-286", 286, 2020, 28, 1, "intcal13",
  "V2‐C-etst", 100, 100, 28, 1, "intcal13"
)