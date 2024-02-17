# script to querey necessary data to submit to noaa ncei

library(tidyverse)

v1_lm <- readRDS('data/long_cores/chronology/v1_c14_age_depth_model.rds')
v2_lm <- readRDS('data/long_cores/chronology/v2_c14_age_depth_model.rds')

ams_meta <-readRDS('data/long_cores/chronology/long_core_ams_meta.rds')

# varve thickness ----

# long core with ekman already ----
vt <- readRDS('data/long_cores/varve_thickness_v1_v2_working.RDS') |> 
  mutate(
    depth_for_join = round(core_depth),
    core_depth = round(core_depth, digits = 1),
    core_depth_no_turb = round(core_depth_no_turb, digits = 1),
    year_bp_ams = round(year_bp_ams),
    year_bp_varve = round(year_bp),
    lyr_mm = round(lyr_mm, digits = 1)) |> 
  select(
         depth = core_depth,
         depth_no_turb = core_depth_no_turb,
         year_bp_ams,
         year_bp_varve,
         core_id = core,
         turbidite_flag,
         indiscernible_flag = disturbed_flag,
         layer_thickness = lyr_mm,
         core_sub_id,
         depth_for_join
         )

# organic matter ----

## long core ----
loi <- readRDS('data/Sediment/LOI/loi_v1_v2_working.RDS') |> 
  # filter(stdep < 3 & stdep > -3) |> 
  mutate(year_bp_ams = 1950 - year_ce_new,
         LOI = round(LOI, 3)) |> 
  select(core_id = core, depth_for_join = depth, organic_matter = LOI) |> 
  mutate(depth_for_join = depth_for_join * 10)

# extend the LOI measurements to the range they correspond to (i.e., were 2 cm cubes extracted so 20 mm, depth corresponds to the top of the sample)

sample_thickness <- 20 # mm 

### V1 ----
v1_sample_top_depths <- loi$depth_for_join[loi$core_id == 'V1']
# Create a new vector with 20 values in between each pair of consecutive values
new_vector <- c()
for (i in 1:(length(v1_sample_top_depths))) {
  new_values <- seq(v1_sample_top_depths[i], v1_sample_top_depths[i] + num_values,
                    by = 1)
  new_vector <- c(new_vector, new_values)
}

# Combine original and new vectors
v1_sample_depths <- new_vector |> unique()

v1_sample_depths <- data.frame(depth_for_join = v1_sample_depths,
                         core_id = 'V1')

### V2 ----
v2_sample_top_depths <- loi$depth_for_join[loi$core_id == 'V2']
# Create a new vector with 20 values in between each pair of consecutive values
new_vector <- c()
for (i in 1:(length(v2_sample_top_depths))) {
  new_values <- seq(v2_sample_top_depths[i], v2_sample_top_depths[i] + num_values,
                    by = 1)
  new_vector <- c(new_vector, new_values)
}

# Combine original and new vectors
v2_sample_depths <- new_vector |> unique()

v2_sample_depths <- data.frame(depth_for_join = v2_sample_depths,
                               core_id = 'V2')

loi_out <- rbind(v1_sample_depths, v2_sample_depths) |> 
  left_join(loi, by = c('core_id', 'depth_for_join')) |> 
  fill(organic_matter, .direction = 'down')

# grain size ----

## long core ----
gs <- readRDS("data/long_cores/gain_size_w_floods.rds") |> 
  filter(`Record Number` != 120) |> # skewed by floods either side |> 
  select(core_id = core, 
         depth_for_join = core_depth, 
         D10 = `Dx (10)`, 
         D25 = `Dx (25)`, 
         D50 = `Dx (50)`, 
         D75 = `Dx (75)`, 
         D90 = `Dx (90)`, 
         perc_clay = `Result In Range  (0.01,2) μm`, 
         perc_silt = `Result In Range  (2,63) μm`, 
         perc_sand = `Result In Range  (63,2000) μm`) |> 
  mutate(depth_for_join = depth_for_join * 10) |> 
  group_by(core_id, depth_for_join) |> 
  summarize(across(D10:perc_sand, mean)) |>   # averge double samples that were done to confirm smaller grain size at 259 cm
  ungroup()

# extend the LOI measurements to the range they correspond to (i.e., were 2 cm cubes extracted so 20 mm, depth corresponds to the top of the sample)

sample_thickness <- 20 # mm 

### V1 ----
v1_sample_top_depths <- gs$depth_for_join[gs$core_id == 'V1']
# Create a new vector with 20 values in between each pair of consecutive values
new_vector <- c()
for (i in 1:(length(v1_sample_top_depths))) {
  new_values <- seq(v1_sample_top_depths[i], v1_sample_top_depths[i] + num_values,
                    by = 1)
  new_vector <- c(new_vector, new_values)
}

# Combine original and new vectors
v1_sample_depths <- new_vector |> unique()

v1_sample_depths <- data.frame(depth_for_join = v1_sample_depths,
                               core_id = 'V1')

### V2 ----
v2_sample_top_depths <- gs$depth_for_join[gs$core_id == 'V2']
# Create a new vector with 20 values in between each pair of consecutive values
new_vector <- c()
for (i in 1:(length(v2_sample_top_depths))) {
  new_values <- seq(v2_sample_top_depths[i], v2_sample_top_depths[i] + num_values,
                    by = 1)
  new_vector <- c(new_vector, new_values)
}

# Combine original and new vectors
v2_sample_depths <- new_vector |> unique()

v2_sample_depths <- data.frame(depth_for_join = v2_sample_depths,
                               core_id = 'V2')

gs_out <- rbind(v1_sample_depths, v2_sample_depths) |> 
  left_join(gs, by = c('core_id', 'depth_for_join')) |> 
  fill(D10:perc_sand, .direction = 'down')

v1_data <- vt |> 
  filter(core_sub_id %in% c('E13-V1', 'V1')) |> 
  mutate(
    core_id_out = core_id,
    core_id = 'V1') |>
  left_join(loi_out, by = c('core_id', 'depth_for_join')) |> 
  left_join(gs_out, by = c('core_id', 'depth_for_join')) |> 
  select(depth:year_bp_varve,
         core_id = core_id_out,
         turbidite_flag:layer_thickness,
         organic_matter:perc_sand)

v2_data <- vt |> 
  filter(core_sub_id %in% c('E13-V2', 'V2')) |> 
  mutate(
    core_id_out = core_id,
    core_id = 'V2') |> 
  left_join(loi_out, by = c('core_id', 'depth_for_join')) |> 
  left_join(gs_out, by = c('core_id', 'depth_for_join')) |> 
  select(-core_sub_id, -depth_for_join, -core_id, core_id = core_id_out) |> 
  select(names(v1_data))


write.csv(v1_data, 'noaa-ncei-archive/noaa-ncei-archive-cariboo-longcore-v1.csv',
          row.names = F)
write.csv(v2_data, 'noaa-ncei-archive/noaa-ncei-archive-cariboo-longcore-v2.csv',
          row.names = F)
