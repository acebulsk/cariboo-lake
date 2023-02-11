# can we derrive a varve counting measurement error based on the instantaneous deposits aka turbidites?

library(tidyverse)

# turbidites from varve thickness 

vt <- readRDS('data/long_cores/V1_V2_turbidite_deposits.rds') 

t_ids <- c('T1', 'T2', 'T3', 'T4', 'T5')

vt_v1 <- vt |> 
  filter(core == 'V1') |> 
  arrange(year_BP) |> 
  mutate(t_id = t_ids) |> 
  select(depth, core_depth_no_turb, year_BP, thickness_mm = value, t_id, core)

# here we are going to filter out some turbidites so we only keep the ones that
# also are seen in v1

vt_v2 <- vt |> 
  filter(core == 'V2') |> 
  arrange(year_BP) |> 
  # v1 basal depth is 1600 so we dont need to see the depths below here
  filter(year_BP < 1600) |> 
  # this seems to be a sidewall slump failure connected to the large event deposited above
  filter(depth != 231.4831) |> 
  # again just take the first event for the duplicate around 650 bp
  filter(depth != 112.0548) |> 
  # now we have just the five that occur in v1 too and can name them 
  mutate(t_id = t_ids) |> 
  rename(thickness_mm = value) |> 
  select(names(vt_v1))

vt_smry <- rbind(vt_v1, vt_v2) |> 
  group_by(t_id) |> 
  mutate(year_diff = diff(year_BP)) |> 
  dplyr::ungroup() |> 
  arrange(t_id) |> 
  select(core, t_id, year_BP)

tbl_out <- rbind(vt_v1, vt_v2) |> 
  select(year_BP, t_id, core) |> 
  pivot_wider(names_from = core, values_from = year_BP, names_prefix = 'year_BP_') |> 
  mutate(year_diff = year_BP_V1 - year_BP_V2) 

tbl_out$year_diff[5]/tbl_out$year_BP_V1[5]


               