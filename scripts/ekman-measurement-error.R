# what is the varve counting error - since we dont have triplicate cores in one
# location to get a counting error Joe suggested counting down to a fixed depth
# of 5 cm using cores between 4-8 km down lake which have similar sed rates

library(tidyverse)

ek_meta <- readxl::read_xlsx('data/Sediment/Grain Size/CB17_GrainSize_Ekmans.xlsx', sheet = 2) |> 
  select(core_num = `Core Number`,
         dist = `Distance Frm Delta (km)`) |> 
  distinct() |> 
  filter(dist > 4, 
         dist < 7) 

# lets look at just these subset since have similar sed rates core 15 did not
# have discernible coulets near the top so was skipped

ek_meta

# count is the number of couplets down from the surface to a consistent depth of 5 cm

ek_fix_depth <- data.frame(
  core = c(12, 13, 14),
  depth = c(5, 5, 5),
  count = c(10, 12, 12)
) 

saveRDS(ek_fix_depth, 'data/ekman/ek_counting_error_table_supplement.rds')

# now show the counting error stats 

mean_count <- mean(ek_fix_depth$count)
sd_count <- sd(ek_fix_depth$count)
percent_err <- sd_count/mean_count

# the percent error of our triplicate ekman count is ... 

percent_err * 100

# 10 Percent