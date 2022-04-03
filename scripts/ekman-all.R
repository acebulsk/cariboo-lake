library(tidyverse)

gs <- readxl::read_xlsx('data/Sediment/Grain Size/CB17_GrainSize_Ekmans.xlsx', sheet = 2) |> 
  select(ID = `Core Number`,
         dist = `Distance Frm Delta (km)`,
         depth = Depth,
         basin = `sub-basin`,
         `D50 (µm)` = `Dx (50)`)
  
varve <- read_csv('data/ekman/EK_varveCounting_orig_long_analysis.csv') |> 
  select(
    ID = core_num,
    thickness = layer_thickness_mm
  ) |> 
  group_by(ID) |> 
  summarise(`Lam. Thickness (mm)` = mean(thickness, na.rm = T),
            n_samples = n()) |> 
  mutate(ID = as.numeric(gsub(".*?([0-9]+).*" , "\\1", ID)))
         
loi <- readxl::read_xlsx('data/Sediment/LOI/LOI_Cariboo_EkmanBulkSamples.xlsx', sheet = 5, skip = 1) |> 
  select(
    ID = `Core #`,
    # dist = `Distance Frm Delta (km)`,
    # depth = Depth,
    # loi_initial = `Initial Results`,
    `LOI (%)` = `LOI (%) Second Round`,
    loi_notes = Notes
  ) |> 
  mutate(ID = as.numeric(gsub(".*?([0-9]+).*" , "\\1", ID)))



all <- left_join(gs, varve) |> 
  left_join(loi, by = 'ID') |> 
  pivot_longer(c(`D50 (µm)`, `Lam. Thickness (mm)`, `LOI (%)`))

p <- ggplot(all, aes(x = dist, y = value, colour = basin)) +
  geom_line() +
  geom_point() +
  facet_grid(rows = vars(name), scales = "free_y") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  xlab('Distance (km)')

saveRDS(p, file = 'figs/ekman/ekman_seds.rds')
  

