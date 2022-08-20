library(tidyverse)

gs <- readxl::read_xlsx('data/Sediment/Grain Size/CB17_GrainSize_Ekmans.xlsx', sheet = 2) |> 
  select(ID = `Core Number`,
         dist = `Distance Frm Delta (km)`,
         depth = Depth,
         basin = `sub-basin`,
         D10 = `Dx (10)`,
         D50 = `Dx (50)`,
         D90 = `Dx (90)`
         ) |> 
  pivot_longer(D10:D90, names_to = 'grain_size_stat', values_to = 'gs_value')

ggplot(gs, aes(x = dist, y = gs_value, color = grain_size_stat)) +
  geom_point() 
  
varve <- read_csv('data/ekman/EK_varveCounting_orig_long_analysis.csv') |> 
  select(
    ID = core_num,
    thickness = layer_thickness_mm
  ) |> 
  group_by(ID) |> 
  summarise(Lam_Thickness = mean(thickness, na.rm = T),
            n_samples = n()) |> 
  mutate(ID = as.numeric(gsub(".*?([0-9]+).*" , "\\1", ID)))
         
loi <- readxl::read_xlsx('data/Sediment/LOI/LOI_Cariboo_EkmanBulkSamples.xlsx', sheet = 5, skip = 1) |> 
  select(
    ID = `Core #`,
    # dist = `Distance Frm Delta (km)`,
    # depth = Depth,
    # loi_initial = `Initial Results`,
    LOI = `LOI (%) Second Round`,
    loi_notes = Notes
  ) |> 
  mutate(ID = as.numeric(gsub(".*?([0-9]+).*" , "\\1", ID)))



all <- left_join(gs, varve) |> 
  left_join(loi, by = 'ID') |> 
  pivot_longer(c(gs_value, Lam_Thickness, LOI))

dat_text <- data.frame(
  label = c("A", "B", "C"),
  name   = c('gs_value', 'Lam_Thickness', 'LOI'),
  x     = c(20, 27.5, 25),
  y     = c(4, 4, 4.5)
)

p <- ggplot(all) +
  geom_line(aes(x = dist, y = value, colour = grain_size_stat)) +
  geom_point(aes(x = dist, y = value, colour = grain_size_stat, shape = basin)) +
  facet_wrap(~name, 
             nrow = 3,
             scales = "free_y", 
             labeller = as_labeller(
               c(gs_value = "Grain Size (µm)", Lam_Thickness = "Avg. Lam. (mm)", LOI = "OM (%)")), 
             strip.position = "left") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  xlab('Distance Down Lake (km)') +
  labs(y = NULL) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  geom_text(
    data    = dat_text,
    mapping = aes(x = Inf, y = Inf, label = label),
    hjust   = 1.5,
    vjust   = 1.5
  )
  

p

saveRDS(p, file = 'figs/ekman/ekman_seds.rds')
  

