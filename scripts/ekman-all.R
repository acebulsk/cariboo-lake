library(tidyverse)
library(viridis)

# Data ####
gs <- readxl::read_xlsx('data/Sediment/Grain Size/CB17_GrainSize_Ekmans.xlsx', sheet = 2) |> 
  select(ID = `Core Number`,
         dist = `Distance Frm Delta (km)`,
         depth = Depth,
         basin = `sub-basin`,
         D10 = `Dx (10)`,
         D50 = `Dx (50)`,
         D90 = `Dx (90)`,
         Clay = `Clay  (0.01 - 2) μm`,
         Silt = `Silt  (2 - 63) μm`,
         Sand = `Sand  (63 - 2000) μm`
         ) 

ek_meta <- gs |> select(ID:basin) |> distinct()
  
varve <- read_csv('data/ekman/EK_varveCounting_orig_long_analysis.csv') |> 
  select(
    ID = core_num,
    thickness = layer_thickness_mm
  ) |> 
  group_by(ID) |> 
  summarise(Lam_Thickness = mean(thickness, na.rm = T)) |> 
  mutate(ID = as.numeric(gsub(".*?([0-9]+).*" , "\\1", ID))) |> 
  left_join(ek_meta, by = 'ID') 

# E13 age estimate and accumulation rate
e13 <- read_csv('data/ekman/EK_varveCounting_orig_long_analysis.csv') |> 
  filter(core_num == 'EK13') |> 
  mutate(cumul_depth_new = cumsum(layer_thickness_mm),
         yr_bp_new = row_number())

counting_error <- 0.1

mean_varve_thick <- mean(e13$layer_thickness_mm)
basaldepth <- max(e13$cumul_depth_new)
age_med <- max(e13$yr_bp_new)
age_hi <- age_med + (age_med * counting_error)
age_lo <- age_med - (age_med * counting_error)

sed_rate_hi <- basaldepth/age_hi
sed_rate_med <- basaldepth/age_med
sed_rate_lo <- basaldepth/age_lo

sed_rate_err <- (sed_rate_hi-sed_rate_lo)/2

# so same as just taking 10% of the avg acc rate

loi <- readxl::read_xlsx('data/Sediment/LOI/LOI_Cariboo_EkmanBulkSamples.xlsx', sheet = 5, skip = 1) |> 
  select(
    ID = `Core #`,
    # dist = `Distance Frm Delta (km)`,
    # depth = Depth,
    # loi_initial = `Initial Results`,
    LOI = `LOI (%) Second Round`,
    loi_notes = Notes
  ) |> 
  filter(is.na(ID) == F) |> 
  mutate(ID = as.numeric(gsub(".*?([0-9]+).*" , "\\1", ID))) |> 
  left_join(ek_meta, by = 'ID') 

# Plot ####
delta_shapes <- c(20, 15, 17)

gs_plot_grains <- 
  gs |> 
  pivot_longer(Clay:Sand, 
               names_to = 'grain_size_stat', 
               values_to = 'gs_value') |> 
  ggplot(aes(x = dist, 
             y = gs_value, 
             color = factor(grain_size_stat, levels = c("Clay", 
                                                                "Silt", 
                                                                "Sand")),
             shape = basin)) +
  geom_point(size = 2) +
  ylab('Percent (%)') +
  scale_shape_manual(guide = 'none', values = delta_shapes) +
  theme_bw() +
  theme(axis.title.x = element_blank(), legend.title = element_blank()) +
  scale_color_manual(values = viridis(3, option = 'C'))

gs_plot_grains
  
gs_plot_dist <- 
    gs |> 
    pivot_longer(D10:D90, 
                 names_to = 'grain_size_stat', 
                 values_to = 'gs_value') |> 
  ggplot(aes(x = dist, y = gs_value, color = grain_size_stat, shape = basin)) +
    geom_point(size = 2) +
    ylab('Grain Size (μm)') +
    scale_shape_manual(guide = 'none', values = delta_shapes) +
    theme_bw() +
    theme(axis.title.x = element_blank(), legend.title = element_blank()) +
  scale_color_manual(values = viridis(3, option = 'C'))

gs_plot_dist

varve_plot <- ggplot(varve, aes(x = dist, y = Lam_Thickness, shape = basin)) +
  geom_point(size = 2) +
  xlim(c(0.290,NA)) +
  ylab('Avg. Lam. (mm)') +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  scale_color_brewer(palette = 'Set2', name = '') +
  scale_shape_discrete(guide = 'none')

loi_plot <- ggplot(loi, aes(x = dist, y = LOI, shape = basin)) +
  geom_point(size = 2) +
  ylab('OM (%)') +
  xlab('Distance Down Lake (km)')+
  scale_color_brewer(palette = 'Set2', name = '') +
  scale_shape_manual(values = delta_shapes) +
  theme_bw()

p <- list(gs_plot_grains, gs_plot_dist, varve_plot, loi_plot)


cp <- cowplot::plot_grid(plotlist = p, nrow=length(p), 
                         # labels = LETTERS[seq( from = 1, to = length(p) )], 
                         labels = "AUTO",
                         label_y = 0.95,
                         label_x = .74,
                         align = 'v',
                         axis = 'tblr')

cp

cowplot::save_plot('sage-submission/figs/ekman_seds.jpg', plot = cp, base_width = 8.5, base_height = 6)

saveRDS(cp, file = 'figs/ekman/ekman_seds.rds')
  

