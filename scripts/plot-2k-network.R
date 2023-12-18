# This script brings in some 2k network paleoclimate data and I added some 
# glacier extent data to plot against Cariboo Lake sedimentary data

# some climate reconstructions https://pastglobalchanges.org/science/data/databases

# query 2k network https://www.ncei.noaa.gov/pub/data/paleo/pages2k/pages2k-temperature-v2-2017/

# wget -r -A.lpd --no-directories https://www.ncei.noaa.gov/pub/data/paleo/pages2k/pages2k-temperature-v2-2017/data-version-2.0.0/ files

library(tidyverse)
library(sf)
library(tmap)
library(gridExtra)
library(zoo)
library(RColorBrewer)
library(viridis)

x_breaks <- seq(2000,0, -250)
mca_dates_bp <- 1950-c(900, 1300)
lia_dates_bp <- 1950-c(1450, 1850)
varve_y_lims <- c(-2.5, 5)
err_bar_pos <- c(-2.2, # y axis
                 0.5) # height

label_at <- function(n) function(x) ifelse(x %% n == 0, x, "")


source("2k-network/2k-recon/R-functions_gmst.R")

tmap_mode("view")

options(ggplot2.discrete.colour= c("#000000", "#56B4E9"))

# custom function so ggplot has no colour at 0 value
mid_rescaler <- function(mid = 0) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}

# them for ggplots
ggtheme_all <- theme(
  legend.position = 'right',
  legend.title = element_blank(),
  axis.title.y = element_text(angle=0,vjust = 0.5),
  axis.title.x = element_blank()
)

ggtheme_sel <- theme(
  axis.text.y = element_blank(),
  legend.text = element_blank(),
  legend.key.size = unit(0, 'cm'),
)

glob_lims <- c(2000, -75)

#### solomina et al #### 
# Solomina et al Glacier Advance Review for western canada she also sites Koch 2011
# Advances listed below are for western Canada, and correspond well to thick varves outlined
# in Menounos et al 2009. 
# Years of glacier advance are given in Table 2 of solomina et al
# relative glacier extent from fig. 3 in solomina et al 2016 was digitized using https://apps.automeris.io/wpd/

glacier_extent_dat <- read.csv('2k-network/Solomina_et_al/email_from_olga/alex_manual_trace/alex_manual_trace.csv') |> 
  mutate(relative_glacier_extent = 1 - relative_glacier_extent,
         year_bp = 1950 - year_ce)

# maurer 2012 castle creek advances 

maurer_ad1 <- c(1.87, 1.72)
maurer_ad1 <- (maurer_ad1*1000)
maurer_ad2 <- c(1.52, 1.43)
maurer_ad2 <- (maurer_ad2*1000)


glc_adv_plot <- glacier_extent_dat |> ggplot(aes(year_bp, relative_glacier_extent, colour = 'none')) +
  annotate("rect", xmin = 1950-400, xmax = 1950-600, ymin = 0, ymax = 1, fill = "gray", alpha = 0.5) +
  annotate("rect", xmin = 1950-1200, xmax = 1950-1500, ymin = 0, ymax = 1, fill = "gray", alpha = 0.5) +
  annotate("rect", xmin = 1950-1690, xmax = 1950-1730, ymin = 0, ymax = 1, fill = "gray", alpha = 0.5) +
  annotate("rect", xmin = 1950-1830, xmax = 1950-1890, ymin = 0, ymax = 1, fill = "gray", alpha = 0.5) +
  annotate("rect", xmin = 1950-1830, xmax = 1950-1890, ymin = 0, ymax = 1, fill = "gray", alpha = 0.5) +
  annotate("rect", xmin = maurer_ad1[1], xmax = maurer_ad1[2], ymin = 0, ymax = 1, fill = "blue", alpha = 0.3) +
  annotate("rect", xmin = maurer_ad2[1], xmax = maurer_ad2[2], ymin = 0, ymax = 1, fill = "blue", alpha = 0.3) +
  geom_line() +
  # scale_x_reverse() +
  xlab('Age (cal yr BP)') +
  ylab('Relative Glacier\nExtent') +
  theme_bw() +
  ggtheme_all +
  xlim(glob_lims) +
  scale_y_continuous(breaks = seq(0, 1, by = 1), labels = c('min', 'max')) +
  scale_colour_manual("", 
                      breaks = c("none", "TempMedia", "TempMin"),
                      values = c("black")) +
  theme(legend.key = element_rect(fill = "white"), legend.text = element_text(color = "white"), legend.title = element_text(color = "white")) +
  guides(color = guide_legend(override.aes = list(color = NA)))
glc_adv_plot


 
#### king et al #####
# this one might be overboard... 
# https://www.ncei.noaa.gov/access/paleo-search/study/33632
 
#### moberg et al 2005 nature suppliment from word file ####
 
 # This file contains the data for the Northern Hemisphere temperature reconstruction in the paper ‘Highly variable Northern Hemisphere temperatures reconstructed from low- and high-resolution data’ by Moberg, A., Sonechkin, D.M., Holmgren, K., Datsenko, N.M & Karlén, W.
 # 
 # The data columns are:
 #   1. Year from AD 1 to 1979.
 # 2. Full reconstruction AD 1-1979 (red series in Fig. 2b).
 # 3. Low-frequency component AD 133-1925 (blue curve in Fig. 2b and 2d).
 # 4. Lower bound for uncertainty in the low-frequency component due to the variance among low-resolution proxes (uncertainty A in supplementary method 1), (medium-blue band in Fig. 2d).
 # 5. As column 4, but upper bound.
 # 6. Lower bound for the combination of the uncertainty A and that due to the determination of the variance scaling factor used in the calibration (uncertainty B), (light-blue band in Fig. 2d).
 # 7. As column 6, but upper bound
 # 8. Lower bound for the combined uncertainties A and B and that due to the determination of the constant adjustment term (uncertainty C), (outermost blue band in Fig. 2d).
 # 9. As column 8, but upper bound.
 
# All data are given as temperature anomalies (K) from the Northern Hemisphere annual mean temperature 1961-90 average. Lack of data are indicated with –9.9999

 col_names <- c('year_ce', 'temp_anom', 'temp_anom_low_freq', 'low_se_1',
               'upper_se_1', 'low_se_2', 'upper_se_2', 'low_se_3', 'upper_se_3')
 
mob <- read.table('2k-network/Moberg_et_al/moberg_et_al_2k_t_anom.txt', 
            comment.char = '#', col.names = col_names, na.strings = '-9.9999')

mob_long <-  mob |> 
  pivot_longer(temp_anom:temp_anom_low_freq) |> 
  mutate(year_bp = 1950 - year_ce)

nh_ann_mean_t <- 14.586 # from https://crudata.uea.ac.uk/cru/data/temperature/abs_glnhsh.txt

nh_y_lims_hi <- max(mob_long$value, na.rm = T)
nh_y_lims_lo <- min(mob_long$value, na.rm = T)

NH_temp_anom <- ggplot(mob_long, aes(year_bp, value, colour = name)) +
  geom_line(aes()) +
  geom_ribbon(aes(ymin = low_se_1,
                  ymax = upper_se_1),
              alpha = 0, linetype = 2) +
  # geom_ribbon(aes(ymin = temp2m.ann.2SE.1 - median(temp2m.ann, na.rm = T), 
  #                 ymax = temp2m.ann.2SE - median(temp2m.ann, na.rm = T), 
  #                 fill = '2SE'), 
  #             alpha = 0.2) +
  # scale_fill_manual(values = c("#08519C", "#9ECAE1"))+
  xlim(glob_lims) +
  ylab('NH Temp.\nAnomaly (°C)') +
  theme_bw() +
  ggtheme_all +
  theme(legend.text = element_blank(),
        legend.key = element_rect(fill = "white")) +
  annotate('text', x = mean(lia_dates_bp), y = .25, label = 'LIA') +
  annotate('text', x = mean(mca_dates_bp), y = -1, label = 'MCA') +
  scale_color_manual(values = c("#fc8961", '#000004', '#000004')) +
  guides(color = guide_legend(override.aes = list(color = NA))) +
  annotate(
    "rect",
    xmin = mca_dates_bp[1],
    xmax = mca_dates_bp[2],
    ymin = nh_y_lims_lo,
    ymax = nh_y_lims_hi,
    fill = "salmon",
    alpha = 0.4
  ) +
  annotate(
    "rect",
    xmin = lia_dates_bp[1],
    xmax = lia_dates_bp[2],
    ymin = nh_y_lims_lo,
    ymax = nh_y_lims_hi,
    fill = "lightblue",
    alpha = 0.4
  )
  # scale_color_manual(values = viridis(3))

NH_temp_anom

##### 12 century hydro climate ####
# BRING IN HYDRO AND TEMP RECONSTRUCTIONS for last 12 centuries
# Fredrik Charpentier Ljungqvist, Paul J. Krusic, Hanna S. Sundqvist, Eduardo Zorita, Gudrun Brattström, and David Frank. 2016. Northern Hemisphere hydroclimatic variability over the past twelve centuries. Nature, 532(7597), 94-98. doi: 10.1038/nature17418 
# file:///tmp/mozilla_alex0/Northern_hemisphere_hydroclima.PDF
# https://www.ncei.noaa.gov/access/paleo-search/study/19725
# Source_Data_Extended_Data_Figure_6.xls: Centennial temperature proxy anomalies updated from ref. 15.
# Source_Data_Extended_Data_Figure_5.xls: Simulated median values of annual precipitation from six atmosphere–ocean coupled general circulation models.
# ANOMOLIES time series value - median

map_df <- readxl::read_xls('2k-network/hydro-recons/Source_Data_Extended_Data_Figure_1.xls')
hydro_df <- readxl::read_xls('2k-network/hydro-recons/Source_Data_Extended_Data_Figure_5.xls',na = '-999.000')
temp_df <- readxl::read_xls('2k-network/hydro-recons/Source_Data_Extended_Data_Figure_6.xls')

# hydro_sf <- hydro_df |> 
#   st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)
# tm_shape(hydro_sf) +
#   tm_dots()
# 
# temp_sf <- temp_df |> 
#   st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)
# tm_shape(temp_sf) +
#   tm_dots()

#### hydro plot ####
# find grid cell closest to cariboo

needle_lt <- 53
needle_ln <- -121

match <- data.frame(Longitude = c(-117.15 , -116.45),
                    Label = c("Sask. Glacier", "Sask. River"))

blanks <- data.frame(name = rep(NA, 9),
                     value = rep(NA, 9),
                     Year = seq(50, 850, 100),
                     Label = rep('Hydroclimate Anomaly',9))

cariboo_hydro <- hydro_df |> 
  mutate(across(everything(), as.numeric),
         Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude),
         long_diff = abs(Longitude - needle_ln),
         lat_diff = abs(Latitude - needle_lt),
         diff_sum = long_diff + lat_diff) |>
  # filter(diff_sum < 5.7) |> 
  # select(-contains('diff')) |> 
  summarise(across(`800s`:`1900s`, mean, na.rm = T)) |> 
  pivot_longer(`800s`:`1900s`) |> 
  mutate(Year = as.numeric(gsub("s", "", name)) + 50, # make it mid point 
         value = as.numeric(value),
         Label = 'Hydroclimate Anomaly') |> 
  filter(value != -999.00) |> 
  rbind(blanks) |> 
  mutate(Year = 1950 - Year)


# cariboo_hydro_median <- median(cariboo_hydro$value, na.rm = T)
# 
# cariboo_hydro <- cariboo_hydro |> 
#     mutate(value = value - cariboo_hydro_median)

hydro_anom_plot <- cariboo_hydro |> 
  ggplot(aes(x=Year, y = Label, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "BrBG", direction = 1, rescaler = mid_rescaler(), na.value = 'transparent') +
  #scale_fill_distiller(palette = "BrBG", direction = 1, rescaler = mid_rescaler(), na.value = 'transparent') +
  # scale_x_reverse() +
  theme_bw() +
  # ggtheme_all +
  theme(
    legend.position = 'right',
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_text(angle=0,vjust = 0.5),
    legend.key.size = unit(0.3, 'cm')) +
  xlim(glob_lims) +
  xlab('Age (cal yr BP)') +
  ylab('NH Precip.\nAnomaly')

hydro_anom_plot
  
# ggsave('figs/2k-network/hydro_anomalies_2grids_12centuries.jpg', width = 8, height = 1.5)

#### plot cariboo long core on climate proxy ####

# gain size ----

gs <- readRDS('data/long_cores/grain_size_v1_v2_combined.RDS') |> 
  mutate(year_ce_avg = round(year_ce_new),
         year_bp_ams = 1950 - year_ce_avg) # this is the ams age depth model

gs_plot <- 
  gs |> 
  mutate(
    smooth = smoother::smth(x = stdep, method = 'gaussian', window = 6),
    mvavg = zoo::rollapply(stdep, 3, mean, align = 'center', fill = NA)
  ) |> 
  ggplot(aes(x = year_bp_ams, color = core)) +
  geom_line(aes(y = stdep), method = "lm", 
            formula = y ~ 0, colour = "black", se=F, 
            linetype="dotted", size = .5, stat = 'smooth', alpha = 0.4) +
  geom_point(aes(y = stdep), alpha = 1) +
  #geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE) +
  geom_line(aes(y = mvavg)) +
  ylab("D50 Sd. Dept.") +
  # xlab("Year (CE)") +  
  theme_bw()+
  xlim(glob_lims) +
  ggtheme_all 


# varve thickness ----

vt <- readRDS('data/long_cores/varve_thickness_v1_v2_working.RDS')

vt_plot <- 
  vt |> 
  filter(core_sub_id != "E13-V1") %>% 
  mutate(
    mvavg = zoo::rollapply(lyr_mm_stdep_fltr, width = 100, by = 1, FUN = mean, na.rm = T, align = "center", partial = T), # partial defines minimum number of objects to continue 25yr window. set 3 to get data point at end of dataset 
  ) |> 
  ggplot(aes(x = year_bp_ams, colour = core)) +
  geom_line(aes(y = lyr_mm_stdep_fltr), method = "lm", 
            formula = y ~ 0, colour = "black", se=F, 
            linetype="dotted", size = .5, stat = 'smooth', alpha = 0.4) +  
  geom_line(aes(y = lyr_mm_stdep_fltr), alpha = 0.3) +
  #geom_line(aes(y = smooth)) +
  geom_line(aes(y = ma_30)) +
  ylab("VT Sd. Dept.") +
  ylim(varve_y_lims)+
  theme_bw()+
  xlim(-50, 2025)+
  scale_color_manual(values = c("#E69F00", "#000000", "#56B4E9"))+
  scale_x_reverse(breaks = x_breaks,  
                  limits = glob_lims,
                  labels = label_at(500)) +
  xlim(glob_lims) +
  ggtheme_all 

vt_plot

# plotly::ggplotly()

# LOI ----

loi <- readRDS('data/Sediment/LOI/loi_v1_v2_working.RDS') |> 
  filter(stdep < 3 & stdep > -3) |> 
  mutate(year_bp_ams = 1950 - year_ce_new)

loi_plot <- 
  loi |> 
  mutate(
    #  smooth = smoother::smth(x = stdep, method = 'gaussian', window = 3),
    mvavg = zoo::rollapply(stdep, 3, mean, align = 'center', fill = NA)
  ) |> 
  ggplot(aes(x = year_bp_ams, colour = core)) +
  geom_line(aes(y = stdep), method = "lm", 
            formula = y ~ 0, colour = "black", se=F, 
            linetype="dotted", size = .5, stat = 'smooth', alpha = 0.4) + 
  geom_point(aes(y = stdep), alpha = 1) +
  #geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE) +
  geom_line(aes(y = mvavg)) +
  ylab("OM Sd. Dept.") +
  theme_bw() +
  xlim(glob_lims) +
  ylim(-3, 3)+
  ggtheme_all

loi_plot

#### plot glac adv. temp anom, hydroclimate, cariboo sediment #### 

# p <- list(vt_plot, gs_plot, loi_plot, glc_adv_plot, tr, hydro_anom_plot, glob_temp_50_p)

p <- list(vt_plot, loi_plot, gs_plot, glc_adv_plot, NH_temp_anom, hydro_anom_plot)


cp <- cowplot::plot_grid(plotlist = p, nrow=length(p), 
                         labels = LETTERS[seq( from = 1, to = length(p) )], 
                         align = 'v', 
                         rel_heights = c(2,2,2,2, 2, 1))

cp

saveRDS(cp, 'figs/2k-network/all_core_stats_2k_anomalies.rds')
cowplot::save_plot('journal-submission/markdown/figs/all_core_stats_2k_anomalies.jpg', 
                   cp, base_width = 8, base_height = 8)

