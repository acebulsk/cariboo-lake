# script to construct fig. 6, 7, 8 of temporal trends in varve, grain size, loi. 

library(dplyr)
library(ggplot2)

label_at <- function(n) function(x) ifelse(x %% n == 0, x, "")

#### age depth model and c14 meta ####

v1_lm <- readRDS('data/long_cores/chronology/v1_c14_age_depth_model.rds')
v2_lm <- readRDS('data/long_cores/chronology/v2_c14_age_depth_model.rds')

ams_meta <- readRDS('data/long_cores/chronology/long_core_ams_meta.rds') 

v1_plt_ams <- ams_meta |> filter(depth != 0, 
                                 core == 'V1')
v2_plt_ams <- ams_meta |> filter(depth != 0, 
                                 core == 'V2')

standard_yr_bp <- 1950 # the year used in the literature as BP datum
yr_core_ce <- 2017 # this is the year we took the core
yr_core_bp <- standard_yr_bp-yr_core_ce

#### Fig 6 - Temporal Trends in Varve Thickness ####
# See varve-count.R for construction of gaus.RDS file

gaus <- readRDS('data/long_cores/varve_thickness_v1_v2_working.RDS')

# add in ekmans to cover disturbed sections

ek <- read.csv('data/ekman/EK_varveCounting_orig_long_analysis.csv') 

v1_plot <- 
  ggplot(gaus %>% 
           filter(core == "V1"), aes(x = year_bp_ams)) +
  geom_smooth(aes(y = lyr_mm_stdep_fltr), 
              method = "lm", formula = y ~ 1, colour = "black", size = 0.5, se=F, linetype="dashed")+
  geom_line(aes(y = lyr_mm_stdep_fltr), alpha = 1/4) +
  geom_line(aes(y = ma_30)) +
  # geom_point(data = v1_plt_ams,
  #            aes(x = year_ce, y = -1.5),  shape = 4, size = 2) +
  geom_errorbarh(data = v1_plt_ams, 
                 aes(y = 1, xmin=year_ce-ams_cal_se, xmax=year_ce+ams_cal_se, height = 25)) +
  ylab("VT Std. Dept.") +
  xlab("Year (CE)") +
  ylim(c(-2, 5)) +
  ggtitle("V1")  + # scale sec y axis based on c14
  theme_bw()
v1_plot

ggplot() +
  geom_point(data = v1_plt_ams,
             aes(x = year_ce, y = -1.5),  shape = 4, size = 2) +
  geom_errorbarh(data = v1_plt_ams, 
                 aes(y = -1.5, xmin=year_ce-ams_cal_se, xmax=year_ce+ams_cal_se, height = 25))


#ggplotly((v1_plot))

v2_plot <- 
  gaus %>% 
  filter(core == "V2") %>% 
  ggplot(aes(x = year_bp_ams)) +
  geom_smooth(aes(y = lyr_mm_stdep_fltr), method = "lm", formula = y ~ 1, colour = "black", size = 0.5, se=F, linetype="dashed")+
  geom_line(aes(y = lyr_mm_stdep_fltr), alpha = 1/4) +
  geom_line(aes(y = ma_30)) +
  # geom_line(aes(y = smooth)) +
  geom_point(aes(x = 2017 - v2_C14$year, y = -1.5), shape = 4) +
  geom_text(aes(x = 2017 - v2_C14$year, y = -1.75), label = "47 ± 75 yr. CE", vjust = 1) +
  xlab("Year (CE)") +
  ylab("VT Std. Dept.") +
  ylim(c(-2, 5)) +
  ggtitle("V2") +
  scale_x_continuous(
    breaks = seq(0,2000, 250),  
    limits = c(-50, 2000),
    labels = label_at(500),  
    sec.axis = 
      sec_axis(
        breaks = seq(4000,0, -500),  
        labels = label_at(1000),
        trans=~ . * summary(v2_lm)$coeff[2] 
        + summary(v2_lm)$coeff[1],
        name="Core Depth (mm)")) + # scale sec y axis based on c14
  theme_bw() 


v2_plot
#ggplotly((v2_plot))


p <- list(v1_plot, v2_plot)
cp <- cowplot::plot_grid(plotlist = p, nrow=2, align = 'v')
cp

ggsave("sage-submission/figs/V1_V2_varvethickness_vs_depth_and_C14_est_yr_ma.png", cp,  width = 8.5, height = 6)
saveRDS(cp, "figs/V1_V2_varvethickness_vs_depth_and_C14_est_yr_ma.rds")

#### Fig 7 - Grain Size ####

# see grain-size.R for creation of df

gs <- readRDS('data/long_cores/grain_size_v1_v2_combined.RDS') 

v1_plot <- 
  gs %>% 
  filter(core == 'V1') %>% 
  mutate(
    smooth = smoother::smth(x = stdep, method = 'gaussian', window = 5),
    mvavg = zoo::rollapply(stdep, width = 3, by = 1, FUN = mean, na.rm = T, align = "center", partial = T)
  ) %>% 
  ggplot(aes(x = year_ce_new)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 1, colour = "black", se=F, linetype="dashed", size = .5) +
  # geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE, colour = "gray") +
  geom_point(aes(y = stdep), alpha = 1) +
  #geom_line(aes(y = smooth)) +
  geom_line(aes(y = mvavg), colour = "gray") +
  geom_point(aes(x = 2017 - v1_C14$year, y = -1.5)) +
  geom_text(aes(x = 2017 - v1_C14$year, y = -1.70), label = "158 ± 40 yr. CE", vjust = 1) +
  ylab("D50 Std. Dept.") +
  xlab("Year (CE)") +  
  ggtitle("V1") +
  scale_x_continuous(
    breaks = seq(0,2000, 250),  
    labels = label_at(500), 
    limits = c(-75, 2050),
    sec.axis=sec_axis( 
      breaks = seq(4000,0, -500),  
      labels = label_at(1000),
      trans=~ . * summary(v1_lm)$coeff[2] + summary(v1_lm)$coeff[1] , 
      name="Core Depth (mm)")) + # scale sec y axis based on c14  theme_bw()
  theme_bw()

v1_plot

v2_plot <- 
  gs %>% 
  filter(core == 'V2') %>% 
  mutate(
    smooth = smoother::smth(x = stdep, method = 'gaussian', window = 3),
    mvavg = zoo::rollapply(stdep, 5, mean, align = 'center', fill = NA)
  ) %>% 
  ggplot(aes(x = year_ce_new)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 1, colour = "black", se=F, linetype="dashed", size = .5) +
  geom_point(aes(y = stdep), alpha = 1) +
  #geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE) +
  geom_line(aes(y = mvavg), colour = "gray") +
  geom_point(aes(x = 2017 - v2_C14$year, y = -1.5), shape = 4) +
  geom_text(aes(x = 2017 - v2_C14$year, y = -1.75), label = "47 ± 75 yr. CE", vjust = 1) +
  ylab("D50 Std. Dept.") +
  xlab("Year (CE)") +  
  ggtitle("V2") +
  scale_x_continuous(
    breaks = seq(0,2000, 250),  
    labels = label_at(500), 
    limits = c(-75, 2050),
    sec.axis=sec_axis( 
      breaks = seq(4000,0, -500),  
      labels = label_at(1000),
      trans=~ . * summary(v2_lm)$coeff[2] + summary(v2_lm)$coeff[1] , 
      name="Core Depth (mm)")) + # scale sec y axis based on c14  theme_bw()
  theme_bw()

v2_plot

p <- list(v1_plot, v2_plot)

cp <- cowplot::plot_grid(plotlist = p, nrow = 2, align = 'v')
cp
# saveRDS(cp, "sage-submission/figs/grain-size/V1_V2_grainsize_vs_depth_and_C14_est_yr.rds")
ggsave("sage-submission/figs/V1_V2_grainsize_vs_depth_and_C14_est_yr.png", cp,  width = 8.5, height = 6)

saveRDS(v1_plot, 'figs/grain_size_v1.rds')
saveRDS(v2_plot, 'figs/grain_size_v2.rds')


#### Fig 8 - LOI ####

# see loi.R for creation of loi df

loi <- readRDS('data/Sediment/LOI/loi_v1_v2_working.RDS') %>% 
  filter(is.na(turbidite) == T)

v1_plot <- 
  loi %>%
  filter(core == "V1") %>% 
  mutate(
    # smooth = smoother::smth(x = stdep, method = 'gaussian', window = 5),
    mvavg = zoo::rollapply(stdep, width = 3, by = 1, FUN = mean, na.rm = T, align = "center", partial = T)
  ) %>% 
  ggplot(aes(x = year_ce_new)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 1, colour = "black", se=F, linetype="dashed", size = .5) +
  # geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE, colour = "gray") +
  geom_point(aes(y = stdep), alpha = 1) +
  #geom_line(aes(y = smooth)) +
  geom_line(aes(y = mvavg), colour = "gray") +
  geom_point(aes(x = 2017 - v1_C14$year, y = -2.5), shape = 4) +
  geom_text(aes(x = 2017 - v1_C14$year, y = -2.70), label = "158 ± 40 yr. CE", vjust = 1) +
  ylab("OM Std. Dept.") +
  xlab("Year (CE)") +  
  ggtitle("V1") +
  scale_x_continuous(
    breaks = seq(0,2000, 250),  
    labels = label_at(500), 
    limits = c(-75, 2050),
    sec.axis=sec_axis( 
      breaks = seq(4000,0, -500),  
      labels = label_at(1000),
      trans=~ . * summary(v1_lm)$coeff[2] + summary(v1_lm)$coeff[1] , 
      name="Core Depth (mm)")) + # scale sec y axis based on c14  theme_bw()
  theme_bw()
v1_plot

v2_plot <- 
  loi %>% 
  filter(core == "V2") %>% 
  mutate(
    smooth = smoother::smth(x = stdep, method = 'gaussian', window = 5),
    mvavg = zoo::rollapply(stdep, width = 3, by = 1, FUN = mean, na.rm = T, align = "center", partial = T)
  ) %>% 
  ggplot(aes(x = year_ce_new)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 1, colour = "black", se=F, linetype="dashed", size = .5) +
  # geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE, colour = "gray") +
  geom_point(aes(y = stdep), alpha = 1) +
  #geom_line(aes(y = smooth)) +
  geom_line(aes(y = mvavg), colour = "gray") +
  geom_point(aes(x = 2017 - v2_C14$year, y = -2), shape = 4) +
  geom_text(aes(x = 2017 - v2_C14$year, y = -2.55), label = "47 ± 75 yr. CE") +
  ylab("OM Std. Dept.") +
  xlab("Year (CE)") +  
  ggtitle("V2") +
  scale_x_continuous(
    breaks = seq(0,2000, 250),  
    labels = label_at(500), 
    limits = c(-75, 2050),
    sec.axis=sec_axis( 
      breaks = seq(4000,0, -500),  
      labels = label_at(1000),
      trans=~ . * summary(v2_lm)$coeff[2] + summary(v2_lm)$coeff[1] , 
      name="Core Depth (mm)")) + # scale sec y axis based on c14  theme_bw()
  theme_bw()
v2_plot

p <- list(v1_plot, v2_plot)

cp <- cowplot::plot_grid(plotlist = p, nrow = 2, align = 'v')
cp

saveRDS(cp, "figs/V1_V2_LOI_vs_depth_and_C14_est_yr.rds")
ggsave("sage-submission/figs/V1_V2_LOI_vs_depth_and_C14_est_yr.png", cp,  width = 8.5, height = 6)

