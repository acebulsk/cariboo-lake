# script to construct fig. 6, 7, 8 of temporal trends in varve, grain size, loi. 

library(dplyr)
library(ggplot2)
library(ggthemes)

options(ggplot2.discrete.colour= c("#000000", "#56B4E9"))

gs_loi_dot_sz <- 0.5

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

varve_y_lims <- c(-2.5, 5)
err_bar_pos <- c(-2.2, # y axis
                 0.5) # height

gaus <- readRDS('data/long_cores/varve_thickness_v1_v2_working.RDS')

varve_x_lims <- c(-100, max(gaus$year_ce_ams, na.rm = T))

v1_se <- c(v1_plt_ams$year_ce-v1_plt_ams$ams_cal_se, v1_plt_ams$year_ce+v1_plt_ams$ams_cal_se)

v1_plot <- 
  ggplot(gaus %>% 
           filter(core == "V1"), aes(x = year_ce_ams)) +
  geom_smooth(aes(y = lyr_mm_stdep_fltr), 
              method = "lm", formula = y ~ 1, colour = "black", linewidth = 0.5, se=F, linetype="dotted")+
  geom_line(aes(y = lyr_mm_stdep_fltr), alpha = 1/4) +
  geom_line(aes(y = ma_30)) +
  geom_point(data = v1_plt_ams,
             aes(x = year_ce, y = err_bar_pos[1]),  shape = 4, size = 2) +
  geom_errorbarh(aes(y = err_bar_pos[1], xmin=v1_se[1], xmax=v1_se[2], height = err_bar_pos[2]), lwd = 0.1) +
  ylab("VT Std. Dept.") +
  xlab("Year (CE)") +
  ylim(varve_y_lims) +
  ggtitle("V1")  +
  scale_x_continuous(
    breaks = seq(0,2000, 250),  
    limits = varve_x_lims,
    labels = label_at(500),  
    sec.axis = 
      sec_axis(
        breaks = seq(400,0, -50),
        labels = label_at(100),
        trans= ~((standard_yr_bp - .) - summary(v1_lm)$coeff[1]) / summary(v1_lm)$coeff[2],
        name="Core Depth (cm)")) + # scale sec y axis based on c14
  theme_bw()
v1_plot

#ggplotly((v1_plot))

v2_se <- c(v2_plt_ams$year_ce-v2_plt_ams$ams_cal_se, v2_plt_ams$year_ce+v2_plt_ams$ams_cal_se)

v2_plot <- 
  gaus %>% 
  filter(core == "V2") %>% 
  ggplot(aes(x = year_ce_ams)) +
  geom_smooth(
    aes(y = lyr_mm_stdep_fltr),
    method = "lm",
    formula = y ~ 1,
    colour = "black",
    size = 0.5,
    se = F,
    linetype = "dotted"
  ) +
  geom_line(aes(y = lyr_mm_stdep_fltr), alpha = 1/4) +
  geom_line(aes(y = ma_30))  +
  geom_point(data = v2_plt_ams,
             aes(x = year_ce, y = err_bar_pos[1]),  shape = 4, size = 2) +
  geom_errorbarh(aes(y = err_bar_pos[1], xmin=v2_se[1], xmax=v2_se[2], height = err_bar_pos[2]), lwd = 0.1) +
  xlab("Year (CE)") +
  ylab("VT Std. Dept.") +
  ylim(varve_y_lims) +
  ggtitle("V2") +
  scale_x_continuous(
    breaks = seq(0,2000, 250),  
    limits = varve_x_lims,
    labels = label_at(500),  
    sec.axis = 
      sec_axis(
        breaks = seq(400,0, -50),
        labels = label_at(100),
        trans= ~((standard_yr_bp - .) - summary(v2_lm)$coeff[1]) / summary(v2_lm)$coeff[2],
        name="Core Depth (cm)")) + # scale sec y axis based on c14
  theme_bw() 


v2_plot
#ggplotly((v2_plot))


p <- list(v1_plot, v2_plot)
cp <- cowplot::plot_grid(plotlist = p, nrow=2, align = 'v')
cp

ggsave("journal-submission/markdown/figs/V1_V2_varvethickness_vs_depth_and_C14_est_yr_ma.png", cp,  width = 8.5, height = 6)

# saveRDS(cp, "figs/V1_V2_varvethickness_vs_depth_and_C14_est_yr_ma.rds")

#### Fig 7 - Grain Size ####

# see grain-size.R for creation of df

gs <- readRDS('data/long_cores/grain_size_v1_v2_combined.RDS') 

gs_x_lims <- c(min(gs$year_ce_new, na.rm = T), max(gs$year_ce_new, na.rm = T))

# gs_x_lims <- c(-100, 2050)

gs_plot <- 
  gs %>% 
  mutate(
    smooth = smoother::smth(x = stdep, method = 'gaussian', window = 5),
    mvavg = zoo::rollapply(stdep, width = 3, by = 1, FUN = mean, na.rm = T, align = "center", partial = T)
  ) %>% 
  ggplot(aes(x = year_ce_new, color = core)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 0, colour = "black", se=F, linetype="dotted", size = .5) +
  # geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE, colour = "gray") +
  geom_point(aes(y = stdep), alpha = 1, size = gs_loi_dot_sz) +
  #geom_line(aes(y = smooth)) +
  geom_line(aes(y = mvavg)) +
  ylab("D50 Std. Dept.") +
  xlab("Year (CE)") +  
  scale_x_continuous(
    breaks = seq(0,2000, 250),  
    limits = gs_x_lims,
    labels = label_at(500),  
    sec.axis = 
      sec_axis(
        breaks = seq(400,0, -50),
        labels = label_at(100),
        trans= ~((standard_yr_bp - .) - summary(v1_lm)$coeff[1]) / summary(v1_lm)$coeff[2],
        name="Core Depth (cm)"))  + # scale sec y axis based on c14  theme_bw()
  # scale_color_colorblind() +
  theme_bw()

gs_plot

# saveRDS(cp, "journal-submission/markdown/figs/grain-size/V1_V2_grainsize_vs_depth_and_C14_est_yr.rds")
ggsave("journal-submission/markdown/figs/V1_V2_grainsize_vs_depth_and_C14_est_yr.png", 
       gs_plot,  
       width = 8, 
       height = 3)

#### Fig 8 - LOI ####

# see loi.R for creation of loi df

loi <- readRDS('data/Sediment/LOI/loi_v1_v2_working.RDS') %>% 
  filter(is.na(turbidite) == T)

loi_x_lims <- c(min(loi$year_ce_new, na.rm = T), max(loi$year_ce_new, na.rm = T))

loi_plot <- 
  loi %>%
  mutate(
    # smooth = smoother::smth(x = stdep, method = 'gaussian', window = 5),
    mvavg = zoo::rollapply(stdep, width = 3, by = 1, FUN = mean, na.rm = T, align = "center", partial = T)
  ) %>% 
  ggplot(aes(x = year_ce_new, colour = core)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 0, colour = "black", se=F, linetype="dotted", size = .5) +
  # geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE, colour = "gray") +
  geom_point(aes(y = stdep), alpha = 1, size = gs_loi_dot_sz) +
  #geom_line(aes(y = smooth)) +
  geom_line(aes(y = mvavg)) +
  ylab("OM Std. Dept.") +
  xlab("Year (CE)") +  
  scale_x_continuous(
    breaks = seq(0,2000, 250),  
    limits = loi_x_lims,
    labels = label_at(500),  
    sec.axis = 
      sec_axis(
        breaks = seq(400,0, -50),
        labels = label_at(100),
        trans= ~((standard_yr_bp - .) - summary(v1_lm)$coeff[1]) / summary(v1_lm)$coeff[2],
        name="Core Depth (cm)")) + # scale sec y axis based on c14  theme_bw()
  theme_bw()
loi_plot

ggsave(
  "journal-submission/markdown/figs/V1_V2_LOI_vs_depth_and_C14_est_yr.png",
  loi_plot,
  width = 8,
  height = 3
)

