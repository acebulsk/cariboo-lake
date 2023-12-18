# script to construct fig. 6, 7, 8 of temporal trends in varve, grain size, loi. 

library(dplyr)
library(ggplot2)
library(ggthemes)

options(ggplot2.discrete.colour= c("#000000", "#56B4E9"))

xlab_bp <- "Age (cal yr BP)"
x_breaks <- seq(2000,0, -250)
sec_x_breaks <- seq(0,400, 50)

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

# LIA and MCA dates from Steinman2012

mca_dates_bp <- c(standard_yr_bp - 900, standard_yr_bp - 1300)
lia_dates_bp <- c(standard_yr_bp - 1450, standard_yr_bp - 1850)

#### Fig 6 - Temporal Trends in Varve Thickness ####
# See varve-count.R for construction of gaus.RDS file

varve_y_lims <- c(-2.5, 5)
err_bar_pos <- c(-2.2, # y axis
                 0.5) # height

gaus <- readRDS('data/long_cores/varve_thickness_v1_v2_working.RDS')

varve_x_lims <- c(2100, min(gaus$year_bp_ams, na.rm = T))

v1_se <- c(v1_plt_ams$median_age-v1_plt_ams$ams_cal_se, v1_plt_ams$median_age+v1_plt_ams$ams_cal_se)

v1_plot <- 
  ggplot(gaus %>% 
           filter(core_sub_id %in% c("V1", "E13-V1")), aes(x = year_bp_ams)) +
  geom_smooth(aes(y = lyr_mm_stdep_fltr), 
              method = "lm", formula = y ~ 1, colour = "black", linewidth = 0.5, se=F, linetype="dotted")+
  geom_line(aes(y = lyr_mm_stdep_fltr, colour = core, group = core), alpha = 1/4) +
  geom_line(aes(y = ma_30, colour = core, group = core)) +
  geom_point(data = v1_plt_ams,
             aes(x = median_age, y = err_bar_pos[1]),  shape = 4, size = 2) +
  geom_errorbarh(aes(y = err_bar_pos[1], xmin=v1_se[1], xmax=v1_se[2], height = err_bar_pos[2]), lwd = 0.1) +
  ylab("VT Std. Dept.") +
  xlab(xlab_bp) +
  ylim(varve_y_lims) +
  ggtitle("V1")  +
  scale_x_continuous(
    breaks = x_breaks,
    limits = varve_x_lims,
    labels = label_at(500),
    trans = 'reverse',
    sec.axis = 
      sec_axis(
        breaks = sec_x_breaks,
        labels = label_at(100),
        trans= ~((.) - summary(v1_lm)$coeff[1]) / summary(v1_lm)$coeff[2],
        name="Core Depth (cm)")) + # scale sec y axis based on c14
  theme_bw()+
  scale_color_manual(values = c("#E69F00", "#000000"))+
  annotate("rect", xmin = mca_dates_bp[1], xmax = mca_dates_bp[2], ymin = varve_y_lims[1], ymax = varve_y_lims[2], fill = "salmon", alpha = 0.4) +
  annotate("rect", xmin = lia_dates_bp[1], xmax = lia_dates_bp[2], ymin = varve_y_lims[1], ymax = varve_y_lims[2], fill = "lightblue", alpha = 0.4) +
  theme(legend.position = 'none')

v1_plot

#ggplotly((v1_plot))

v2_se <- c(v2_plt_ams$median_age-v2_plt_ams$ams_cal_se, v2_plt_ams$median_age+v2_plt_ams$ams_cal_se)

v2_plot <- 
  gaus %>% 
  filter(core_sub_id %in% c("V2", "E13-V2")) %>% 
  ggplot(aes(x = year_bp_ams)) +
  geom_smooth(
    aes(y = lyr_mm_stdep_fltr),
    method = "lm",
    formula = y ~ 1,
    colour = "black",
    size = 0.5,
    se = F,
    linetype = "dotted"
  ) +
  geom_line(aes(y = lyr_mm_stdep_fltr, colour = core), alpha = 1/4) +
  geom_line(aes(y = ma_30, colour = core))  +
  geom_point(data = v2_plt_ams,
             aes(x = median_age, y = err_bar_pos[1]),  shape = 4, size = 2) +
  geom_errorbarh(aes(y = err_bar_pos[1], xmin=v2_se[1], xmax=v2_se[2], height = err_bar_pos[2]), lwd = 0.1) +
  xlab(xlab_bp) +
  ylab("VT Std. Dept.") +
  ylim(varve_y_lims) +
  ggtitle("V2") +
  scale_x_reverse(
    breaks = x_breaks,  
    limits = varve_x_lims,
    labels = label_at(500),  
    sec.axis = 
      sec_axis(
        breaks = sec_x_breaks,
        labels = label_at(100),
        trans= ~((.) - summary(v2_lm)$coeff[1]) / summary(v2_lm)$coeff[2],
        name="Core Depth (cm)")) + # scale sec y axis based on c14
  theme_bw() +
  scale_color_manual(values = c("#E69F00", "#000000"))+
  annotate("rect", xmin = mca_dates_bp[1], xmax = mca_dates_bp[2], ymin = varve_y_lims[1], ymax = varve_y_lims[2], fill = "salmon", alpha = 0.4) +
  annotate("rect", xmin = lia_dates_bp[1], xmax = lia_dates_bp[2], ymin = varve_y_lims[1], ymax = varve_y_lims[2], fill = "lightblue", alpha = 0.4) +
  theme(legend.position = 'none')


v2_plot

#ggplotly((v2_plot))


p <- list(v1_plot, v2_plot)
cp <- cowplot::plot_grid(plotlist = p, nrow=2, align = 'v')
cp

ggsave("journal-submission/markdown/figs/V1_V2_varvethickness_vs_depth_and_C14_est_yr_ma.png", cp,  width = 8.5, height = 6)

# saveRDS(cp, "figs/V1_V2_varvethickness_vs_depth_and_C14_est_yr_ma.rds")

#### Fig 7 - Grain Size ####

# see grain-size.R for creation of df

gs <- readRDS('data/long_cores/grain_size_v1_v2_combined.RDS') |> 
  mutate(year_bp = 1950 - year_ce_new)

# gs_x_lims <- c(min(gs$year_ce_new, na.rm = T), max(gs$year_ce_new, na.rm = T))
gs_x_lims <- c(max(gs$year_bp, na.rm = T), min(gs$year_bp, na.rm = T))

# gs_x_lims <- c(-100, 2050)

gs_plot <- 
  gs %>% 
  mutate(
    smooth = smoother::smth(x = stdep, method = 'gaussian', window = 5),
    mvavg = zoo::rollapply(stdep, width = 3, by = 1, FUN = mean, na.rm = T, align = "center", partial = T)
  ) %>% 
  ggplot(aes(x = year_bp, color = core)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 0, colour = "black", se=F, linetype="dotted", size = .5) +
  # geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE, colour = "gray") +
  geom_point(aes(y = stdep), alpha = 1, size = gs_loi_dot_sz) +
  #geom_line(aes(y = smooth)) +
  geom_line(aes(y = mvavg)) +
  ylab("D50 Std. Dept.") +
  xlab(xlab_bp) +  
  scale_x_reverse(
    breaks = x_breaks,  
    limits = gs_x_lims,
    labels = label_at(500),  
    sec.axis = 
      sec_axis(
        breaks = sec_x_breaks,
        labels = label_at(100),
        trans= ~((.) - summary(v1_lm)$coeff[1]) / summary(v1_lm)$coeff[2],
        name="Core Depth (cm)"))  + # scale sec y axis based on c14  theme_bw()
  # scale_color_colorblind() +
  theme_bw()+
  annotate("rect", xmin = mca_dates_bp[1], xmax = mca_dates_bp[2], ymin = -2.5, ymax = 2.5, fill = "salmon", alpha = 0.4) +
  annotate("rect", xmin = lia_dates_bp[1], xmax = lia_dates_bp[2], ymin = -2.5, ymax = 2.5, fill = "lightblue", alpha = 0.4)


gs_plot

# saveRDS(cp, "journal-submission/markdown/figs/grain-size/V1_V2_grainsize_vs_depth_and_C14_est_yr.rds")
ggsave("journal-submission/markdown/figs/V1_V2_grainsize_vs_depth_and_C14_est_yr.png", 
       gs_plot,  
       width = 8, 
       height = 3)

#### Fig 8 - LOI ####

# see loi.R for creation of loi df

loi <- readRDS('data/Sediment/LOI/loi_v1_v2_working.RDS') %>% 
  filter(is.na(turbidite) == T) |> 
  mutate(year_bp = standard_yr_bp - year_ce_new)

loi_x_lims <- c(max(loi$year_bp, na.rm = T), min(loi$year_bp, na.rm = T))

loi_plot <- 
  loi %>%
  mutate(
    # smooth = smoother::smth(x = stdep, method = 'gaussian', window = 5),
    mvavg = zoo::rollapply(stdep, width = 3, by = 1, FUN = mean, na.rm = T, align = "center", partial = T)
  ) %>% 
  ggplot(aes(x = year_bp, colour = core)) +
  geom_smooth(aes(y = stdep), method = "lm", formula = y ~ 0, colour = "black", se=F, linetype="dotted", size = .5) +
  # geom_smooth(aes(y = stdep), method = lm, formula = y ~ splines::bs(x), se = FALSE, colour = "gray") +
  geom_point(aes(y = stdep), alpha = 1, size = gs_loi_dot_sz) +
  #geom_line(aes(y = smooth)) +
  geom_line(aes(y = mvavg)) +
  ylab("OM Std. Dept.") +
  xlab(xlab_bp) +  
  scale_x_reverse(
    breaks = x_breaks,  
    limits = loi_x_lims,
    labels = label_at(500),  
    sec.axis = 
      sec_axis(
        breaks = sec_x_breaks,
        labels = label_at(100),
        trans= ~((.) - summary(v1_lm)$coeff[1]) / summary(v1_lm)$coeff[2],
        name="Core Depth (cm)")) + # scale sec y axis based on c14  theme_bw()
  theme_bw()+
  annotate("rect", xmin = mca_dates_bp[1], xmax = mca_dates_bp[2], ymin = -2.5, ymax = 2.5, fill = "salmon", alpha = 0.4) +
  annotate("rect", xmin = lia_dates_bp[1], xmax = lia_dates_bp[2], ymin = -2.5, ymax = 2.5, fill = "lightblue", alpha = 0.4)

loi_plot

ggsave(
  "journal-submission/markdown/figs/V1_V2_LOI_vs_depth_and_C14_est_yr.png",
  loi_plot,
  width = 8,
  height = 3
)

