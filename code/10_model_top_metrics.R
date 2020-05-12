# 10_model_top_metrics

library(tidyverse)
library(viridis)
library(sf)
library(purrr)
library(rlang)
library(ggthemes)
library(tidylog)
library(cowplot)

# Load Data ---------------------------------------------------------------

# load updated data w HUC_regions:
load("data_output/05_selected_bmi_csci_por_trim_w_huc_region.rda")
load("data_output/05_selected_bmi_csci_por_w_huc_region.rda")
load("models/09_all_ri_all_regions_csci.rda")

# simple just samples:
bmi_sampleid <- bmi_csci_por_trim %>% st_drop_geometry() %>% 
  dplyr::distinct(SampleID, .keep_all = TRUE) %>% 
  select(StationCode:csci, gage_id_c:median_in_iqr, huc_region)

# get names
library(readxl)
ff_defs <- readxl::read_xlsx("docs/Functional_Flow_Metrics_List_and_Definitions_final.xlsx", range = "A1:F25", .name_repair = "universal", trim_ws = TRUE) 

# join with the full RI table
ri_table <- left_join(ri_all_regions, ff_defs, by=c("var"="Flow.Metric.Code"))

# drop unused factors in flow component:
ri_table <- ri_table %>% 
  filter(method=="mse") %>% 
  mutate(flow_component=forcats::fct_drop(flow_component),
         var = as.factor(var),
         var = fct_reorder2(var, flow_component, var),
         model=as.factor(model),
         Flow.Metric.Name = as.factor(Flow.Metric.Name),
         Flow.Metric.Name = forcats::fct_reorder2(Flow.Metric.Name, model, RI))


# SET UP NAMES --------------------------------------------------------------------

## VARIABLES:
# "all_ca_ffc_only"
# "central_valley", "great_basin", "north_coast", "south_coast", 

hydroDat <- "POR"
modname <- "all_ca_ffc_only" # model name 
plotname <- "All Site Pairs"  #"Central Valley" #"All Site Pairs"
bmiVar <- quote(csci) # select response var

# make pathnames
(plot_savename <- tolower(paste0("09_gbm_", as_name(bmiVar), "_",hydroDat, "_",modname)))


# Get FFC METRIC Data and Join ---------------------------------------------------

# load FFC Metrics Data
load("data_output/02_usgs_all_ffm_data.rda")

## METRICS
# for raw annual ffmetrics
g_all_ffc %>% group_by(gage_id, Year) %>% distinct()
ffm <- g_all_ffc %>% mutate(Year=as.integer(Year)) %>% 
  distinct() # filter out duplication

# make it long not wide for joins:
ffm <- pivot_longer(ffm, cols= c(DS_Dur_WS:Peak_Fre_5), names_to = "ffm_metric", values_to = "ffm_value") %>% 
  # drop nas
  filter(!is.na(ffm_value))

# join for POR
bmi_ffm_por <- left_join(bmi_csci_por_trim, ffm, by=c("gage_id_c"="gage_id", "metric"="ffm_metric"))

# # join for ann data:
# bmi_ffm_ann <- left_join(bmi_sampleid, ffm, by=c("gage_id_c"="gage_id", "YYYY"="Year"))
# 
# # years -1  -2
# lag_yrs_1 <- unique(bmi_sampleid$YYYY) - 1
# lag_yrs_2 <- unique(bmi_sampleid$YYYY) - 2
# 
# # make lag data
# ffm_lag1 <- ffm %>% filter(Year %in% lag_yrs_1) %>% 
#   mutate(year_flow = Year-1) # add for labeling purposes
# ffm_lag2 <- ffm %>% filter(Year %in% lag_yrs_2) %>% 
#   mutate(year_flow = Year-2) # add for labeling purposes
# 
# # rejoin
# bmi_ffm_lag1 <- left_join(bmi_sampleid, ffm_lag1, by=c("gage_id_c"="gage_id", "YYYY"="Year"))
# bmi_ffm_lag2 <- left_join(bmi_sampleid, ffm_lag2, by=c("gage_id_c"="gage_id", "YYYY"="Year"))


# Get FFC PERCENTILE Data and Join ----------------------------------------

## PERCENTILES
ffm <- g_all_percentiles %>% 
  # filter out stuff:
  filter(!grepl("Julian", metric), !grepl("X__", metric))


# POR: join data
bmi_ffm_por <- left_join(bmi_csci_por_trim, ffm, by=c("gage_id"="gage_id", "metric"="metric"))

# add proper names and RI's here:

bmi_ffm_por <- left_join(bmi_ffm_por, ri_table[,c(1:2,6:11)], by=c("metric"="var"))


# SET UP CSCI PLOTS -------------------------------------------------------

# breaks w biological sig (Mazor et al. 2016)
csci_breaks <- c(0, 0.25, 0.5, 0.63, 0.79, 0.92)
csci_labs <- c("Very likely altered", "Likely altered", "Possibly altered","Likely intact")

# SP ROC -----------------------------------------------------------------

# select metric
metselect <- "SP_ROC"

# data 
plotdat <- bmi_ffm_por %>% 
  filter(model=="all_ca", 
         !is.na(huc_region),
         result_type=="observed",
         metric==metselect) %>% 
  st_drop_geometry() %>% 
  select(StationCode,gage_id, list_id, NHDV2_COMID, comid_bmi, csci, csci_percentile, huc_region,gage_id_c, metric:status_code, p10:p90, result_type:Flow.Metric.Name) %>% 
  distinct()

# POR for SP_ROC:
# Trendline info:
# for gam use: method = "gam", formula = y ~ s(x, bs = "cs").
# for loess use method = "loess", span=1.5, (default span = 0.75), width of the moving window, or
# proportion of points in plot which influence the smooth at each value.
# each of the local regressions used to produce that curve incorporate x% of total data 
# (default is 75%)

(gg1a <- 
   ggplot() +
    # 10/90 percentile pts
    #geom_point(data=plotdat, aes(x=p10, y=csci), color="gray80", pch=22, size=1.5, alpha=0.9, show.legend = F) +
    #geom_point(data=plotdat, aes(x=p90, y=csci), color="steelblue", pch=22, size=1.5, alpha=0.9, show.legend = F) +
    # add the CSCI biological/stream condition thresholds (Mazor et al 2016)
    geom_rect(aes(xmin=0.01, xmax=0.5, ymin=0, ymax=0.63), fill="maroon", alpha=0.2) +
    geom_rect(aes(xmin=0.01, xmax=0.5, ymin=0.63, ymax=0.79), fill="orange2", alpha=0.2) +
    geom_rect(aes(xmin=0.01, xmax=0.5, ymin=0.79, ymax=0.92), fill="yellow2", alpha=0.2)+
    geom_rect(aes(xmin=0.01,xmax=0.5, ymin=0.92, ymax=1.35), fill="seagreen", alpha=0.2)+
    annotate(geom = "text", label="Very likely altered", color="gray50", x=0.015, y=0.58) +
    annotate(geom = "text", label="Likely altered", color="gray50", x=0.015, y=0.71) +
    annotate(geom = "text", label="Possibly altered", color="gray50", x=0.015, y=0.85) +
    annotate(geom = "text", label="Likely intact", color="gray50", x=0.015, y=1) +
    
    # data points
    geom_point(data=plotdat, aes(x=p50, y=csci), fill="gray10", pch=21, size=2.5, alpha=0.85, show.legend = FALSE) +

    #stat_smooth(data=plotdat, aes(x=p10, y=csci), method = "loess", span=.95, lty=2, color="gray40",se = FALSE, fill="gray80", show.legend = F)+
    # the smooth line
    stat_smooth(data=plotdat, aes(x=p50, y=csci), method = "gam", formula = y ~ s(x, bs = "cs"), color="gray40", fill="gray80", show.legend = F)+
    # all the other stuff
    scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
    scale_x_log10(expand=c(0.01,0.01), limits=c(0.01, 0.5)) +
    theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    labs(y="CSCI Score", 
         x=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         #title = "Period of Record",
         subtitle = "Period of Record (50th percentile)"))

ggsave("figs/10_ffm_vs_top_ri_all_ca_sp_roc_por_gam_shading.png", width = 11, height = 7, dpi=300, units="in")
ggsave("figs/10_ffm_vs_top_ri_all_ca_sp_roc_por_gam_shading.pdf", width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)

(gg3b <- ggplot(data=bmi_ffm_por %>% filter(metric=="SP_ROC", 
                                            !is.na(huc_region)), 
                aes(y=csci, x=ffm_value, fill=huc_region)) + 
    scale_fill_colorblind("HUC Region")+
    scale_x_log10() +
    geom_point(pch=21, size=1.5, alpha=0.5, show.legend = F) +
    #geom_smooth(method = "lm", color="gray", fill="gray80", show.legend = F)+
    geom_smooth(method = "gam", color="gray20", formula = y ~ s(x, bs = "cs"), fill="gray", show.legend = FALSE) +
    scale_color_colorblind("HUC Region")+
    theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    labs(y="CSCI", x="SP_ROC", title="SP_ROC", subtitle = "by HUC region")+
facet_wrap(huc_region~.))

ggsave("models/10_ffm_vs_top_ri_all_ca_sp_roc_POR_by_huc_region_gam.png", width = 11, height = 7, dpi=300, units="in")

# DS_MAG_90 ---------------------------------------------------------------

# POR by region
(gg2a <- ggplot(data=bmi_ffm_por %>% filter(metric=="DS_Mag_90", 
                                            !is.na(huc_region), ffm_value>0), 
                aes(y=csci, x=ffm_value, fill=huc_region)) + 
   scale_fill_colorblind("HUC Region")+
   scale_x_log10(breaks=c(0.1, 10, 100, 1000), labels=c(0.1, 10, 100, 1000)) +
   geom_point(pch=21, size=1.5, alpha=0.5, show.legend = F) +
   #geom_smooth(method = "lm", color="gray50", fill="gray", show.legend = F)+
   geom_smooth(method = "gam", color="gray20", formula = y ~ s(x, bs = "cs"), fill="gray", show.legend = FALSE) +
   scale_color_colorblind("HUC Region")+
   theme_clean(base_family = "Roboto Condensed") +
   theme(panel.border = element_blank(),
         plot.background = element_blank()) +
   labs(y="CSCI", x=" DS_MAG_90 (cfs)", title="DS_Mag_90", subtitle = plotname))#
#facet_wrap(huc_region~.))
ggsave("models/10_ffm_vs_top_ri_all_ca_ds_mag_90_POR_gam.png", width = 11, height = 7, dpi=300, units="in")

(gg2b <- ggplot(data=bmi_ffm_por %>% filter(metric=="DS_Mag_90", 
                                            !is.na(huc_region), ffm_value>0), 
                aes(y=csci, x=ffm_value, fill=huc_region)) + 
    scale_fill_colorblind("HUC Region")+
    scale_x_log10(breaks=c(0.1, 10, 100, 1000), labels=c(0.1, 10, 100, 1000)) +
    geom_point(pch=21, size=1.5, alpha=0.5, show.legend = F) +
    #geom_smooth(method = "lm", color="gray50", fill="gray", show.legend = F)+
    geom_smooth(method = "gam", color="gray20", formula = y ~ s(x, bs = "cs"), fill="gray", show.legend = FALSE) +
    scale_color_colorblind("HUC Region")+
    theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    labs(y="CSCI", x="DS_Mag_90 (cfs)", title="DS_Mag_90", subtitle = "by HUC region") +
    facet_wrap(huc_region~.))
ggsave("models/10_ffm_vs_top_ri_all_ca_ds_mag_90_POR_by_huc_region_gam.png", width = 11, height = 7, dpi=300, units="in")


# plot
(gg2 <- ggplot(data=bmi_ffm_ann %>% filter(ffm_metric=="DS_Mag_90", 
                                           !is.na(huc_region)), 
               aes(x=csci, y=ffm_value+0.1, fill=huc_region)) + 
    scale_fill_colorblind("HUC Region")+
    scale_y_log10() +
    geom_point(pch=21, size=4, show.legend = F) +
    # using a spline here
    #geom_smooth(method = "lm", aes(color=status), fill="gray",  
    #show.legend = F)+
    geom_smooth(method = "gam", color="steelblue", formula = y ~ s(x, bs = "cs"), fill="gray", show.legend = FALSE) +
    scale_color_colorblind("HUC Region")+
    theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    labs(x="CSCI", y="log(FFM Value)", title="DS_Mag_50", subtitle="by HUC region")+
    facet_wrap(huc_region~.))

# plot
(gg2b <- ggplot(data=bmi_ffm_ann %>% 
                  filter(ffm_metric=="DS_Mag_50", !status=="not_enough_data"),
                aes(x=csci, y=ffm_value+0.1, fill=status)) + 
    scale_fill_viridis_d("Alt. Status", direction = -1) +
    scale_y_log10() +
    geom_point(pch=21, size=4, show.legend = F) +
    geom_smooth(method = "lm", color="steelblue", fill="gray",  
                show.legend = F)+
    labs(x="CSCI", y="log(FFM Value)", title="DS_Mag_50", subtitle="by alteration status")+
    scale_color_viridis_d("Alt. Status", direction = -1) +
    theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    facet_wrap(.~status))


cowplot::plot_grid(gg2, gg2b, nrow=1, labels="AUTO")
ggsave("models/10_ffm_vs_top_ri_all_ca_ds_mag_50.png", width = 11, height = 7, dpi=300, units="in")


# SP_ROC w LAG ------------------------------------------------------------------


(gg3c <- ggplot(data=bmi_ffm_lag1 %>% 
                  filter(ffm_metric=="SP_ROC", !status=="not_enough_data"),
                aes(x=csci, y=ffm_value, fill=status)) + 
   #scale_fill_viridis_d("Alt. Status", direction = -1) +
   scale_y_log10() +
   geom_point(pch=21, size=4, show.legend = F) +
   geom_smooth(method = "gam", color="gray40", show.legend = FALSE) +
   labs(x="CSCI", y="log(FFM Value)", title="SP_ROC: Lag 1", subtitle="by alteration status")+
   scale_fill_colorblind("Alteration Status") +
   #scale_color_viridis_d("Alt. Status", direction = -1) +
   theme_clean(base_family = "Roboto Condensed") +
   theme(panel.border = element_blank(),
         plot.background = element_blank()) +
   facet_grid(.~status))

(gg3d <- ggplot(data=bmi_ffm_lag2 %>% 
                  filter(ffm_metric=="SP_ROC", !status=="not_enough_data"),
                aes(x=csci, y=ffm_value, fill=status)) + 
    scale_fill_viridis_d("Alt. Status", direction = -1) +
    scale_y_log10() +
    geom_point(pch=21, size=4, show.legend = F) +
    geom_smooth(method = "gam", color="gray40", show.legend = FALSE) +
    labs(x="CSCI", y="log(FFM Value)", title="SP_ROC: Lag 2", subtitle="by alteration status")+
    scale_fill_colorblind("Alteration Status") +
    #scale_color_viridis_d("Alt. Status", direction = -1) +
    theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    facet_grid(.~status))

plot_grid(gg3b, gg3c, gg3d, ncol=3)

# FA_MAG ------------------------------------------------------------------


(gg5 <- ggplot(data=bmi_ffm_ann %>% filter(metric=="FA_Mag"), 
               aes(x=csci, y=ffm_value)) + 
   geom_point(pch=21, color="forestgreen") +
   geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
               color="forestgreen")+
   #scale_x_continuous(breaks = seq(0, 1.2, 0.2), limits = c(0,1.2)) +
   labs(x="CSCI", subtitle = "FA_Mag: Annual data")+
   theme_minimal())


(gg5b <- ggplot(data=bmi_ffm_por %>% filter(metric=="FA_Mag"), 
               aes(x=csci, y=ffm_value)) + 
    geom_point(pch=21, color="forestgreen") +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
                color="forestgreen")+
    scale_x_continuous(breaks = seq(0, 1.2, 0.2)) +
    labs(x="CSCI", subtitle = "FA_Mag: POR data")+
    theme_minimal())

# Cowplot them together ---------------------------------------------------

library(cowplot)

plot_grid(gg1, gg2, gg3, gg4, gg5, ncol=3)
plot_grid(gg1b, gg2b, gg3b, gg4b, gg5b, ncol=3)
