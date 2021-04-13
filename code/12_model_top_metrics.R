# 12_glm_model_top_metrics

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
load("data_output/05_bmi_csci_por_trim_ecoreg.rda")

load("models/10_csci_asci_ri_por_trim_all_regions.rda")
load("data_output/11_csci_ffm_ann_trim.rda")

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

# breaks w biological stream condition thresholds (Mazor et al. 2016)
csci_breaks <- c(0, 0.63, 0.79, 0.92)
csci_labs <- c("Very likely altered", "Likely altered", "Possibly altered","Likely intact")

# SP_ROC: ALL SITES -----------------------------------------------------------------

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
    # geom_rect(aes(xmin=0.01, xmax=0.5, ymin=0, ymax=0.63), fill="maroon", alpha=0.2) +
    # geom_rect(aes(xmin=0.01, xmax=0.5, ymin=0.63, ymax=0.79), fill="orange2", alpha=0.2) +
    # geom_rect(aes(xmin=0.01, xmax=0.5, ymin=0.79, ymax=0.92), fill="yellow2", alpha=0.2)+
    # geom_rect(aes(xmin=0.01,xmax=0.5, ymin=0.92, ymax=1.35), fill="seagreen", alpha=0.2)+
    annotate(geom = "text", label="Very likely altered", color="gray50", 
             x=0.011, y=0.58, hjust=0, size=4) +
    annotate(geom = "text", label="Likely altered", color="gray50", 
             x=0.011, y=0.71, hjust=0, size=4) +
    annotate(geom = "text", label="Possibly altered", color="gray50", 
             x=0.011, y=0.85, hjust=0, size=4) +
    annotate(geom = "text", label="Likely intact", color="gray50", 
             x=0.011, y=1, hjust=0, size=4) +
    
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

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)


# SP_ROC: HUC REGIONS FACETED -----------------------------------------------------------------

# FACETED
(gg1b_faceted <- 
   ggplot() +
   annotate(geom = "text", label="Very likely altered", color="gray50", x=0.011, y=0.57, size=3.5, hjust=0) +
   annotate(geom = "text", label="Likely altered", color="gray50", x=0.011, y=0.71, size=3.5,  hjust=0) +
   annotate(geom = "text", label="Possibly altered", color="gray50", x=0.011, y=0.85, size=3.5,  hjust=0) +
   annotate(geom = "text", label="Likely intact", color="gray50", x=0.011, y=1, size=3.5,  hjust=0) +
   
   # for faceted
   geom_point(data=plotdat , aes(x=p50, y=csci, group=huc_region, shape=huc_region, color=huc_region),
              size=3, alpha=0.85, show.legend = FALSE) +
   stat_smooth(data=plotdat %>% filter(huc_region!="great_basin"),
               aes(x=p50, y=csci, group=huc_region, color=huc_region),
               method = "gam", formula = y ~ s(x, bs = "cs"), show.legend = F, se = FALSE) +
   facet_wrap(huc_region~., labeller = labeller(huc_region=c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))) +
   theme(panel.border = element_blank(),
         plot.background = element_blank()) +

   # all the other stuff
   scale_color_colorblind("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   scale_shape_discrete("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
   scale_x_log10(expand=c(0.01,0.01), limits=c(0.01, 0.5)) +
   theme_clean(base_family = "Roboto Condensed") +
   labs(y="CSCI Score", 
        x=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
        title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
        subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_faceted_by_huc_region.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_faceted_by_huc_region.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)



# SP_ROC: HUC REGIONS UNFACETED -------------------------------------------

(gg1b_unfaceted <- 
    ggplot() +
    # geom_rect(data = data.frame(xmin=0.01, xmax=0.5, ymin=0, ymax=0.63),
    #           aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), fill="maroon", alpha=0.2) +
    # geom_rect(data = data.frame(xmin=0.01, xmax=0.5, ymin=0.63, ymax=0.79), 
    #           aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="orange2", alpha=0.2) +
    # geom_rect(data = data.frame(xmin=0.01, xmax=0.5, ymin=0.79, ymax=0.92),
    #           aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="yellow2", alpha=0.2) +
    # geom_rect(data = data.frame(xmin=0.01,xmax=0.5, ymin=0.92, ymax=1.35), 
    #           aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="seagreen", alpha=0.2)+
    annotate(geom = "text", label="Very likely altered", color="gray50", x=0.011, y=0.57, size=3.5, hjust=0) +
    annotate(geom = "text", label="Likely altered", color="gray50", x=0.011, y=0.71, size=3.5,  hjust=0) +
    annotate(geom = "text", label="Possibly altered", color="gray50", x=0.011, y=0.85, size=3.5,  hjust=0) +
    annotate(geom = "text", label="Likely intact", color="gray50", x=0.011, y=1, size=3.5,  hjust=0) +
    
    # for all points (unfaceted)
    geom_point(data=plotdat , aes(x=p50, y=csci, group=huc_region, shape=huc_region, color=huc_region),
               size=2.5, alpha=0.7, show.legend = FALSE) + # switch to size 3 and 0.85 for all
    stat_smooth(data=plotdat %>% filter(huc_region!="great_basin"),
                aes(x=p50, y=csci, group=huc_region, color=huc_region), method = "gam",
                formula = y ~ s(x, bs = "cs"), show.legend = T, se = FALSE) +
   
    # all the other stuff
    scale_color_colorblind("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
    scale_shape_discrete("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
    scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
    scale_x_log10(expand=c(0.01,0.01), limits=c(0.01, 0.5)) +
    theme_clean(base_family = "Roboto Condensed") +
   theme(panel.border = element_blank(),
         plot.background = element_blank(),
         legend.position = c(0.88,0.88)) +
   
    labs(y="CSCI Score", 
         x=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_huc_region.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_huc_region.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)


# DS_MAG_90: ALL SITES -----------------------------------------------------------------

# select metric
metselect <- "DS_Mag_90"

# data 
plotdat <- bmi_ffm_por %>% 
  filter(model=="all_ca", 
         !is.na(huc_region),
         result_type=="observed",
         metric==metselect) %>% 
  st_drop_geometry() %>% 
  select(StationCode,gage_id, list_id, NHDV2_COMID, comid_bmi, csci, csci_percentile, huc_region,gage_id_c, metric:status_code, p10:p90, result_type:Flow.Metric.Name) %>% 
  distinct()

# PLOT
(gg2a <- 
    ggplot() +
    # add the CSCI biological/stream condition thresholds (Mazor et al 2016)
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0, ymax=0.63), fill="maroon", alpha=0.2) +
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0.63, ymax=0.79), fill="orange2", alpha=0.2) +
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0.79, ymax=0.92), fill="yellow2", alpha=0.2)+
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0.92, ymax=1.35), fill="seagreen", alpha=0.2)+
    annotate(geom = "text", label="Very likely altered", color="gray50", 
             x=0.02, y=0.55, size=4.5, hjust=0) +
    annotate(geom = "text", label="Likely altered", color="gray50", 
             x=0.02, y=0.72,  size=4.5, hjust=0) +
    annotate(geom = "text", label="Possibly altered", color="gray50", 
             x=0.02, y=0.86,  size=4.5, hjust=0) +
    annotate(geom = "text", label="Likely intact", color="gray50", 
             x=0.02, y=1,  size=4.5, hjust=0) +
    
    # data points
    geom_point(data=plotdat, aes(x=p50, y=csci), fill="gray10", pch=21, size=2.5, alpha=0.85, show.legend = FALSE) +
    # gam smooth
    stat_smooth(data=plotdat, aes(x=p50, y=csci), method = "gam", formula = y ~ s(x, bs = "cs"), color="gray40", fill="gray80", show.legend = F)+
    # all the other stuff
    scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
    scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000), limits=c(0.01, 5500),
                  labels=c(0.01, 0.1, 1, 10, 100, 1000), expand=c(0.02, 0.02)) +
    scale_color_colorblind("HUC Region")+
    theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    labs(y="CSCI Score", 
         x=paste0(unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), " (cfs)"), 
         title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)

# DS_MAG_90: HUC REGIONS FACETED --------------------------------------------------------

# PLOT
(gg2b_faceted <- 
   ggplot() +
   annotate(geom = "text", label="Very likely altered", color="gray50", 
            x = 0.02, y=0.55, size=4, hjust=0) +
   annotate(geom = "text", label="Likely altered", color="gray50", 
            x=0.02, y=0.72,  size=4, hjust=0) +
   annotate(geom = "text", label="Possibly altered", color="gray50", 
            x=0.02, y=0.86,  size=4, hjust=0) +
   annotate(geom = "text", label="Likely intact", color="gray50", 
            x=0.02, y=1,  size=4, hjust=0) +
   
   # data points w no fill
   geom_point(data=plotdat, aes(x=p50, y=csci, group=huc_region, shape=huc_region, color=huc_region),
              size=3, alpha=0.85, show.legend = FALSE) +
   
   # gam smooth
   stat_smooth(data=plotdat, aes(x=p50, y=csci, color=huc_region), method = "gam", formula = y ~ s(x, bs = "cs"), show.legend = FALSE, se = FALSE ) +

   facet_wrap(huc_region~., labeller = labeller(huc_region=c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))) +

   # all the other stuff
   scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
   scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000), limits=c(0.01, 5500),
                 labels=c(0.01, 0.1, 1, 10, 100, 1000), expand=c(0.02, 0.02)) +
   scale_color_colorblind("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   scale_shape_discrete("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   theme_clean(base_family = "Roboto Condensed") +
   theme(panel.border = element_blank(),
         plot.background = element_blank())+
   labs(y="CSCI Score", 
        x=paste0(unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), " (cfs)"), 
        title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
        subtitle = "Period of Record (50th percentile)"))
 
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_faceted_by_huc_region.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_faceted_by_huc_region.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)


# DS_MAG_90: HUC REGIONS UNFACETED ------------------------------------------------------

# PLOT
(gg2b_unfaceted <- 
    ggplot() +
    annotate(geom = "text", label="Very likely altered", color="gray50", 
             x = 0.02, y=0.55, size=4, hjust=0) +
    annotate(geom = "text", label="Likely altered", color="gray50", 
             x=0.02, y=0.72,  size=4, hjust=0) +
    annotate(geom = "text", label="Possibly altered", color="gray50", 
             x=0.02, y=0.86,  size=4, hjust=0) +
    annotate(geom = "text", label="Likely intact", color="gray50", 
             x=0.02, y=1,  size=4, hjust=0) +
    
    # data points w no fill
    geom_point(data=plotdat, aes(x=p50, y=csci, group=huc_region, shape=huc_region, color=huc_region),
               size=2.5, alpha=0.7, show.legend = FALSE) +
    
    # gam smooth
    stat_smooth(data=plotdat, aes(x=p50, y=csci, color=huc_region), method = "gam", formula = y ~ s(x, bs = "cs"), show.legend = TRUE, se = FALSE ) +
    
    # all the other stuff
    scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
    scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000), limits=c(0.01, 5500),
                  labels=c(0.01, 0.1, 1, 10, 100, 1000), expand=c(0.02, 0.02)) +
    scale_color_colorblind("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
    scale_shape_discrete("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
    theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank(),
          legend.position = c(0.88,0.88)) +
    labs(y="CSCI Score", 
         x=paste0(unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), " (cfs)"), 
         title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_huc_region.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_huc_region.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)


# WET_BFL_MAG_50: ALL SITES -----------------------------------------------------------------

# select metric
metselect <- "Wet_BFL_Mag_50"

# data 
plotdat <- bmi_ffm_por %>% 
  filter(model=="all_ca", 
         !is.na(huc_region),
         result_type=="observed",
         metric==metselect) %>% 
  st_drop_geometry() %>% 
  select(StationCode,gage_id, list_id, NHDV2_COMID, comid_bmi, csci, csci_percentile, huc_region,gage_id_c, metric:status_code, p10:p90, result_type:Flow.Metric.Name) %>% 
  distinct()

# PLOT
(gg3a <- 
    ggplot() +

    # add the CSCI biological/stream condition thresholds (Mazor et al 2016)
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0, ymax=0.63), fill="maroon", alpha=0.2) +
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0.63, ymax=0.79), fill="orange2", alpha=0.2) +
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0.79, ymax=0.92), fill="yellow2", alpha=0.2)+
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0.92, ymax=1.35), fill="seagreen", alpha=0.2)+
    annotate(geom = "text", label="Very likely altered", color="gray50", 
             x=0.02, y=0.55, size=4, hjust=0) +
    annotate(geom = "text", label="Likely altered", color="gray50", 
             x=0.02, y=0.72, size=4, hjust=0) +
    annotate(geom = "text", label="Possibly altered", color="gray50", 
             x=0.02, y=0.86, size=4, hjust=0) +
    annotate(geom = "text", label="Likely intact", color="gray50", 
             x=0.02, y=1, size=4, hjust=0) +
    
    # data points
    geom_point(data=plotdat, aes(x=p50, y=csci), fill="gray10", pch=21, size=2.5, alpha=0.85, show.legend = FALSE) +
    
    # gam smooth
    stat_smooth(data=plotdat, aes(x=p50, y=csci), method = "gam", formula = y ~ s(x, bs = "cs"), color="gray40", fill="gray80", show.legend = F)+
    # all the other stuff
    scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
    scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000), limits=c(0.01, 5500),
                  labels=c(0.01, 0.1, 1, 10, 100, 1000), expand=c(0.02, 0.02)) +
    scale_color_colorblind("HUC Region")+
    theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    labs(y="CSCI Score", 
         x=paste0(unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), " (cfs)"), 
         title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)

# DS_MAG_90: HUC REGIONS FACETED --------------------------------------------------------

# PLOT
(gg3b_faceted <- 
   ggplot() +

   annotate(geom = "text", label="Very likely altered", color="gray50", 
            x=0.02, y=0.55, size=4, hjust=0) +
   annotate(geom = "text", label="Likely altered", color="gray50", 
            x=0.02, y=0.72, size=4, hjust=0) +
   annotate(geom = "text", label="Possibly altered", color="gray50", 
            x=0.02, y=0.86, size=4, hjust=0) +
   annotate(geom = "text", label="Likely intact", color="gray50", 
            x=0.02, y=1, size=4, hjust=0) +
   
   # data points
   geom_point(data=plotdat, aes(x=p50, y=csci, group=huc_region, shape=huc_region, color=huc_region), size=3, alpha=0.8, show.legend = FALSE) +
   
   # gam smooth
   stat_smooth(data=plotdat, aes(x=p50, y=csci, color=huc_region), method = "gam", formula = y ~ s(x, bs = "cs"), show.legend = FALSE, se = FALSE ) +
   
   facet_wrap(huc_region~., labeller = labeller(huc_region=c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))) +
   
   # all the other stuff
   scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
   scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000), limits=c(0.01, 5500),
                 labels=c(0.01, 0.1, 1, 10, 100, 1000), expand=c(0.02, 0.02)) +
   scale_color_colorblind("HUC Region")+
   theme_clean(base_family = "Roboto Condensed") +
   theme(panel.border = element_blank(),
         plot.background = element_blank()) +
   labs(y="CSCI Score", 
        x=paste0(unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), " (cfs)"), 
        title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
        subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_faceted_by_huc_region.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_faceted_by_huc_region.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)

# DS_MAG_90: HUC REGIONS UNFACETED --------------------------------------------------------

# PLOT
(gg3b_unfaceted <- 
   ggplot() +
   
   annotate(geom = "text", label="Very likely altered", color="gray50", 
            x=0.02, y=0.55, size=4, hjust=0) +
   annotate(geom = "text", label="Likely altered", color="gray50", 
            x=0.02, y=0.72, size=4, hjust=0) +
   annotate(geom = "text", label="Possibly altered", color="gray50", 
            x=0.02, y=0.86, size=4, hjust=0) +
   annotate(geom = "text", label="Likely intact", color="gray50", 
            x=0.02, y=1, size=4, hjust=0) +
   
   # data points
   geom_point(data=plotdat, aes(x=p50, y=csci, group=huc_region, shape=huc_region, color=huc_region), size=2.5, alpha=0.7, show.legend = TRUE) +
   
   # gam smooth
   stat_smooth(data=plotdat, aes(x=p50, y=csci, color=huc_region), method = "gam", formula = y ~ s(x, bs = "cs"), show.legend = TRUE, se = FALSE ) +
   
   # all the other stuff
   scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
   scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000), limits=c(0.01, 5500),
                 labels=c(0.01, 0.1, 1, 10, 100, 1000), expand=c(0.02, 0.02)) +

   scale_color_colorblind("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   scale_shape_discrete("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   
   theme_clean(base_family = "Roboto Condensed") +
   theme(panel.border = element_blank(),
         plot.background = element_blank(),
         legend.position = c(0.88,0.88)) +
   labs(y="CSCI Score", 
        x=paste0(unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), " (cfs)"), 
        title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
        subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_by_huc_region.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_by_huc_region.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)



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

