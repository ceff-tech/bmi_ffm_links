# 10_model_top_metrics


library(tidyverse)
library(viridis)
library(sf)
library(purrr)
library(rlang)
library(ggthemes)
library(tidylog)

# Load Data ---------------------------------------------------------------

# orig data
load("data_output/07_selected_bmi_csci_por_trim_w_huc_region.rda")

# simple just samples:
bmi_sampleid <- bmi_csci_por_trim %>% st_drop_geometry() %>% 
  dplyr::distinct(SampleID, .keep_all = TRUE) %>% 
  select(StationCode:csci, gage_id_c:median_in_iqr, huc_region)

# Load Data --------------------------------------------------------------------

## VARIABLES:
# "all_ca_ffc_only"
# "central_valley", "great_basin", "north_coast", "south_coast", 

hydroDat <- "POR"
modname <- "all_ca_ffc_only" # model name 
bmiVar <- quote(csci) # select response var

# make pathnames
(mod_pathname <- paste0("07_gbm_final_", tolower(bmiVar), "_",tolower(hydroDat), "_",modname))
(mod_savename <- tolower(paste0("08_gbm_", as_name(bmiVar), "_",hydroDat, "_",modname)))

# get the gbm model:
(top_ri <- list.files(path="models/", pattern = paste0("^", mod_savename,"_RI_combined",".*\\.rds$")))

top_ris <- read_rds(path=paste0("models/", top_ri))

# get model datasets (for PDPs)
load(paste0("models/",mod_pathname, "_model_data.rda"))


# Get FFC Data and Join ---------------------------------------------------

# load FFC Metrics Data
load("data_output/05_all_alt_ffc.rda")

g_all_ffc %>% group_by(gage_id, Year) %>% distinct()

ffm <- g_all_ffc %>% mutate(Year=as.integer(Year)) %>% 
  distinct() # filter out duplication
rm(g_all_ffc)

# make it long not wide for joins:
ffm <- pivot_longer(ffm, cols= c(DS_Dur_WS:Peak_Fre_5), names_to = "ffm_metric", values_to = "ffm_value") %>% 
  # drop nas
  filter(!is.na(ffm_value))

# join for POR
#bmi_ffm_por <- left_join(bmi_csci_por_trim, ffm, by=c("gage_id_c"="gage_id", "metric"="ffm_metric"))

# join for ann data:
bmi_ffm_ann <- left_join(bmi_sampleid, ffm, by=c("gage_id_c"="gage_id", "YYYY"="Year"))

# years -1  -2
lag_yrs_1 <- unique(bmi_sampleid$YYYY) - 1
lag_yrs_2 <- unique(bmi_sampleid$YYYY) - 2

# make lag data
ffm_lag1 <- ffm %>% filter(Year %in% lag_yrs_1) %>% 
  mutate(year_flow = Year-1) # add for labeling purposes
ffm_lag2 <- ffm %>% filter(Year %in% lag_yrs_2) %>% 
  mutate(year_flow = Year-2) # add for labeling purposes

# rejoin
bmi_ffm_lag1 <- left_join(bmi_sampleid, ffm_lag1, by=c("gage_id_c"="gage_id", "YYYY"="Year"))
bmi_ffm_lag2 <- left_join(bmi_sampleid, ffm_lag2, by=c("gage_id_c"="gage_id", "YYYY"="Year"))


# PEAK 5 ------------------------------------------------------------------

# look at Peak_5
# bmi_ffm_ann %>% filter(ffm_metric=="Peak_5") %>%
#   filter(!is.na(ffm_value)) %>% View()


# plot
(gg1 <- ggplot(data=bmi_ffm_ann %>% filter(ffm_metric=="Peak_5", 
                                           !is.na(huc_region)), 
               aes(x=csci, y=ffm_value, fill=huc_region)) + 
    scale_fill_colorblind("HUC Region")+
    scale_y_log10() +
    geom_point(pch=21, size=4, show.legend = F) +
    # using a spline here
    geom_smooth(method = "gam", aes(color=huc_region), formula = y ~ s(x, bs = "cs"), fill="gray", show.legend = FALSE) +
   scale_color_colorblind("HUC Region")+
   theme_clean(base_family = "Roboto Condensed") +
   labs(x="CSCI", y="log(FFM Value)", title="Peak 5", subtitle="by HUC region") +
   facet_wrap(huc_region~.))

# plot
(gg1b <- ggplot(data=bmi_ffm_ann %>% 
                  filter(ffm_metric=="Peak_5", !status=="not_enough_data"),
                aes(x=csci, y=ffm_value, fill=status)) + 
    scale_fill_viridis_d("Alt. Status", direction = -1) +
    scale_y_log10() +
    geom_point(pch=21, size=4, show.legend = F) +
    geom_smooth(method = "lm", aes(color=status), fill="gray",  
                show.legend = F)+
    labs(x="CSCI", y="log(FFM Value)", title="Peak 5")+
    scale_color_viridis_d("Alt. Status", direction = -1) +
    theme_clean(base_family = "Roboto Condensed") +
    facet_grid(.~status))


cowplot::plot_grid(gg1, gg1b, nrow=2)
ggsave("models/10_ffm_vs_top_ri_all_ca_peak_5.png", width = 8, height = 11, dpi=300, units="in")

# DS_MAG_50 ---------------------------------------------------------------

#bmi_ffm_ann %>% filter(metric=="DS_Mag_50") %>% view()

# plot
(gg2 <- ggplot(data=bmi_ffm_ann %>% filter(ffm_metric=="DS_Mag_50", 
                                           !is.na(huc_region)), 
               aes(x=csci, y=ffm_value+0.1, fill=huc_region)) + 
   scale_fill_colorblind("HUC Region")+
   scale_y_log10() +
   geom_point(pch=21, size=4, show.legend = F) +
   # using a spline here
   #geom_smooth(method = "lm", aes(color=status), fill="gray",  
               #show.legend = F)+
   geom_smooth(method = "gam", aes(color=huc_region), formula = y ~ s(x, bs = "cs"), fill="gray", show.legend = FALSE) +
   scale_color_colorblind("HUC Region")+
   theme_clean(base_family = "Roboto Condensed") +
   labs(x="CSCI", y="log(FFM Value)", title="DS_Mag_50")+
   facet_wrap(huc_region~.))

# plot
(gg2b <- ggplot(data=bmi_ffm_ann %>% 
                  filter(ffm_metric=="DS_Mag_50", !status=="not_enough_data"),
                aes(x=csci, y=ffm_value+0.1, fill=status)) + 
    scale_fill_viridis_d("Alt. Status", direction = -1) +
    scale_y_log10() +
    geom_point(pch=21, size=4, show.legend = F) +
    geom_smooth(method = "lm", aes(color=status), fill="gray",  
                show.legend = F)+
    labs(x="CSCI", y="log(FFM Value)", title="DS_Mag_50")+
    scale_color_viridis_d("Alt. Status", direction = -1) +
    theme_clean(base_family = "Roboto Condensed") +
    facet_wrap(.~status))


cowplot::plot_grid(gg2, gg2b, nrow=2)
ggsave("models/10_ffm_vs_top_ri_all_ca_ds_mag_50.png", width = 8, height = 11, dpi=300, units="in")

# SP ROC -----------------------------------------------------------------

# plot
(gg3 <- ggplot(data=bmi_ffm_ann %>% filter(ffm_metric=="SP_ROC", 
                                           !is.na(huc_region)), 
               aes(x=csci, y=ffm_value, fill=huc_region)) + 
   scale_fill_colorblind("HUC Region")+
   scale_y_log10() +
   geom_point(pch=21, size=4, show.legend = F) +
   #geom_smooth(method = "lm", color="darkgray", fill="gray", show.legend = F)+
   geom_smooth(method = "gam", color="steelblue", formula = y ~ s(x, bs = "cs"), fill="gray", show.legend = FALSE) +
   scale_color_colorblind("HUC Region")+
   theme_minimal(base_family = "Roboto Condensed") +
   labs(x="CSCI", y="log(FFM Value)", title="SP_ROC", subtitle = "by HUC Region") +
   facet_wrap(huc_region~.))

# plot
(gg3b <- ggplot(data=bmi_ffm_ann %>% 
                  filter(ffm_metric=="SP_ROC", !status=="not_enough_data"),
                aes(x=csci, y=ffm_value+0.1, fill=status)) + 
    scale_fill_viridis_d("Alt. Status", direction = -1) +
    scale_y_log10() +
    geom_point(pch=21, size=4, show.legend = F) +
    geom_smooth(method = "lm", color="steelblue", fill="gray",  
                show.legend = F)+
    labs(x="CSCI", y="log(FFM Value)", title="SP_ROC", subtitle="by Alteration Status")+
    scale_color_viridis_d("Alt. Status", direction = -1) +
    theme_minimal(base_family = "Roboto Condensed") +
    facet_grid(.~status))


cowplot::plot_grid(gg3, gg3b, nrow=2)
ggsave("models/10_ffm_vs_top_ri_all_ca_sp_roc.png", width = 8, height = 11, dpi=300, units="in")



# SP_ROC ------------------------------------------------------------------


(gg3 <- ggplot() + 
   geom_point(data=bmi_ffm_por %>% filter(metric=="SP_ROC"), 
              aes(x=csci, y=ffm_value, fill=as.factor(status_code)), 
              pch=21, size=3) +
   ggthemes::scale_fill_colorblind("Status Code") +
   geom_smooth(data=bmi_ffm_ann %>% filter(metric=="SP_ROC"), 
               aes(x=csci, y=ffm_value), 
               method = "gam", 
               #formula = y ~ s(x, bs = "cs"),  
               color="purple2")+
   theme_minimal() +
   labs(title="Spring Recession Rate", y="Spring Recession Rate", x="CSCI"))

(gg3b <- ggplot() + 
    geom_point(data=bmi_ffm_ann %>% filter(metric=="SP_ROC"), 
               aes(x=csci, y=ffm_value, fill=as.factor(status_code)), 
               pch=21, size=3) +
    ggthemes::scale_fill_colorblind("Status Code") +
    geom_smooth(data=bmi_ffm_ann %>% filter(metric=="SP_ROC"), 
                aes(x=csci, y=ffm_value), 
                method = "gam", 
                #formula = y ~ s(x, bs = "cs"),  
                color="purple4")+
    theme_minimal() +
    labs(title="Spring Recession Rate: Ann"))

(gg3c <- ggplot() + 
    geom_point(data=bmi_ffm_lag1 %>% filter(metric=="SP_ROC"), 
               aes(x=csci, y=ffm_value, fill=as.factor(status_code)), 
               pch=21, size=3) +
    ggthemes::scale_fill_colorblind("Status Code") +
    geom_smooth(data=bmi_ffm_lag1 %>% filter(metric=="SP_ROC"), 
                aes(x=csci, y=ffm_value), 
                method = "gam", 
                #formula = y ~ s(x, bs = "cs"),  
                color="purple4")+
    theme_minimal() +
    labs(title="Spring Recession Rate: Lag1"))

(gg3d <- ggplot() + 
    geom_point(data=bmi_ffm_lag2 %>% filter(metric=="SP_ROC"), 
               aes(x=csci, y=ffm_value, fill=as.factor(status_code)), 
               pch=21, size=3) +
    ggthemes::scale_fill_colorblind("Status Code") +
    geom_smooth(data=bmi_ffm_lag2 %>% filter(metric=="SP_ROC"), 
                aes(x=csci, y=ffm_value), 
                method = "gam", 
                #formula = y ~ s(x, bs = "cs"),  
                color="purple4")+
    theme_minimal() +
    labs(title="Spring Recession Rate: Lag2"))



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
