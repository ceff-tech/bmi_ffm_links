# 10_model_top_metrics


library(tidyverse)
library(viridis) # colors
library(sf)
#library(rlang)
#library(purrr)

# Load Data ---------------------------------------------------------------

# RI BRT data
bmi_RI_combined <- readRDS(file = "models/08_gbm_RI_csci_por.rds")

# orig data
bmi_csci_por <- read_rds("data_output/05_selected_bmi_stations_w_csci_ffm_alt_por.rds") %>% st_drop_geometry()

# look at Peak5
peak_5 <- filter(bmi_csci_por, metric=="Peak_5") %>% 
  select(StationCode:latitude, ID:comid, sampleyear, csci, metric, status_code)

# get actual data
load("data_output/04_usgs_all_ffc_metrics.rda") 
ffm <- g_all_ffc %>% mutate(Year=as.integer(Year))

# join to orig data
bmi_ffm_ann <- left_join(bmi_csci_por, ffm, by=c("ID"="gage_id", "sampleyear"="Year"))


# PEAK 5 ------------------------------------------------------------------

# plot
ggplot(data=bmi_ffm_ann %>% filter(metric=="Peak_5"), 
       aes(x=csci, y=Peak_5)) + 
  geom_point(pch=21, color="maroon") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),  
              color="maroon")+
  theme_minimal()


# DS_MAG_50 ---------------------------------------------------------------

ggplot(data=bmi_ffm_ann %>% filter(metric=="DS_Mag_50"), 
       aes(x=csci, y=DS_Mag_50)) + 
  geom_point(pch=21, color="steelblue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),  
              color="steelblue")+
  theme_minimal()


# WET_TIM -----------------------------------------------------------------

ggplot(data=bmi_ffm_ann %>% filter(metric=="Wet_Tim"), 
       aes(x=csci, y=Wet_Tim)) + 
  geom_point(pch=21, color="seagreen") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),  
              color="seagreen")+
  theme_minimal()


# SP_ROC ------------------------------------------------------------------

ggplot(data=bmi_ffm_ann %>% filter(metric=="SP_ROC"), 
       aes(x=csci, y=SP_ROC)) + 
  geom_point(pch=21, color="purple2") +
  geom_smooth(method = "gam",
              color="purple2")+
  theme_minimal()


# FA_MAG ------------------------------------------------------------------


ggplot(data=bmi_ffm_ann %>% filter(metric=="FA_Mag"), 
       aes(x=csci, y=FA_Mag)) + 
  geom_point(pch=21, color="forestgreen") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              color="forestgreen")+
  theme_minimal()
