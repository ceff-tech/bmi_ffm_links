# 10 Id top flow variables from BRT


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(leaflet)
library(mapview)
library(viridis) # colors
library(gbm) # boosted regression trees
library(dismo)
library(pdp)
library(rlang)


# Data --------------------------------------------------------------------

# get data from GBM outputs:
(brt <- list.files(path="data_output/gbms", pattern = "^10_gbm_final.*\\.rds$"))

gbm_final <- read_rds(path=paste0("data_output/gbms/", brt))

# get hydrodatasets (for PDPs)
load("data_output/gbms/10_gbm_final_csci_percentile_hydrodata.rda")

## VARIABLES:
hydroDat <- "Annual" # can be Annual, Lag1, Lag2, POR
bmiVar <- quote(csci_percentile) # select response var from list above

#load("data_output/08_gbm_bmi_metrics_RI_combined_noSC.rda")
load("data_output/05_selected_bmi_stations_w_comids.rda")
load("data_output/07_mainstems_bmi_selected_gages.rda")


# Plot & Summarize All RI Combined ----------------------------------------

# most common hydrometric by flowdata type?
bmi_RI_combined %>% group_by(flowdat, var) %>% 
  summarize(meanRI = mean(rel.inf)) %>% 
  top_n(3) %>% 
  arrange(flowdat, desc(meanRI))

# most common hydrometric by response?
bmi_RI_combined %>% group_by(Ymetric, var) %>% 
  summarize(meanRI = mean(rel.inf),
            medianRI = median(rel.inf),
            maxRI = max(rel.inf),
            SD = sd(rel.inf)) %>% 
  top_n(3) %>% 
  arrange(Ymetric, desc(meanRI))

# most common hydrometric across top vars in all response types and all flowdat?
# so best hydrometric across Ymetrics?
bmi_RI_combined %>% group_by(Ymetric, var) %>% 
  summarize(meanRI = mean(rel.inf),
            medianRI = median(rel.inf),
            maxRI = max(rel.inf),
            SD = sd(rel.inf)) %>% 
  top_n(5) %>% group_by(var) %>% tally() %>% arrange(desc(n))

# so best hydrometric across flowdat?
bmi_RI_combined %>% group_by(flowdat, var) %>% 
  summarize(meanRI = mean(rel.inf),
            medianRI = median(rel.inf),
            maxRI = max(rel.inf),
            SD = sd(rel.inf)) %>% 
  top_n(5) %>% group_by(var) %>% tally() %>% arrange(desc(n))


# LEAFLET -------------------------------------------------------------------

library(leaflet)

# Make a leaflet map!
m <- leaflet() %>% addTiles() %>% 
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  
  # add scale bar
  addMeasure(position = "topright",
             primaryLengthUnit = "meters",
             primaryAreaUnit = "sqmeters",
             activeColor = "#3D535D",
             completedColor = "#7D4479") %>%
  
  
  # CDEC BMI STATIONS
  addCircleMarkers(data=bmi_coms, group="BMI Coms",
                   popup=paste0("<strong>","StationID: ","</strong>", 
                                bmi_coms$StationCode, 
                                "<br><strong>", "Lat: ","</strong>", 
                                bmi_coms$lat, 
                                "<br><strong>", "Lon: ","</strong>", 
                                bmi_coms$lon),
                   stroke=TRUE, weight=0.6,radius=4,
                   fillOpacity = 0.5, color="gray",
                   fillColor= "maroon") %>%  
  
  addLayersControl(
    baseGroups = c("ESRI Aerial", "Topo"),
    overlayGroups = c("BMI Coms"),
    options = layersControlOptions(collapsed=T)
  )

m


