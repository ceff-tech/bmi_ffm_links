# 09 Id top flow variables from BRT


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(leaflet)
library(mapview)

# Data --------------------------------------------------------------------

# get data from BRT outputs
load("data_output/gbm_bmi_metrics_RI_combined_noSC.rda")
load("data_output/selected_bmi_stations_w_comids.rda")
load("data_output/mainstems_bmi_selected_gages.rda")


# Plot/Summarize ----------------------------------------------------------

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
  
  
  # CDEC SNOW STATIONS
  addCircleMarkers(data=bmi_coms, group="BMI Coms",
                   lng=~lon, lat=~lat,
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


