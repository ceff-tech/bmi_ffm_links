# 09 Id top flow variables from BRT


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(leaflet)
library(mapview)

# Data --------------------------------------------------------------------

# get data from BRT outputs
load("data_output/gbm_bmi_metrics_RI_combined.rda")
load("data_output/selected_bmi_stations_w_comids.rda")
load("data_output/maintems_us_ds_selected_gages.rda")


# Plot/Summarize ----------------------------------------------------------

# most common variable?
bmi_RI_combined %>% group_by(flowdat, var) %>% 
  summarize(meanRI = mean(rel.inf)) %>% 
  top_n(3) %>% 
  arrange(flowdat, desc(meanRI))


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


