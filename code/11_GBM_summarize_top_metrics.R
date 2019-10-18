# 11 Identify top RI Flow Metrics
# summarize data from all GBMs

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
library(purrr)

# SET VARIABLES:
#hydroDat <- "Annual" # can be Annual, Lag1, Lag2, POR
#bmiVar <- quote(mmi_percentile) # select response var
# Shannon_Diversity, csci_percentile, Intolerant_Percent, mmi_percentile

# Create RI RDS Data -------------------------------------------------------------

# get data from specific BMI GBM outputs:
#(rds_var <- list.files(path="data_output/gbms", pattern = paste0("^10_gbm_RI_", tolower(bmiVar), ".*\\.rds$"), full.names = T))

# Get all GBM RI data:
#(rdss <- list.files(path="data_output/gbms", pattern = paste0("^10_gbm_RI_.*\\.rds$"), full.names = T))

# use purrr to read in all the files
#rds_ri<-purrr::map_df(rdss, readRDS)

# save out
#saveRDS(rds_ri, file = "data_output/gbms/11_gbm_RI_all_vars.rds")

# Load Data ---------------------------------------------------------------

bmi_RI_combined <- readRDS("data_output/gbms/11_gbm_RI_all_vars.rds")
load("data_output/05_selected_bmi_stations_w_comids.rda")
load("data_output/07_mainstems_bmi_selected_gages.rda")

# Plot & Summarize All RI Combined ----------------------------------------

# most common hydrometric by flowdata type?
bmi_RI_combined %>% group_by(flowdat, var) %>% 
  summarize(meanRI = mean(rel.inf),
            sumRI = sum(rel.inf)) %>% 
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


# Add Flow Component Column for Grouping ----------------------------------

flowmets <- unique(bmi_RI_combined$var)

bmi_RI_combined <- bmi_RI_combined %>% 
  mutate(flow_component = case_when(
    grepl("DS_", var) ~ "Dry-season baseflow", #Mag90, Mag10, Tim, Dur_WS, Dur_WSI
    grepl("SP_", var) ~ "Spring recession flow",
    grepl("Peak_", var) ~ "Peak flow",
    grepl("Wet_Tim|Wet_BFL_Mag|Wet_BFL_Dur", var) ~ "Wet-season baseflow",
    grepl("WSI_Dur|WSI_Mag|WSI_Tim", var) ~ "Fall pulse flow"
  ))
  


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


