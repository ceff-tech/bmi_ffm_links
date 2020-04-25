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

# Load Data ---------------------------------------------------------------

bmi_RI_combined <- readRDS(file = "models/08_gbm_RI_csci_por.rds")

# orig data
load("data_output/07_selected_bmi_csci_por_trim_w_huc_region.rda")

# simple just sites:
bmi_csci_sites <- bmi_csci_por_trim %>% 
  dplyr::distinct(StationCode, ID, .keep_all = TRUE)

# Plot & Summarize All RI Combined ----------------------------------------

# most common hydrometric by flowdata type?
bmi_RI_combined %>% group_by(flowdat, var) %>% 
  summarize(meanRI = mean(rel.inf),
            sumRI = sum(rel.inf)) %>% 
  top_n(5) %>% 
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
    grepl("DS_", var) ~ "Dry-season baseflow",
    grepl("SP_", var) ~ "Spring recession flow",
    grepl("Peak_", var) ~ "Peak flow",
    grepl("Wet_", var) ~ "Wet-season baseflow",
    grepl("FA_", var) ~ "Fall pulse flow",
    TRUE ~ "General"
  ),
  flow_component = factor(flow_component, levels = c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow", "General")),
  var = as.factor(var),
  var = fct_reorder2(var, flow_component, var))

  
levels(bmi_RI_combined$var)
levels(bmi_RI_combined$flow_component)
summary(bmi_RI_combined)

# Summary Plot ------------------------------------------------------------

# Faceted by hydrodat and flow metrics:
bmi_RI_combined %>% group_by(flowdat, var, flow_component) %>% 
  summarize(meanRI = mean(rel.inf)) %>% 
  #top_n(5) %>% 
  arrange(desc(meanRI)) %>% 
  filter(flow_component!="General") %>% 
  ggplot(.) +
  geom_col(aes(x=var,#x=forcats::fct_reorder2(var, flow_component, var),
               y=meanRI, fill=flow_component), color="gray20", lwd=.1,
           position="dodge") +
  coord_flip() +
  scale_fill_viridis_d("Flow Component")+
  labs(x="", y="Mean Relative Inf (%)", subtitle="Top Flow Metrics across all BMI Metrics") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_grid(.~flowdat)

ggsave(filename = "figs/09_faceted_RI_by_flowcomp_hydrodat.png", width = 9, height = 6, units = "in", dpi = 300)

# Faceted by BMI metrics and flow components:
bmi_RI_combined %>% group_by(flowdat, var, Ymetric, flow_component) %>% 
  summarize(meanRI = mean(rel.inf)) %>% 
  #top_n(6) %>% 
  arrange(desc(meanRI)) %>% 
  filter(flow_component!="General", flowdat=="Annual") %>%  
  ggplot(.) +
  geom_col(aes(x=var, y=meanRI, fill=flow_component), color="gray20", lwd=.1, position="dodge") +
  coord_flip() +
  scale_fill_viridis_d("Flow Components")+
  labs(x="", y="Mean Relative Inf (%)", subtitle="ANNUAL: Top Flow Metrics across BMI Metrics") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_grid(.~Ymetric)

ggsave(filename = "figs/faceted_RI_by_flowcomp_bmi_ANNUAL.png", width = 9, height = 6, units = "in", dpi = 300)
  
# Faceted by BMI metrics and flow components:
bmi_RI_combined %>% group_by(flowdat, var, Ymetric, flow_component) %>% 
  summarize(meanRI = mean(rel.inf)) %>% 
 # top_n(6) %>% 
  arrange(desc(meanRI)) %>% 
  filter(flow_component!="General", flowdat=="Lag1") %>%  
  ggplot(.) +
  geom_col(aes(x=var, y=meanRI, fill=flow_component), color="gray20", lwd=.1, position="dodge") +
  coord_flip() +
  scale_fill_viridis_d("Flow Components")+
  labs(x="", y="Mean Relative Inf (%)", subtitle="LAG-1: Top Flow Metrics across BMI Metrics") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_grid(.~Ymetric)

ggsave(filename = "figs/faceted_RI_by_flowcomp_bmi_LAG1.png", width = 9, height = 6, units = "in", dpi = 300)
  
# Faceted by BMI metrics and flow components:
bmi_RI_combined %>% group_by(flowdat, var, Ymetric, flow_component) %>% 
  summarize(meanRI = mean(rel.inf)) %>% 
  # top_n(6) %>% 
  arrange(desc(meanRI)) %>% 
  filter(flow_component!="General", flowdat=="Lag2") %>%  
  ggplot(.) +
  geom_col(aes(x=var, y=meanRI, fill=flow_component), color="gray20", lwd=.1, position="dodge") +
  coord_flip() +
  scale_fill_viridis_d("Flow Components")+
  labs(x="", y="Mean Relative Inf (%)", subtitle="LAG-2: Top Flow Metrics across BMI Metrics") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_grid(.~Ymetric)

ggsave(filename = "figs/faceted_RI_by_flowcomp_bmi_LAG2.png", width = 9, height = 6, units = "in", dpi = 300)



# Mapview -----------------------------------------------------------------

library(mapview)

sel_gages_bmi <- read_rds("data_output/03_selected_usgs_h12_all_gages.rds")

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList)

# filter data
unique(bmi_csci_por_trim$metric)
bmi_peak_5 <- filter(bmi_csci_por_trim, metric=="Peak_5") %>% 
  st_drop_geometry() %>% 
  st_as_sf(., coords=c("longitude", "latitude"), crs=4326, remove=FALSE) 
bmi_sp_roc <- filter(bmi_csci_por_trim, metric=="SP_ROC") %>% 
  st_drop_geometry() %>% 
  st_as_sf(., coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

m1 <- mapview(bmi_peak_5, zcol="status_code", layer.name="Benthos", alpha=0.8, cex=5) #+

m1 <- mapview(bmi_sp_roc, zcol="status", layer.name="Benthos", alpha=0.8, cex=5) #+
  #mapview(sel_gages_bmi, col.regions="blue", layer.name="Gages", cex=3, alpha=0.2)

# add measure option  
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")  
    
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
  addCircleMarkers(data=bmi_csci_por_trim, group="BMI",
                   popup=paste0("<strong>","StationID: ","</strong>", 
                                bmi_csci_por_trim$StationCode, 
                                "<br><strong>", "Lat: ","</strong>", 
                                bmi_csci_por_trim$latitude, 
                                "<br><strong>", "Lon: ","</strong>", 
                                bmi_csci_por_trim$longitude),
                   stroke=TRUE, weight=0.6,radius=4,
                   fillOpacity = 0.5, color="gray",
                   fillColor= "maroon") %>%  
  
  addLayersControl(
    baseGroups = c("ESRI Aerial", "Topo"),
    overlayGroups = c("BMI Coms"),
    options = layersControlOptions(collapsed=T)
  )

m


