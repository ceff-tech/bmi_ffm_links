# 04 Generate Final Selected Sites/Data
## R. Peek
## Look at final output


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
library(lubridate)


# Load Data ---------------------------------------------------------------

# FISH REGIONS
ca_sp_regions <- read_sf("data/spatial/umbrella_sp_regions.shp", as_tibble = T)

# BMI SITES
load("data_output/03_selected_final_bmi_stations_dat_all_gages.rda")

# BMI
sel_bmi_gages_csci <- readRDS("data_output/03_selected_bmi_h12_all_gages_csci.rds") # sel_bmi_gages w csci scores

sel_gages_bmi <- readRDS("data_output/03_selected_usgs_h12_all_gages.rds")
sel_h12_bmi <- readRDS("data_output/03_selected_h12_all_gages.rds")
load("data_output/03_selected_nhd_mainstems_gages.rda")

ca_sp_regions <- read_sf("data/spatial/umbrella_sp_regions.shp", as_tibble = T)

# Set up Mapview Basemap --------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Make Mapview of Selected Gages and BMI Stations ----------------------

# not selected bmi
bmi_not_selected_v2 <- sel_bmi_gages_csci %>% filter(!as.character(StationCode) %in% sel_bmi_coms_final_v2$StationCode) # n=203

# get all gages selected (n=160)
gages_selected_v2 <- sel_gages_bmi %>% 
  filter(ID %in% sel_bmi_coms_final_v2$ID)

# get the gages not selected (n=47)
gages_not_selected_v2 <- sel_gages_bmi %>% 
  filter(!ID %in% sel_bmi_coms_final_v2$ID)

table(gages_selected_v2$CEFF_type) # ALT=116  REF=44

# this map of all sites selected U/S and D/S
m3 <- mapview(sel_bmi_coms_final_v2, cex=6, col.regions="orange", 
              layer.name="Selected BMI comids") +  
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(gages_selected_v2, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all bmi or gages in same H12 but not selected
  mapview(gages_not_selected_v2, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(bmi_not_selected_v2, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# Tmap --------------------------------------------------------------------

library(tmap)
library(USAboundaries)
ca<-us_counties(states="ca")
load("data_output/major_rivers_dissolved.rda")

# make a tmap
tm_shape(ca) + 
  tm_polygons() +
  tm_shape(rivs) + tm_lines(col="darkblue", lwd=0.7, alpha=0.8) +
  tm_shape(sel_bmi_coms_final_v2) +
  tm_dots(col = "orange", shape = 21, size = 0.2, alpha=0.8) + 
  tm_layout(title = "BMI Sites\n in CA", legend.show = FALSE, frame = FALSE, fontfamily = "Roboto Condensed", title.position = c(0.7, 0.7)) +
  tm_compass(type = "4star", position = c("right","top"))+
  tm_scale_bar(position = c("left","bottom"))

tmap::tmap_save(filename = "figs/03_map_tmap_selected_bmi_sites_w_rivers.png", width = 8, height = 11, units = "in", dpi = 300)  

# make paired sites
tm_shape(ca) + 
  tm_polygons(alpha = 0.2) +
  tm_shape(rivs) + tm_lines(col="darkblue", lwd=0.7, alpha=0.8) +
  tm_shape(sel_bmi_coms_final_v2) +
  tm_dots(col = "gold1", shape = 21, size = 0.5, alpha=0.8, title="BMI Sites", legend.show = TRUE, legend.is.portrait = TRUE) + 
  tm_shape(gages_selected_v2) +
  tm_dots(col = "CEFF_type", shape=21, size=0.2, alpha=0.8, palette=c( "#440154FF","steelblue"), 
          title="USGS Gage", legend.show = TRUE) +
  tm_layout(title = "BMI Sites (orange)\n& USGS Gage Pairs",
            frame = FALSE, fontfamily = "Roboto Condensed",
            title.position = c(0.65, 0.7)) +
  tm_compass(type = "4star", position = c("right","top"))+
  tm_scale_bar(position = c("left","bottom"))

tmap::tmap_save(filename = "figs/03_map_tmap_selected_bmi_usgs_sites_w_rivers.png", width = 8, height = 11, units = "in", dpi = 300)  

# tmaptools::palette_explorer()
# tm_shape(bmi_coms_final) +
#   tm_symbols(shape = 21, col = "h12_area_sqkm", n=5, pal="-Greens") #reverse the palette

# View Final Tally --------------------------------------------------------

# any NA's?
bmi_coms_dat %>% st_drop_geometry %>% filter(is.na(StationCode)) # nope

# now look at how many unique samples are avail: n=270 unique samples
bmi_coms_dat %>% st_drop_geometry %>% distinct(SampleID) %>% tally
# total distinct stations 270

# how many unique USGS gages? n=160 (ALT=116, REF=44)
bmi_coms_dat %>% st_drop_geometry %>% distinct(ID, .keep_all=TRUE) %>% count(CEFF_type)

