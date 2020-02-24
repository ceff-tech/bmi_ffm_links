# match nearest BMI site to nearest USGS site

library(tidyverse)
library(sf)
library(lubridate)
library(viridis)
library(here)
library(mapview)

# Data --------------------------------------------------------------------

bmi_csci_por <- read_rds("data_output/04_selected_bmi_stations_w_csci_flow_por.rds")

# the spatially joined points
sel_bmi_gages <- readRDS("data_output/02_selected_bmi_h12_all_gages.rds")
sel_gages_bmi <- readRDS("data_output/02_selected_usgs_h12_all_gages.rds") 
sel_h12 <- read_rds("data_output/02_selected_h12_all_gages.rds")

# nhd streamlines
load("data_output/02_selected_nhd_mainstems_all_gages.rda") # mainstems_all

# Filter to Final Sites ---------------------------------------------------

mainstems_final <- mainstems_all %>% filter(nhdplus_comid %in% bmi_csci_por$comid)
mainstems_final_gages <- mainstems_all %>% filter(gageID %in% bmi_csci_por$ID)
save(mainstems_final, file="data_output/05_selected_mainstems_final.rda")

gages_final <- sel_gages_bmi %>% filter(site_id %in% bmi_csci_por$ID)

# Quick Map Summary -------------------------------------------------------

# 142 specific BMI stations (unique), 34 H12s, 46 USGS Gages
mapview(gages_final, col.regions="cyan2", cex=4, col.alpha=0.5) +
  mapview(bmi_csci_por, zcol="csci", cex=6)+
  mapview(mainstems_final_gages, color="gray80", cex=2.5, alpha=0.4)+
  mapview(mainstems_final, color="darkblue", cex=2.5)

# Find Closest Sites Along Streamline -------------------------------------

# first convert to UTM (metric)
gages_final_3310 <- st_transform(gages_final, 3310)
mainstems_final_3310 <- st_transform(mainstems_final, 3310)
bmi_csci_por_3310 <- st_transform(bmi_csci_por, 3310)

# find nearest COMID
#bmi_nearest <- st_nearest_feature(bmi_coms_3310, mainstems_3310)
#bmi_nearest <- mainstems_3310[bmi_nearest,]

# find nearest GAGE: first search with st_nearest
bmi_nearest <- st_nearest_feature(gages_final_3310, bmi_csci_por_3310)
# now use that index of nearest for each gage in the data
bmi_nearest <- bmi_csci_por_3310[bmi_nearest,] # should match no. of gages

# map
mapview(gages_final, col.regions="blue", cex=6) + 
  mapview(bmi_csci_por, col.regions="gray", cex=4) +
  mapview(bmi_nearest, col.regions="orange", cex=3)
  
# find distance from bmi sites to usgs sites
# tst <- st_distance(sel_gages_bmi, bmi_coms, by_element = F) %>% as_tibble()
# https://github.com/r-spatial/sf/issues/799

# save out the "NEAREST" site
saveRDS(bmi_nearest, file="data_output/05_selected_bmi_csci_por_nearest_gage.rds")

# Link Regions ------------------------------------------------------------

# read in fish regions:
ca_sp_regions <- read_sf("data/spatial/umbrella_sp_regions.shp", as_tibble = T)

# save back out for easier spatial:
#save(ca_sp_regions, file = "data/07_umbrella_sp_regions.rda")

# spatial join gage sites with regions, adds ATTRIBUTES, retains ALL pts if left=TRUE,
gages_final_3310 <- st_join(gages_final_3310, left = TRUE, ca_sp_regions["huc_region"])

mapview(gages_final_3310, zcol="huc_region") + 
  mapview(ca_sp_regions, alpha=0.2)+
  mapview(bmi_nearest, col.regions="orange", cex=6)
