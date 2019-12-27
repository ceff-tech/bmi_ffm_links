# 03 Spatially Linking BMI with ALL GAGES
## R. Peek 2019
## Spatially link the BMI station data with the USGS gage data using multiple spatial filters

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
#devtools::install_github("USGS-R/nhdplusTools")
library(nhdplusTools)
#library(tmap)

# Load Data ---------------------------------------------------------------

load("data_output/00_bmi_cleaned_all.rda") # all data
load("data_output/00_bmi_cleaned_stations_distinct_xy.rda") # distinct bmi_clean_stations
load("data_output/01_bmi_cleaned_stations_w_site_status.rda") # bmi_clean_stations_ss (includes site status)
load("data/usgs_ca_daily_flow_gages.rda") # all daily flow gages
#load("data_output/02_gages_final_250.rda") # all gages
load("data_output/huc12_sf.rda") # CA h12s

# add watershed area in sqkm
h12 <- h12 %>% 
  mutate(h12_area_sqkm=Shape_Area/1e6) %>% 
  st_transform(4269)
  
# update one col and make spatial
gages <- ca_usgs_gages %>% 
  mutate(end_yr = as.integer(year(date_end)))
  
# Make Data Spatial -------------------------------------------------------

# make spatial
bmi_clean <- bmi_clean %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>% 
  st_transform(4269)

bmi_clean_stations <- bmi_clean_stations %>% 
  st_transform(4269)

bmi_clean_stations_ss <- bmi_clean_stations_ss %>%
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=F) %>%
  st_transform(4269)

# check projs are same
st_crs(bmi_clean)
st_crs(bmi_clean_stations)
st_crs(bmi_clean_stations_ss)
st_crs(gages)

# Gages in Same Time As BMI -----------------------------------------------

# so 1192 gages meet temporal scale, years must be post 1994
gages_final2 <- gages %>% filter(end_yr > 1994)

# plot
mapview(bmi_clean_stations, col.regions="orange", cex=4) + mapview(bmi_clean_stations_ss, zcol="SiteStatus") + 
  mapview(gages_final2, col.regions="skyblue4")

# Intersect BMI/Gages by H12 ----------------------------------------------

# Add H12 to points to BMI and Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE), using BMI_CLEAN ALL DISTINCT STATIONS
bmi_h12 <- st_join(bmi_clean_stations, left = TRUE, h12[c("HUC_12","h12_area_sqkm")])

# Add H12 to all gages
gages_h12 <- st_join(gages_final2, left=TRUE, h12[c("HUC_12")]) %>% 
  select(site_id, HUC_12, lon, lat, elev_m, date_begin, date_end, end_yr) %>% st_drop_geometry()
#class(gages_h12)

# now join based on H12: how many bmi stations vs HUC12s? n=244 stations
sel_bmi_gages <- inner_join(bmi_h12, gages_h12, by="HUC_12") %>% 
  rename(ID=site_id) %>% 
  #distinct(StationCode, .keep_all = T)
  distinct(StationCode, ID, .keep_all = T) # gets 2188 sites!

mapview(sel_bmi_gages)

# number of unique h12s?
length(unique(factor(sel_bmi_gages$HUC_12))) # unique h12=320
length(unique(sel_bmi_gages$ID)) # unique gages=601
length(unique(sel_bmi_gages$StationCode)) # unique BMI Stations=1107

# Get Selected Gages ONLY: 
sel_gages_bmi <- gages_final2 %>% filter(site_id %in% sel_bmi_gages$ID)

# select H12s that have points inside:
sel_h12_bmi <- h12[sel_bmi_gages, ]

# save out
write_rds(sel_h12_bmi, path="data_output/03_selected_allgages_h12_bmi.rds")
write_rds(sel_gages_bmi, path="data_output/03_selected_allgages_gages_bmi.rds")
write_rds(sel_bmi_gages, path="data_output/03_selected_allgages_bmi.rds")

# Mapview -----------------------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList)

m1 <- mapview(sel_bmi_gages, col.regions="orange", layer.name="Benthos", alpha=0.5, cex=9) +
  mapview(sel_gages_bmi, col.regions="blue", layer.name="Gages", cex=4) + 
  mapview(sel_h12_bmi, layer="H12", color="dodgerblue", col.regions="cyan", alpha=0.8, lwd=1, legend=FALSE)

# add measure option  
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# Get BMI COMIDs ----------------------------------------------------------

# join with NHD comids from existing BMI comid list
bmi_comids <- readxl::read_excel("data/BMI_COMIDs_for_Ryan.xlsx")
sel_bmi_gages <- sel_bmi_gages %>% left_join(., bmi_comids, by="StationCode")

summary(sel_bmi_gages$comid) # so missing 356 COMIDs

# GET COMIDS FOR BMI POINTS -----------------------------------

# use this package to look up missing comids and add
library(nhdplusTools)

# get the comid for the BMI points w no comids using purrr
bmi_segs <- sel_bmi_gages %>% filter(is.na(comid)) %>% select(StationCode, latitude, longitude, ID, comid)

# convert to data frame and rejoin and reproject
bmi_missing_coms <- bmi_segs %>% st_drop_geometry() %>% as.data.frame()
bmi_missing_coms <- bmi_missing_coms %>% rowid_to_column() %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>% 
  st_transform(3310) %>% group_split(rowid) %>%  
  # this function takes a minute or so to run
  map(~discover_nhdplus_id(.x$geometry))

# now have a list of all the missing COMIDs 
# just need to flatten from list to df and rejoin

# flatten
bmi_segs_df <-bmi_missing_coms %>% flatten_dfc() %>% t() %>% as.data.frame() %>% 
  rename("comid"=V1) %>% 
  mutate(StationCode = bmi_segs$StationCode)
#save(bmi_segs_df, file = "data_output/03_selected_bmi_missing_comids_all.rda")

# rejoin
bmi_comids_rev <- bind_rows(bmi_segs_df, bmi_comids)

# save back out:
saveRDS(bmi_comids_rev, file="data_output/03_bmi_all_stations_comids_all_gages.rds")

# rejoin to get full comids
sel_bmi_gages <- sel_bmi_gages %>% left_join(., bmi_comids_rev, by="StationCode") %>% 
  # remove old col and rename:
  select(-comid.x) %>% rename(comid=comid.y)
summary(sel_bmi_gages)

save(sel_bmi_gages, sel_gages_bmi, file="data_output/03_selected_bmi_and_gages_all_gages.rda")

# GET UPSTREAM FLOWLINES --------------------------------------------------

## TRANSFORM TO UTM datum for flowlines
sel_bmi_sf <- st_transform(sel_bmi_gages, crs=3310) # use CA Teal albs metric
sel_gages_sf <- st_transform(sel_gages_bmi, crs=3310)

#save(sel_gages_sf, sel_bmi_sf, file = "data_output/03_selected_bmi_sites_allgages_sf_3310.rda")

# get the COMID for each gage in list
usgs_segs <- sel_gages_bmi %>% split(.$site_id) %>%
  map(~discover_nhdplus_id(.x$geometry))

# fix 326 and 327 which pull two segs
usgs_segs[[326]] <- 14971709
usgs_segs[[326]]
usgs_segs[[327]] <- 14971711
usgs_segs[[327]]

# use purrr
# use the list of comids to make a list to pass to the nhdplusTools function
coms_list <- map(usgs_segs, ~list(featureSource = "comid", featureID=.x))
coms_list[[40]] # tst check, should list feature source and featureID

# Get Mainstem Segs, needed to do in chunks
mainstemsUS_200 <- map(coms_list[1:320], ~navigate_nldi(nldi_feature = .x,
                                          mode="upstreamMain",
                                          data_source = ""))

mainstemsUS_400 <- map(coms_list[320:330], ~navigate_nldi(nldi_feature = .x,
                                                        mode="upstreamMain",
                                                        data_source = ""))
# bind together
mainstems_US <- rbind(mainstemsUS_100, mainstemsUS_200, mainstemsUS_300)

mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                           mode="downstreamMain",
                                           distance_km = 15,
                                           data_source = ""))

# IT WORKSSSSSS!!!!!
mapview(mainstems, col.regions="blue", col="blue", legend=F, lwd=2.5) +
  mapview(mainstemsDS, color="skyblue4", lwd=4, legend=F) + 
  mapview(sel_bmi_sf, col.regions="orange", legend=F) + 
  mapview(sel_gages_sf, col.regions="dodgerblue", legend=F, cex=5) 


# make a single flat layer
mainstems_flat_ds <- mainstemsDS %>%
  set_names(., sel_gages_sf$ID) %>%
  map2(sel_gages_sf$ID, ~mutate(.x, gageID=.y))

# bind together
mainstems_ds <- do.call(what = sf:::rbind.sf,
          args = mainstems_flat_ds)

# make a single flat layer
mainstems_flat_us <- mainstems %>%
  set_names(., sel_gages_sf$ID) %>%
  map2(sel_gages_sf$ID, ~mutate(.x, gageID=.y))

# bind together
mainstems_us <- do.call(what = sf:::rbind.sf,
                             args = mainstems_flat_us)

rm(mainstems_flat_ds, mainstems_flat_us)

save(mainstems_us, mainstems_ds, file = "data_output/03_selected_nhd_flowlines_mainstems.rda")

mapview(mainstems_ds) + mapview(mainstems_us, color="purple")

# RELOAD AND MAP ----------------------------------------------------------

# create a final map of selected gages and bmi + huc12 + flowlines
load("data_output/03_selected_bmi_and_gages.rda")
load("data_output/03_selected_nhd_flowlines_mainstems.rda")
load("data_output/03_selected_h12_contain_bmi_gage.rda")

mapview(mainstems_ds, color="slateblue", legend=F) +
  mapview(mainstems_us, color="darkblue", legend=F) +
  mapview(sel_gages_bmi, col.regions="purple", cex=8) + 
  mapview(sel_bmi_gages, col.regions="orange", cex=6)

