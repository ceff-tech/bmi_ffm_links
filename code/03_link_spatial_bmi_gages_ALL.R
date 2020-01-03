# 02 Spatially Linking BMI with ALL GAGES
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
load("data/usgs_ca_all_daily_flow_gages.rda") # all daily flow gages
bmi_comids <- readRDS("data_output/02_bmi_all_stations_comids.rds")
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

# stations with site status
bmi_clean_stations_ss <- bmi_clean_stations_ss %>%
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=F) %>%
  st_transform(4269)

# check projs are same
st_crs(bmi_clean)
st_crs(bmi_clean_stations)
st_crs(bmi_clean_stations_ss)
st_crs(gages)

# FILTER 01. Gages in Same Time As BMI -----------------------------------

# so 1192 gages meet temporal scale, years must be post 1994
gages_all_filt <- gages %>% filter(end_yr > 1994)

# plot
#mapview(bmi_clean_stations, col.regions="orange", cex=4) + 
  #mapview(bmi_clean_stations_ss, zcol="SiteStatus") + 
  #mapview(gages_all_filt, col.regions="skyblue4")

# FILTER 02: Intersect BMI/Gages by H12 -----------------------------------

# Add H12 to points to BMI and Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE), using BMI_CLEAN ALL DISTINCT STATIONS
bmi_h12 <- st_join(bmi_clean_stations, left = TRUE, h12[c("HUC_12","h12_area_sqkm")])

# Add H12 to all gages
gages_h12 <- st_join(gages_all_filt, left=TRUE, h12[c("HUC_12")]) %>% 
  select(site_id, HUC_12, lon, lat, elev_m, date_begin, date_end, end_yr) %>% st_drop_geometry()

# now join based on H12: how many bmi stations vs HUC12s? n=2188 stations
sel_bmi_gages <- inner_join(bmi_h12, gages_h12, by="HUC_12") %>% 
  rename(ID=site_id) %>% 
  #distinct(StationCode, .keep_all = T)
  distinct(StationCode, ID, .keep_all = T) # gets 2188 sites!

# view
#mapview(sel_bmi_gages)

# number of unique h12s?
length(unique(factor(sel_bmi_gages$HUC_12))) # unique h12=320
length(unique(sel_bmi_gages$ID)) # unique gages=601
length(unique(sel_bmi_gages$StationCode)) # unique BMI Stations=1107

# Get Selected Gages ONLY: 
sel_gages_bmi <- gages_all_filt %>% filter(site_id %in% sel_bmi_gages$ID)

# select H12s that have points inside:
sel_h12_bmi <- h12[sel_bmi_gages, ]

# save out
write_rds(sel_h12_bmi, path="data_output/03_selected_h12_all_gages.rds")
write_rds(sel_gages_bmi, path="data_output/03_selected_usgs_h12_all_gages.rds")
write_rds(sel_bmi_gages, path="data_output/03_selected_bmi_h12_all_gages.rds")

# * Map of Filtered Gages ------------------------------------------------------

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

# GET BMI COMIDs ----------------------------------------------------------

# load previous stuff run from 02 script (section GET BMI COMIDS)
bmi_comids <- read_rds("data_output/02_bmi_all_stations_comids.rds")

# join with NHD comids from existing BMI comid list
sel_bmi_gages <- sel_bmi_gages %>% left_join(., bmi_comids, by="StationCode")
# should have 2188

summary(sel_bmi_gages$comid) # no missing

save(sel_bmi_gages, sel_gages_bmi, file="data_output/03_selected_bmi_and_gages_same_h12_all_gages.rda")

# GET GAGE COMIDS --------------------------------------------------

## TRANSFORM TO UTM datum for flowlines
sel_bmi_sf <- st_transform(sel_bmi_gages, crs=3310) # use CA Teal albs metric
sel_gages_sf <- st_transform(sel_gages_bmi, crs=3310)

# get the COMID for each gage in list
usgs_segs <- sel_gages_bmi %>% split(.$site_id) %>%
  map(~discover_nhdplus_id(.x$geometry))

# now have a list of all the missing COMIDs, check for dups
usgs_segs %>% 
  purrr::map_lgl(~ length(.x)>1) %>% 
  #table() # 3 are FALSE
  .[.==TRUE] # get values that are TRUE

# view comids
usgs_segs["11186000"]

# fix 326 and 327 which pull two segs
usgs_segs["11186000"] <- 14971709
usgs_segs["11186001"] <- 14971711
usgs_segs["11404240"] <- 2775510

# double check again:
usgs_segs %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table() # all FALSE

# save the USGS station COMIDs file:
write_rds(usgs_segs, path="data_output/03_usgs_sel_gages_comids.rds")

# use the list of comids to make a list to pass to the nhdplusTools function
coms_list <- map(usgs_segs, ~list(featureSource = "comid", featureID=.x))
coms_list[[326]] # tst check, should list feature source and featureID

# GET UPSTREAM FLOWLINES --------------------------------------------------

# Get Mainstem Segs, needed to do in chunks if needed and rbind
mainstemsUS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                          mode="upstreamMain",
                                          data_source = ""))

# make a single flat layer
mainstems_flat_us <- mainstemsUS %>%
  set_names(., sel_gages_sf$site_id) %>%
  map2(sel_gages_sf$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_us <- do.call(what = sf:::rbind.sf,
                        args = mainstems_flat_us)

rownames(mainstems_us) <- c()

# add direction to gage col
mainstems_us <- mainstems_us %>% 
  mutate(to_gage = "US")

rm(mainstems_flat_us, mainstemsUS)

# GET DOWNSTREAM FLOWLINES ------------------------------------------------

# get NHD segments downstream of selected USGS gages, 15 km buffer
mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                           mode="downstreamMain",
                                           distance_km = 15,
                                           data_source = ""))

# make a single flat layer
mainstems_flat_ds <- mainstemsDS %>%
  set_names(., sel_gages_sf$site_id) %>%
  map2(sel_gages_sf$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_ds <- do.call(what = sf:::rbind.sf,
          args = mainstems_flat_ds)
# remove weird rownames
rownames(mainstems_ds) <- c()

# add direction to gage col
mainstems_ds <- mainstems_ds %>% 
  mutate(to_gage = "DS")

rm(mainstems_flat_ds, mainstemsDS)

# bind all mainstems
mainstems_all <- rbind(mainstems_us, mainstems_ds)

# * SAVE OUT STREAMLINES FOR GAGES ------------------------------------------

save(mainstems_us, mainstems_ds, file = "data_output/03_selected_nhd_flowlines_mainstems_all_gages.rda")

# * PREVIEW MAP ----------------------------------------------------------

load("data_output/03_selected_bmi_and_gages_same_h12_all_gages.rda")

# make a map
# mapview(mainstems_ds, color="slateblue", legend=F) +
#   mapview(mainstems_us, color="darkblue", legend=F) +
#   mapview(sel_gages_bmi, col.regions="purple", cex=8) + 
#   mapview(sel_bmi_gages, col.regions="orange", cex=6) +
#   mapview(sel_h12_bmi, layer="H12", color="dodgerblue", col.regions="cyan", alpha=0.8, lwd=1, legend=FALSE)

# FILTER TO BMI SITES IN USGS MAINSTEM COMIDS -----------------------------

load("data_output/03_selected_nhd_flowlines_mainstems_all_gages.rda")

# bind all mainstems
mainstems_all <- rbind(mainstems_us, mainstems_ds)

# all stations us of gage:
bmi_coms_us <- sel_bmi_gages %>% 
  dplyr::filter(comid %in% as.integer(mainstems_us$nhdplus_comid)) %>% 
  mutate(to_gage = "US") # gets 1275 stations

# all stations 15km downstream on mainstem
bmi_coms_ds <- sel_bmi_gages %>% 
  dplyr::filter(comid %in% as.integer(mainstems_ds$nhdplus_comid)) %>% 
  mutate(to_gage="DS") # gets 1260 stations

# combine US and DS
bmi_coms_final <- rbind(bmi_coms_ds, bmi_coms_us)

# distinct stations (could include replicates)
bmi_coms_final %>% st_drop_geometry() %>% 
  distinct(StationCode, ID) %>% tally() # 1627

# distinct COMIDs
bmi_coms_final %>% st_drop_geometry() %>% distinct(comid) %>% tally() # 566

# * FINAL MAP -------------------------------------------------------

# create a final map of selected gages and bmi + huc12 + flowlines

# get all BMI not selected...check why not on map
bmi_not_selected <- sel_bmi_gages %>% filter(!as.character(comid) %in% mainstems_all$nhdplus_comid) # should be 561 = (2188 total -  1627 selected)

# this map of all sites selected U/S and D/S
m3 <- mapview(bmi_coms_ds, cex=6, col.regions="orange", layer.name="Selected BMI D/S") +  
  mapview(mainstems_all, zcol="to_gage", cex=3, layer.name="NHD Flowlines")+
  mapview(bmi_coms_us, cex=6, col.regions="yellow", layer.name="Selected BMI U/S") +
  mapview(sel_gages_bmi, col.regions="cyan", cex=7, layer.name="Selected USGS Gages") + 
  # these are all bmi in same H12 but not selected
  mapview(bmi_not_selected, col.regions="gray", cex=3.2, alpha=0.5, layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save this final map out as:"map_of_final_gages_bmi_stations_all_gages"

# SAVE OUT ----------------------------------------------------------------

# pull BMI sites and get list of data, first join with orig full dataset:
bmi_coms_dat <- left_join(bmi_coms_final, st_drop_geometry(bmi_clean), by="StationCode") %>% select(-latitude.y, -longitude.y, -lon, -lat) %>% 
  rename(lat = latitude.x, lon = longitude.x)

# now look at how many unique samples are avail: n=1507 unique samples
bmi_coms_dat %>% as.data.frame() %>% group_by(SampleID) %>% distinct(SampleID) %>% tally

# now look at how many unique stations: n=792 stations
bmi_coms_dat %>% as.data.frame() %>% group_by(StationCode) %>% distinct(StationCode) %>% tally

# save out
save(bmi_coms_dat, bmi_coms_final, file = "data_output/03_final_bmi_stations_dat_all_gages.rda")
