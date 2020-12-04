# 03a Spatially Linking BMI & selected USGS Gages within HUC12s
## R. Peek 2020

## Spatially link the BMI station data with the USGS gage list if they occur in same H12

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
library(lubridate)

# 01. Load Data ---------------------------------------------------------------

# ALL BMI SAMPLES W CSCI SCORES (bmi_samples_distinct_csci)
load("data_output/01_bmi_samples_distinct_csci.rda") # n=2925

# ALL BMI DISTINCT STATIONS (bmi_stations_distinct)
load("data_output/01_bmi_stations_distinct.rda") # n=2935

# ALL GAGES W FFC DATA
# read from ffm_comparison repo: https://github.com/ryanpeek/ffm_comparison

gages_ffc <- read_rds(file = url("https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_alteration.rds")) %>% 
  distinct(gageid, .keep_all=TRUE) # n=959

# get all gages and merge for sf
gages_sf <- read_rds(file = url("https://github.com/ryanpeek/ffm_comparison/raw/main/data/usgs_ca_all_dv_gages.rds"))

# get ref and alt gage lists
ref_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_ref_gages_list.csv")
alt_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_alt_gages_list.csv")

# add CEFF type (ALT or REF)
gages_ffc <- gages_ffc %>% 
  mutate(CEFF_type = case_when(
    gages_ffc$gageid %in% ref_gages$site_id ~ "REF",
    gages_ffc$gageid %in% alt_gages$site_id ~ "ALT"
  ))

# now merge to make spatial
ffc_gages <- inner_join(gages_sf, gages_ffc, by=c("site_id"="gageid")) %>% 
  select(-c(metric:median_in_iqr)) %>% 
  st_transform(4326) %>% 
  # drop San Pablo Bay Gage: (11182030)
  filter(!site_id=="11182030")

# drop intermediate files
rm(gages_sf, ref_gages, alt_gages, gages_ffc)

# HUC12s
load("data/spatial/huc12_sf.rda") # CA h12s
# check size:
pryr::object_size(h12)

# 02. Make BMI Data Spatial -------------------------------------------------------

# make spatial
bmi_samples_distinct_csci <- bmi_samples_distinct_csci %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F)

bmi_stations_distinct <- bmi_stations_distinct %>% 
  st_transform(4326)

# check projections are same
st_crs(bmi_stations_distinct) == st_crs(ffc_gages)
st_crs(ffc_gages) == st_crs(h12)

# 03. JOIN BMI & Gages by H12 -----------------------------------

# Spatial Join: Add H12 to BMI and Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE), using BMI DISTINCT STATIONS
bmi_station_h12 <- st_join(bmi_stations_distinct, 
                           left = TRUE, h12[c("HUC_12")])

# by sample
bmi_sample_h12 <- st_join(bmi_samples_distinct_csci, 
                          left = TRUE, h12[c("HUC_12")])

# Add H12 to gages
gages_h12 <- st_join(ffc_gages, left=TRUE, h12[c("HUC_12")]) %>%
  st_drop_geometry()

# now join based on H12: what BMI stations share same H12 as USGS gage?
sel_bmi_station_gages_h12 <- inner_join(bmi_station_h12, gages_h12, by="HUC_12") %>% 
  distinct(StationCode, site_id, .keep_all = T) # n=1915

# now join based on H12: what BMI stations share same H12 as USGS gage?
#sel_bmi_sample_gages_h12 <- inner_join(bmi_sample_h12, gages_h12, by="HUC_12") %>% 
#  distinct(SampleID, site_id, .keep_all = T) # n=1794


## A. Summarize Selected Sites & HUCS -----------------------------

# number of unique HUC12
length(unique(factor(sel_bmi_station_gages_h12$HUC_12))) # h12=312

# number of unique gages
length(unique(sel_bmi_station_gages_h12$site_id)) # gages=509

# number of unique bmi stations
length(unique(sel_bmi_station_gages_h12$StationCode)) # BMI Stations=1117

# make sure these have CSCI scores: of those in same H12, how many have CSCI scores? N=1040
sel_bmi_gages_csci <- left_join(sel_bmi_station_gages_h12, st_drop_geometry(bmi_samples_distinct_csci)[,c(1:2,5,12:14)], by="StationCode") %>% 
  filter(!is.na(csci)) %>%  # n=1794
  # unique BMI STATIONS
  #distinct(StationCode, site_id, .keep_all=TRUE) # unique: n=1040
  # unique BMI SAMPLES
  distinct(SampleID, site_id, .keep_all=TRUE) # unique: n=1794

# number of unique?
length(unique(factor(sel_bmi_gages_csci$HUC_12))) # h12=251
length(unique(sel_bmi_gages_csci$site_id)) # gages=415
length(unique(sel_bmi_gages_csci$StationCode)) # BMI Stations=635

# Get Selected Gages ONLY:  # n=415 (that have CSCI scores)
sel_gages_bmi <- ffc_gages %>% 
  filter(site_id %in% sel_bmi_gages_csci$site_id) %>% 
  distinct(site_id, .keep_all = T)

# select H12s that have points inside:
sel_h12_bmi <- h12[sel_bmi_gages_csci, ] # 251
sel_h12_gages <- h12[ffc_gages, ] # 597

## B. Map of Filtered BMI & Gages  ------------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList)

# a map of all gages and BMI stations that fall within the same H12

# get the GAGES not selected (n=543)
gages_not_selected <- ffc_gages %>% 
  filter(!site_id %in% sel_bmi_gages_csci$site_id)

# get BMI stations NOT selected with CSCI (n=1090)
bmi_not_selected <- bmi_samples_distinct_csci %>% 
  filter(!StationCode %in% sel_bmi_gages_csci$StationCode) %>% 
  distinct(StationCode, .keep_all=TRUE)

# this map of all sites selected U/S and D/S
m1 <- 
  # selected stations
  mapview(sel_bmi_gages_csci, cex=6, col.regions="orange", 
              layer.name="Selected BMI Stations") +  
  # selected gages
  mapview(sel_gages_bmi, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # NOT Selected gages
  mapview(gages_not_selected, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  # NOT selected bmi
  mapview(bmi_not_selected, col.regions="gold2", color="gray20", cex=3.2, 
          layer.name="Other BMI Sites w CSCI Scores") + 
  # All BMI Stations
  mapview(bmi_stations_distinct, col.regions="gray", color="gray20", cex=3, 
          layer.name="All BMI Sites") + 
  # selected HUC12 bmi
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=FALSE, layer.name="HUC12") + 
  # selected HUC12 gages
  mapview(sel_h12_gages, col.regions="gray50", alpha.region=0.1, 
          color="darkblue", legend=FALSE, layer.name="HUC12 Gages")

# add measurement
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")



# 04. Save Out -----------------------------------------------------------------

# save out
write_rds(sel_h12_bmi, file="data_output/02a_h12_w_bmi_csci.rds")
write_rds(sel_h12_gages, file="data_output/02a_h12_w_ffc_gages.rds")
write_rds(sel_gages_bmi, file="data_output/02a_selected_ffc_gages_by_h12.rds")
write_rds(sel_bmi_gages_csci, file="data_output/02a_selected_bmi_station_csci_by_h12.rds")
write_rds(sel_bmi_station_gages_h12, file="data_output/02a_selected_bmi_station_h12.rds")

# z05. BMI COMIDS: GET NEW/MISSING COMIDS --------------------------

# no NA's
summary(sel_bmi_gages_csci$COMID)

# IF NEEDED

library(nhdplusTools)
#  
# ## TRANSFORM TO SAME DATUM
sel_bmi_station_gages_h12 <- st_transform(sel_bmi_station_gages_h12, crs = 3310) # use CA Teale albs metric
sel_gages_bmi <- st_transform(sel_gages_bmi, crs=3310)
  
# Create dataframe for looking up COMIDS (here use all stations)
bmi_segs <- sel_bmi_station_gages_h12 %>%
  select(StationCode, longitude, latitude, COMID) %>%
  filter(is.na(COMID))
 
# use nhdtools to get comids
bmi_all_coms <- bmi_segs %>%
  group_split(StationCode) %>%
  set_names(., bmi_segs$StationCode) %>%
  map(~discover_nhdplus_id(.x$geometry))
  
# flatten into single dataframe instead of list
bmi_segs_df <-bmi_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "StationCode")
  
# rm COMIDs starting with "V"
bmi_comids <- bmi_segs_df %>% filter(!grepl("^V", StationCode))
 
# bind with existing bmi_comids:
bmi_coms <- readRDS("data_output/03_bmi_all_stations_comids.rds")

# bind
bmi_comids <- bind_rows(bmi_coms, bmi_comids)

# write back out
write_rds(bmi_comids, file="data_output/03_bmi_all_stations_comids.rds")
 
# clean up
rm(bmi_all_coms, bmi_segs_df, bmi_segs, bmi_coms)

