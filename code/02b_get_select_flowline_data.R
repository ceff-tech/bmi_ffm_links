# 02b Spatially Linking BMI & selected USGS Gages by NHD Flowlines
## R. Peek 2020

## Spatially link the BMI station data with the USGS FFC gages that occur in same flowline and h12

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
library(glue)
library(here)
library(lubridate)
library(beepr) # to tell us when stuff is done


#devtools::install_github("USGS-R/nhdplusTools")
library(nhdplusTools)

# 01. Load Data ---------------------------------------------------------------

# selected HUC12s
sel_h12_bmi <- read_rds("data_output/02a_sel_h12_w_bmi_csci.rds")
sel_h12_gages <- read_rds("data_output/02a_sel_h12_w_ffc_gages.rds")

# selected bmi and gages
sel_gages_bmi <- read_rds("data_output/02a_sel_ffc_gages_by_h12.rds")
sel_bmi_gages_csci <- read_rds("data_output/02a_sel_bmi_stations_csci_by_h12.rds")
sel_bmi_station_gages_h12 <- read_rds("data_output/02a_sel_bmi_stations_h12.rds")

# BMI COMIDs (from Section 02)
bmi_comids <- readRDS("data_output/02b_bmi_stations_comids_revised.rds")

# 02. BMI COMIDS: See Existing COMIDs --------------------------

# # read in comids from raf:
# bmi_comids_raw <- rio::import("data/bmi/bmi_comids_for_ryan.xlsx") %>% 
#   rename(COMID_bmi=comid)
# 
# # check/update the COMID for each BMI site (run once)
# # ADD COMID (comid=USGS gage, COMID=bmi)
# 
# # transform to same datum/crs
# sel_bmi_station_gages_h12 <- st_transform(sel_bmi_station_gages_h12, crs = 3310) # use CA Teale albs metric
# sel_bmi_gages_csci <- st_transform(sel_bmi_gages_csci, crs = 3310) # use CA Teale albs metric
# sel_gages_bmi <- st_transform(sel_gages_bmi, crs=3310)
# 
# # Create dataframe for looking up COMIDS (here use all stations)
# bmi_segs <- sel_bmi_station_gages_h12 %>%
#   select(StationCode, longitude, latitude) %>%
#   distinct(StationCode, .keep_all = TRUE)
# 
# # compare with raw COMIDS to see how many match:
# bmi_segs <- left_join(bmi_segs, bmi_comids_raw, by="StationCode" )
# 
# # add in COMID from bmi_comid (nhdtools)
# bmi_segs <- left_join(bmi_segs, bmi_comids, by="StationCode")
# 
# # finally read merge into one dataset (keep BMI comid except where NA, fill with NHD version)
# bmi_segs <- bmi_segs %>% 
#   mutate(COMID = case_when(
#     is.na(COMID_bmi) ~ COMID_nhd,
#     TRUE ~ as.integer(COMID_bmi)
#   ))
# 
# # Save out: 
# write_rds(bmi_segs, file="data_output/02b_bmi_stations_comids_revised.rds")

## 02b. BMI COMIDs from NHDTools ------------------------------------------------

# # use nhdtools to get comids
# bmi_all_coms <- bmi_segs %>%
#   group_split(StationCode) %>%
#   set_names(bmi_segs$StationCode) %>%
#   map(~discover_nhdplus_id(.x$geometry))
#    
# # flatten into single dataframe instead of list
# bmi_segs_df <-bmi_all_coms %>% flatten_dfc() %>% t() %>%
#   as.data.frame() %>%
#   rename("COMID"=V1) %>% rownames_to_column(var = "StationCode")
# 
# # rm COMIDs starting with "V" (this is remnant of old version)
# #bmi_comids <- bmi_segs_df %>% filter(!grepl("^V", StationCode))
# 
# # write back out
# write_rds(bmi_comids, file="data_output/02b_bmi_stations_comids.rds")
# 
# # clean up
# rm(bmi_all_coms, bmi_segs_df, bmi_segs, bmi_coms)

# 03. GET UPSTREAM FLOWLINES FROM GAGE --------------------------------------------------

# use list of gage NHD comids to make a list to pass to nhdplusTools to get flowlines
# pull data from 10 km upstream

## transform datum for flowlines
sel_bmi_gages_csci <- st_transform(sel_bmi_gages_csci, crs=3310) # use CA Teale albs metric
sel_gages_bmi <- st_transform(sel_gages_bmi, crs=3310)

# check for missing comids?
summary(sel_bmi_gages_csci$comid) # gage sites

# Use the GAGE com_list
coms_list <- map(sel_gages_bmi$comid, ~list(featureSource = "comid", featureID=.x))

# check
coms_list[[200]] # should list feature source and featureID

# Get upstream mainstem streamlines (10 km limit) from gages
mainstemsUS <- map(coms_list, ~navigate_nldi(nldi_feature = .x, 
                                             mode="UM", # upstream main 
                                             distance_km = 10))
beep(2)

# check length (for NAs?) (n=415 if no missing)
mainstemsUS %>% 
     purrr::map_lgl(~ length(.x)>1) %>% table()

# transform the sf layer to match mainstems crs (4326)
sel_gages_bmi <- sel_gages_bmi %>% st_transform(4326)

# make a single flat layer
mainstems_flat_us <- mainstemsUS %>%
  set_names(., sel_gages_bmi$site_id) %>%
  map2(sel_gages_bmi$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_us <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_us, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_us <- mainstems_us %>% 
  mutate(from_gage = "UM")

# rm temp files
rm(mainstems_flat_us, mainstemsUS)

## Map and Save ---------------------------------

# preview
mapview(mainstems_us) + 
  mapview(sel_bmi_gages_csci, cex=6, col.regions="orange", 
          layer.name="Selected BMI Stations") +  
  mapview(sel_gages_bmi, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages")

# save 
write_rds(mainstems_us, file = "data_output/02b_sel_gage_mainstems_us.rda")

# 04. GET DOWNSTREAM MAIN FLOWLINES FROM GAGE ------------------------------------------------

# get NHD segments downstream of selected USGS gages, 10 km buffer
mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                           mode="downstreamMain",
                                           distance_km = 10))
beep(2)

# check length (for NAs?)
mainstemsDS %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

# make a single flat layer
mainstems_flat_ds <- mainstemsDS %>%
  set_names(., sel_gages_bmi$site_id) %>%
  map2(sel_gages_bmi$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_ds <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_ds, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_ds <- mainstems_ds %>% 
  mutate(from_gage = "DM")

rm(mainstems_flat_ds, mainstemsDS)

## Map and Save ------------------------

mapview(mainstems_ds, color="yellow3") +
  mapview(mainstems_us, color="darkgreen") +
  mapview(sel_bmi_gages_csci, cex=6, col.regions="orange", 
          layer.name="Selected BMI Stations") +  
  mapview(sel_gages_bmi, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages")

# save 
write_rds(mainstems_ds, file = "data_output/02b_sel_gage_mainstems_ds.rds")


# 05. GET DOWNSTREAM DIVERSION MAIN FLOWLINES FROM GAGE ------------------------------------------------

# get diversions
mainstemsDD <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="downstreamDiversions",
                                             distance_km = 10))
beep(2)

# check length (for NAs?)
mainstemsDD %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

# make a single flat layer
mainstems_flat_dd <- mainstemsDD %>%
  set_names(., sel_gages_bmi$site_id) %>%
  map2(sel_gages_bmi$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_dd <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_dd, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_dd <- mainstems_dd %>% 
  mutate(from_gage = "DD")

rm(mainstemsDD, mainstems_flat_dd)

## Map and Save ------------------------

# mapview
mapview(mainstems_us, color="yellow") + mapview(mainstems_ds, color="blue") +
  mapview(mainstems_dd, color="purple") +
  mapview(sel_bmi_gages_csci, cex=6, col.regions="orange", 
          layer.name="Selected BMI Stations") +  
  mapview(sel_gages_bmi, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages")

write_rds(mainstems_dd, file = "data_output/02b_sel_gage_mainstems_dd.rds")

# bind all mainstems
mainstems_all <- rbind(mainstems_us, mainstems_ds, mainstems_dd)
save(mainstems_all, file="data_output/02b_sel_gage_mainstems_all.rda")


