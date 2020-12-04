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
bmi_comids <- readRDS("data_output/02b_bmi_stations_comids.rds")

# z02. BMI COMIDS: GET NEW/MISSING COMIDS --------------------------

# check/update the COMID for each BMI site (run once)
# ADD COMID (comid=USGS gage, COMID=bmi)

# transform to same datum/crs
# sel_bmi_station_gages_h12 <- st_transform(sel_bmi_station_gages_h12, crs = 3310) # use CA Teale albs metric
# sel_bmi_gages_csci <- st_transform(sel_bmi_gages_csci, crs = 3310) # use CA Teale albs metric
# sel_gages_bmi <- st_transform(sel_gages_bmi, crs=3310)
#   
# # Create dataframe for looking up COMIDS (here use all stations)
# bmi_segs <- sel_bmi_station_gages_h12 %>%
#   select(StationCode, longitude, latitude) %>%
#   distinct(StationCode, .keep_all = TRUE) %>% 
#   mutate(COMID = NA)
#  
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
# # rm COMIDs starting with "V"
# bmi_comids <- bmi_segs_df %>% filter(!grepl("^V", StationCode))
#  
# # bind with existing bmi_comids:
# #bmi_coms <- readRDS("data_output/02b_bmi_all_stations_comids.rds")
# # bind
# #bmi_comids <- bind_rows(bmi_coms, bmi_comids)
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


# 06. FILTER TO BMI SITES IN USGS MAINSTEM COMIDS -----------------------------

### LEFT OFF HERE

# reload sites/data here
sel_bmi_gages_csci <- readRDS("data_output/03_selected_bmi_h12_all_gages_csci.rds")
sel_bmi_gages <- readRDS("data_output/03_selected_bmi_h12_all_gages.rds")
sel_gages_bmi <- readRDS("data_output/03_selected_usgs_h12_all_gages.rds")
sel_h12_bmi <- readRDS("data_output/03_selected_h12_all_gages.rds")
load("data_output/03_selected_nhd_mainstems_gages.rda")

# get distinct segs only 
mainstems_distinct <- mainstems_all %>% distinct(nhdplus_comid, .keep_all=TRUE)

# all BMI comids that occur in list of mainstem NHD comids: (n=723)
sel_bmi_coms_final <- sel_bmi_gages_csci %>% 
  filter(COMID %in% mainstems_distinct$nhdplus_comid)

# distinct comid/station/gages combinations:
sel_bmi_coms_final %>% st_drop_geometry() %>% 
  distinct(StationCode, site_id) %>% tally() # n=723

# distinct BMI COMIDs
sel_bmi_coms_final %>% st_drop_geometry() %>% distinct(COMID) %>% tally() # 347

# distinct GAGES COMIDS
sel_bmi_coms_final %>% st_drop_geometry() %>% distinct(site_id) %>% tally() # 319

# 09. FINAL MAP -------------------------------------------------------

# create a final map of selected gages and bmi + huc12 + flowlines

# get all BMI not selected...check why not on map
bmi_not_selected <- sel_bmi_gages_csci %>% filter(!COMID %in% mainstems_distinct$nhdplus_comid) # should be 317 (loss of 70% of data)

# get all gages selected (n=319)
gages_selected <- sel_gages_bmi %>% 
  filter(site_id %in% sel_bmi_coms_final$site_id)

# get the gages not selected (n=96)
gages_not_selected <- sel_gages_bmi %>% 
  filter(!site_id %in% sel_bmi_coms_final$site_id)

# get the hucs selected (n=198)
hucs_selected <- sel_h12_bmi %>% 
  filter(HUC_12 %in% sel_bmi_coms_final$HUC_12)

# get the hucs not selected (n=53)
hucs_not_selected <- sel_h12_bmi %>% 
  filter(!HUC_12 %in% sel_bmi_coms_final$HUC_12)


## 
mapviewOptions(fgb = FALSE)

# this map of all sites selected U/S and D/S
m2 <- mapview(sel_bmi_coms_final, cex=6, col.regions="orange", 
              layer.name="Selected BMI comids") +  
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(gages_selected, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all bmi or gages in same H12 but not selected
  mapview(gages_not_selected, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(bmi_not_selected, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other BMI Sites in H12") + 
  mapview(hucs_selected, col.regions="orange3", alpha.region=0.1, 
          color="orange", legend=F, layer.name="Selected HUC12") +
  mapview(hucs_not_selected, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="Other HUC12")

m2@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save this final map out as:"map_of_final_gages_bmi_stations_all_gages"
#mapshot(m2, url = paste0(here::here(),"/figs/03_map_of_final_bmi_stations_gages_h12s.html"))


# ADD SITES MANUALLY ------------------------------------------------------

## UPDATED 11-30-2020

gages_to_drop <- c(11084500)
gages_to_add <- c(11063000, 11063510, 11152300, 11414000, 11379000, 11477500, 11527000)
csci_to_add <- c("905SDBDN9", "801S01523","801RB8309", "801RB8483", 
                 "801RB8396", "SMCR8_327", "309SAC","113GAR084", "519CE0531",
                 "517PS0030", "509FCA054", "111CE0089", "106WE0580")
csci_to_drop <- c("801RB8593")


# do the thing:
sel_bmi_coms_final_v2 <- st_drop_geometry(sel_bmi_coms_final) %>% 
  # add n=13
  bind_rows(., filter(st_drop_geometry(sel_bmi_gages_csci), StationCode %in% csci_to_add)) %>%
  # filter out the stuff we don't want
  filter(!StationCode %in% csci_to_drop, 
         !site_id %in% gages_to_drop) 

# re-make the geom using bmi stations
sel_bmi_coms_final_v2 <- st_as_sf(sel_bmi_coms_final_v2, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

### MAP AGAIN

# not selected bmi
bmi_not_selected_v2 <- sel_bmi_gages_csci %>% filter(!StationCode %in% sel_bmi_coms_final_v2$StationCode) # n=303

# get all gages selected (n=326)
gages_selected_v2 <- sel_gages_bmi %>% 
  filter(site_id %in% sel_bmi_coms_final_v2$site_id)

# get the gages not selected (n=89)
gages_not_selected_v2 <- sel_gages_bmi %>% 
  filter(!site_id %in% sel_bmi_coms_final_v2$site_id)

# get hucs selected (n=205)
hucs_selected_v2 <- sel_h12_bmi %>% 
  filter(HUC_12 %in% sel_bmi_coms_final_v2$HUC_12)

# this map of all sites selected U/S and D/S
m3 <- mapview(sel_bmi_coms_final_v2, cex=6, col.regions="orange", 
              layer.name="Selected BMI comids") +  
  mapview(mainstems_all, zcol="from_gage", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(gages_selected_v2, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all bmi or gages in same H12 but not selected
  mapview(gages_not_selected_v2, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(bmi_not_selected, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other BMI Sites in H12") + 
  mapview(hucs_selected_v2, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# SAVE OUT ----------------------------------------------------------------

# load the full dataset from 00
load("data_output/00_bmi_cleaned_all.rda") # all data

# pull BMI sites and get list of data, first join with orig full dataset:
bmi_coms_dat <- left_join(sel_bmi_coms_final_v2, st_drop_geometry(bmi_clean) %>% select(StationCode, SampleID, MM:problemFinalID), by=c("StationCode", "SampleID")) 

# now look at how many unique CSCI samples are avail: n=437 unique samples
bmi_coms_dat %>% st_drop_geometry() %>% distinct(SampleID) %>% tally

# now look at how many unique stations: n=270 stations
bmi_coms_dat %>% st_drop_geometry() %>% distinct(StationCode) %>% tally

# now look at how many unique gageID: n=326
bmi_coms_dat %>% st_drop_geometry() %>% distinct(site_id, .keep_all=TRUE) %>% tally()

# see how many are ref vs alt
ref_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_ref_gages_list.csv")
alt_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_alt_gages_list.csv")

# add CEFF alt type
bmi_coms_dat <- bmi_coms_dat %>% 
  mutate(CEFF_type = case_when(
    bmi_coms_dat$site_id %in% ref_gages$site_id ~ "REF",
    bmi_coms_dat$site_id %in% alt_gages$site_id ~ "ALT"
  ))

# look at gages by type
bmi_coms_dat %>% 
  st_drop_geometry() %>% distinct(site_id, .keep_all=TRUE) %>% group_by(CEFF_type) %>% tally()

#ALT         245
#REF          81

# summary
summary(bmi_coms_dat)
hist(bmi_coms_dat$MM) # what months?

# if trim to summer months how many records do we lose? (14% of data)
bmi_coms_dat_trim <- bmi_coms_dat %>% filter(MM>4 & MM<10) 
hist(bmi_coms_dat_trim$MM)

# if trimming we lose a few gages: ALT=100, REF=42
bmi_coms_dat_trim %>% st_drop_geometry() %>% distinct(site_id, .keep_all=TRUE) %>% count(CEFF_type)

#ALT 221
#REF 78

# save out
save(bmi_coms_dat, bmi_coms_dat_trim, sel_bmi_coms_final_v2, file = "data_output/03_selected_final_bmi_stations_dat_all_gages.rda")

