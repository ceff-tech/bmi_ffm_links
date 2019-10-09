### Make a list of ref/station/samples for BMI
## R. Peek
## Creates dataset of BMI stations across CA that have spatial metadata and Site Status (stressed/interm/ref)
## DATA OUT:
### - bmi_stations_metadat (all distinct bmi station and metadata, includes site status, n=1985)
### "01_bmi_stations_ref_nonref_metadata.rda"
### - bmi_clean_stations_ss (bmi stations with site status that exist in bmi_clean dataset, n=1349)
### "01_bmi_cleaned_stations_w_site_status.rda"

# Libraries ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(lubridate)
library(sf)
library(mapview)

# Look at data: 
# these were sent by Raffi M. on May 29, 2019

# Reference Sites/Samples -------------------------------------------------

# all ref sites
ref_sites_xl <- read_excel("data/Reference_sites.xlsx") %>% 
  dplyr::rename(lat=New_Lat, lon=New_Long)# 781 obs with 6 vars

ref_samples <- read_csv("data/samples.ref.csv") %>% 
  data.frame() %>% 
  mutate(SampleDate=mdy_hms(SampleDate))

ref_stations <- read_csv("data/stations.ref.csv") %>% # 586, matches above
  dplyr::rename(lat=New_Lat, lon=New_Long) %>% 
  select(StationCode, lat, lon, County, PSARegion, Eco_III_2010)

# how many in both?
ref_sites_xl %>% filter(StationCode %in% ref_stations$StationCode) %>% tally

# double check (this should = nrow(ref_sites_xl) - nrow(ref_stations))
anti_join(ref_sites_xl, ref_stations, by="StationCode") %>% tally

# join two reference sets w lat longs and map:
ref_sites <- left_join(ref_sites_xl, ref_stations) %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=F)

# no lat/lons
ref_xeric_data <- read_csv("data/ReportingMetrics_USGS Xeric Flows study.csv") %>%
  select(StationCode:SampleDate, CollectionMethodCode, Replicate:Result) %>% 
  mutate(SampleDate=mdy(SampleDate))

# only 24 unique stations?!?
ref_xeric_sites <- ref_xeric_data %>% select(StationCode, StationName) %>% 
  distinct(StationCode, .keep_all = T) %>% data.frame()

# are these in reference set above?
ref_xeric_sites %>% filter(StationCode %in% ref_sites$StationCode)
# 7 sites, but need lat/lon for other 17...

# Non-Reference Sites -----------------------------------------------------

# non-ref sites
nref_samples <- read_csv("data/samples.nonref.csv") %>% # 1677 nonref unique StationCodes
  data.frame() %>% 
  mutate(SampleDate=mdy_hms(SampleDate))

# get metadata  
nref_stations <- read.csv("data/stations.nonref.csv", stringsAsFactors = F) %>% 
  dplyr::rename(lat=New_Lat, lon=New_Long) %>% 
  select(StationCode, SiteStatus, lat, lon, County, PSARegion, Eco_III_2010) %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=F)

# ?? 1985 sites...mostly overlap both ref/non-ref
stations_out <- read_excel("data/stations.out.xlsx") %>% 
  dplyr::rename(lat=New_Lat, lon=New_Long) %>% 
  select(StationCode, SiteStatus, lat, lon, County, PSARegion, Eco_III_2010, everything()) %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=F)

# Map ---------------------------------------------------------------------


mapview(ref_sites, col.regions="orange", layer.name="Ref sites") +
  mapview(nref_stations, col.regions="black", layer.name="Non-ref sites") +
  mapview(stations_out, zcol="SiteStatus", layer.name="stations_out?")


mapview(ref_sites, col.regions="orange", layer.name="Ref sites") +
  mapview(nref_stations, col.regions="black", layer.name="Non-ref sites") +
  mapview(stations_out %>% filter(SiteStatus=="Reference"), zcol="SiteStatus", layer.name="stations_out?")

# Comparing Data ----------------------------------------------------------

# see if there's any matches?
nref_sites_df <- nref_stations %>% st_drop_geometry() %>% as.data.frame()
ref_sites_df <- ref_sites %>% st_drop_geometry() %>% data.frame() %>% select(-BugCode, -SampleID)
sta_out_df <- stations_out %>% st_drop_geometry() %>% data.frame() 

# only 4 matches across both
inner_join(ref_sites_df, nref_sites_df, by="StationCode") %>% tally
inner_join(ref_sites_df, nref_sites_df, by="StationCode") %>% distinct(StationCode)
# 4 similar sites:
# 105WLCABC
# 201WLK190
# 202BUT030
# R5BIO-016

# see what's totally diff between the two?
anti_join(ref_sites_df, nref_sites_df, by=c("StationCode")) %>% tally # 777 unique ref_sites not in nref_sites
anti_join(nref_sites_df, ref_sites_df, by=c("StationCode")) %>% tally # 1395 unique nonref_sites not in ref_sites, so 4 sites overlap

# samples shared only in stations_out?
inner_join(ref_sites_df, sta_out_df, by=c("StationCode")) %>% distinct(StationCode) %>% tally # 590 of 781 ref sites in stations_out

inner_join(nref_sites_df, sta_out_df, by=c("StationCode")) %>% distinct(StationCode) %>% tally # all nonref sites in stations_out (n=1399)

# Check all Data and Join ----------------------------------------------

# join ref/non-ref and look for distinct sites only:
sites_df <- bind_rows(ref_sites_df, nref_sites_df, sta_out_df) %>% 
  distinct(StationCode, .keep_all = T)

sites_df %>% group_by(SiteStatus) %>% tally()
# 781 reference
# 837 intermediate (nonref)
# 558 stressed (nonref)

ref_sites_df %>% group_by(SiteStatus) %>% tally()
nref_sites_df %>% group_by(SiteStatus) %>% tally() # so dropped 4 from this set

# check back:
anti_join(ref_sites_df, sites_df, by=c("StationCode")) %>% tally # all included
anti_join(nref_sites_df, sites_df, by=c("StationCode")) %>% tally # all included

# full metadata set is same as sites_df == stations_out
bmi_stations_metadat <- stations_out

# save out
save(bmi_stations_metadat, file = "data_output/01_bmi_stations_ref_nonref_metadata.rda")

# Check with Other data (see script 00) ----------------------------------

load("data_output/01_bmi_stations_ref_nonref_metadata.rda")
load("data_output/00_bmi_cleaned_all.rda") # all data from Script 01

# create only spatially distinct stations:
bmi_clean_distinct <- bmi_clean %>% 
  distinct(StationCode, latitude, longitude) %>% 
  rename(lon=longitude, lat=latitude)

# join with full data from other set (sites_df)
length(unique(bmi_clean_distinct$StationCode)) # n=2935
length(unique(bmi_stations_metadat$StationCode)) # n= 1985

# some duplicated stations w/ diff lats or longs, or very similar
bmi_clean_stations_ss <- bmi_stations_metadat %>% st_drop_geometry() %>% 
  select(StationCode, SiteStatus, lat, lon) %>% 
  inner_join(., bmi_clean_distinct %>% select(-lat, -lon), by="StationCode") # n=1349 that have data

bmi_clean_stations_ss %>% distinct(StationCode) %>% tally()

# save out final dataset that is ONLY the StationCode, lat, lon, and SiteStatus for the BMI data that is currently available/in the "bmi_clean" dataset.

save(bmi_clean_stations_ss, file = "data_output/01_bmi_cleaned_stations_w_site_status.rda")
