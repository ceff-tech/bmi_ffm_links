### Make a list of ref/station/samples for BMI
## R. Peek
## Creates dataset of BMI stations across CA that have spatial metadata and Site Status (stressed/interm/ref)

# 00 Libraries ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(lubridate)
library(sf)
library(mapview)
library(tidylog)

# Data were sent by Raffi M. on May 29, 2019
# 01 Import Reference Sites/Samples -------------------------------------------------

# all ref sites
ref_sites_xl <- read_excel("data/bmi/bmi_reference_sites.xlsx") %>% 
  dplyr::rename(lat=New_Lat, lon=New_Long)# 781 obs with 6 vars

ref_samples <- read_csv("data/bmi/bmi_samples.ref.csv") %>% 
  data.frame() %>% 
  mutate(SampleDate=mdy_hms(SampleDate))

ref_stations <- read_csv("data/bmi/bmi_stations.ref.csv") %>% # 586, matches above
  dplyr::rename(lat=New_Lat, lon=New_Long) %>% 
  select(StationCode, lat, lon, County, PSARegion, Eco_III_2010)

# how many in both? n=586
ref_sites_xl %>% filter(StationCode %in% ref_stations$StationCode) %>% tally

# double check (this should = nrow(ref_sites_xl) - nrow(ref_stations))
anti_join(ref_sites_xl, ref_stations, by="StationCode") %>% tally #n=195

# join two reference sets w lat longs and map:
ref_sites <- left_join(ref_sites_xl, ref_stations) %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=F)

# no lat/lons
ref_xeric_data <- read_csv("data/bmi/bmi_reporting_metrics_usgs_xeric_flows_study.csv") %>%
  select(StationCode:SampleDate, CollectionMethodCode, Replicate:Result) %>% 
  mutate(SampleDate=mdy(SampleDate))

# only 24 unique stations?!?
ref_xeric_sites <- ref_xeric_data %>% select(StationCode, StationName) %>% 
  distinct(StationCode, .keep_all = T) %>% data.frame()

# are these in reference set above?
ref_xeric_sites %>% filter(StationCode %in% ref_sites$StationCode)
# 7 sites, but need lat/lon for other 17...


# 02 Import Non-Reference Sites -----------------------------------------------------

# non-ref sites
nref_samples <- read_csv("data/bmi/bmi_samples.nonref.csv") %>% # 1677 nonref unique StationCodes
  data.frame() %>% 
  mutate(SampleDate=mdy_hms(SampleDate))

# get metadata  
nref_stations <- read.csv("data/bmi/bmi_stations.nonref.csv", stringsAsFactors = F) %>% 
  dplyr::rename(lat=New_Lat, lon=New_Long) %>% 
  select(StationCode, SiteStatus, lat, lon, County, PSARegion, Eco_III_2010) %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=F)

# ?? 1985 sites...mostly overlap both ref/non-ref
stations_out <- read_excel("data/bmi/bmi_stations.out.xlsx") %>% 
  dplyr::rename(lat=New_Lat, lon=New_Long) %>% 
  select(StationCode, SiteStatus, lat, lon, County, PSARegion, Eco_III_2010, everything()) %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=F)

# 03 Make Map of Sites --------------------------------------------------

# mapview(ref_sites, col.regions="orange", cex=3, layer.name="Ref sites") +
#   mapview(nref_stations, col.regions="black", cex=2, layer.name="Non-ref sites") +
#   mapview(stations_out, zcol="SiteStatus", layer.name="stations_out?")

# show only ref sites
# mapview(ref_sites, col.regions="orange", cex=2, layer.name="Ref sites") +
#   mapview(nref_stations, col.regions="black", layer.name="Non-ref sites") +
#   mapview(stations_out %>% filter(SiteStatus=="Reference"), zcol="SiteStatus", layer.name="stations_out?")

# 04 Clean & Compare Data ----------------------------------------------------

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

# 05 Check all Data and Join ----------------------------------------------

# join ref/non-ref and look for distinct sites only:
sites_df <- bind_rows(ref_sites_df, nref_sites_df, sta_out_df) %>% 
  distinct(StationCode, .keep_all = T)

# get site status 
sites_df %>% group_by(SiteStatus) %>% tally()

# 837 intermediate (nonref)
# 781 reference
# 558 stressed (nonref)

ref_sites_df %>% group_by(SiteStatus) %>% tally() # same as above
nref_sites_df %>% group_by(SiteStatus) %>% tally() # so dropped 4 from this set

# check back, looks good
anti_join(ref_sites_df, sites_df, by=c("StationCode")) %>% tally # all included
anti_join(nref_sites_df, sites_df, by=c("StationCode")) %>% tally # all included

# full metadata set for site status is sites_df, but metadata full is stations_out
bmi_stations_metadat <- sites_df %>% select(StationCode:PSARegion)

# 06 Save Out -------------------------------------------------------------

# save out station metadata for all ref_nonref we have data
save(bmi_stations_metadat, file = "data_output/01_bmi_stations_ref_nonref_metadata.rda")

# 07 Sanity Check with Other data (see script 00) ----------------------------------

# load data to make sure it worked
load("data_output/01_bmi_stations_ref_nonref_metadata.rda") # from script 01, n=2176

# load all bmi data
load("data_output/00_bmi_cleaned_all.rda") # all data from Script 00

# load distinct stations
load("data_output/00_bmi_stations_distinct.rda") # all data from Script 00

# join with full data from other set (sites_df)
length(unique(bmi_stations_distinct$StationCode)) # n=2935
length(unique(bmi_stations_metadat$StationCode)) # n= 2176

# which sites do we not have SS for?
missing_site_status <- anti_join(bmi_stations_distinct, bmi_stations_metadat, by="StationCode") #%>% View("No_match") # 1475 missing

write_csv(missing_site_status, path = "data_output/01_bmi_missing_site_status.csv")

# quick check
# inner_join(bmi_stations_distinct, bmi_stations_metadat, by="StationCode") %>% View("Joined") # 1460 with site status

# some duplicated stations w/ diff lats or longs, or very similar
bmi_stations_distinct_status <- bmi_stations_metadat %>% 
  select(StationCode, SiteStatus, lat, lon) %>% 
  inner_join(., bmi_stations_distinct %>% select(-latitude, -longitude), by="StationCode") # n=1460 that have data

# still n=1460
bmi_stations_distinct_status %>% distinct(StationCode) %>% tally()

# 08 Save Final Dataset ---------------------------------------------------

# save out final dataset that is ONLY the StationCode, lat, lon, and SiteStatus for the BMI data that is currently available/in the "bmi_clean" dataset.

save(bmi_stations_distinct_status, file = "data_output/01_bmi_stations_distinct_status.rda")
