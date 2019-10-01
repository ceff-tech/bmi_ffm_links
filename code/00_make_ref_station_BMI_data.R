# make a list of ref/station/samples for BMI

library(readxl)
library(tidyverse)
library(lubridate)
library(sf)

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
  select(StationCode, SiteStatus, lat, lon, County, PSARegion, Eco_III_2010) %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=F)


# Map ---------------------------------------------------------------------

library(mapview)
mapview(ref_sites, col.regions="orange", layer.name="Ref sites") +
  mapview(nref_stations, col.regions="black", layer.name="Non-ref sites") +
  mapview(stations_out, zcol="SiteStatus", layer.name="stations_out?")


# Comparing Dat -----------------------------------------------------------

# see if there's any matches?
nref_sites_df <- nref_stations %>% as.data.frame() %>% select(-geometry)
ref_sites_df <- ref_sites %>% data.frame() %>% select(-geometry, -BugCode, -SampleID)
sta_out_df <- stations_out %>% data.frame() %>% select(-geometry)

# only 4 matches across both
inner_join(ref_sites_df, nref_sites_df, by="StationCode") %>% tally

# see what's totally diff between the two?
anti_join(ref_sites_df, nref_sites_df, by=c("StationCode")) %>% tally
anti_join(nref_sites_df, ref_sites_df, by=c("StationCode")) %>% tally

# samples shared only in stations_out?
inner_join(ref_sites_df, sta_out_df, by=c("StationCode")) %>% distinct(StationCode) %>% tally # 590 of 781 ref sites in stations_out

inner_join(nref_sites_df, sta_out_df, by=c("StationCode")) %>% distinct(StationCode) %>% tally # all nonref sites in stations_out (n=1399)


# Join All into Spatial List ----------------------------------------------

# join ref/non-ref and look for distinct sites only:
sites_df <- bind_rows(ref_sites_df, nref_sites_df, sta_out_df) %>% 
  distinct(StationCode, .keep_all = T)

# 781 reference
# 837 intermediate (nonref)
# 558 stressed (nonref)

save(sites_df, file = "data_output/bmi_stations_distinct_ref_nonref.rda")

#  Check with Other data (see script 01) ----------------------------------

load("data_output/bmi_stations_distinct_ref_nonref.rda")
load("data_output/bmi_cleaned_all.rda") # all data

# create only spatially distinct stations:
bmi_distinct <- bmi_clean %>% 
  distinct(StationCode, latitude, longitude) %>% 
  rename(lon=longitude, lat=latitude)

# join with full data from other set (sites_df)
length(unique(bmi_distinct$StationCode))
length(unique(sites_df$StationCode))

# some duplicated stations w/ diff lats or longs, or very similar
sites_all_df <- bind_rows(bmi_distinct, sites_df) %>% 
  #distinct(StationCode,.keep_all = T) # with only station=3651
  distinct(StationCode, lat, lon, .keep_all = T) # with lat/lon=5103

bmi_all_sites <- sites_all_df

#save(bmi_all_sites, file = "data_output/bmi_final_station_list.rda")

length(unique(sites_all_df$StationCode))
