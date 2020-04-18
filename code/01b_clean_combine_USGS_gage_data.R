# 01 Get Gages for Pairing
## R. Peek
## Creates dataset of USGS reference and impaired stations

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(lubridate)
library(janitor)
library(mapview)
library(readxl)
#library(tmap)
#library(Hmisc)
options(scipen = 100)

# Get All Daily USGS Gages ------------------------------------------------

# all daily USGS gages:
load("data_output/00_usgs_ca_all_daily_flow_gages.rda") # all gages for metadata
ca_usgs_gages <- ca_usgs_gages %>% 
  mutate(gage_id=as.numeric(site_id),
         ID = paste0("T", site_id)) %>%
  dplyr::select(gage_id, ID, station_nm:geometry)


# Get All Altered USGS Gages ----------------------------------------------

# alteration list from Ted
usgs_alt_list <- read_csv("data/usgs/usgs_gages_final_altered_list.csv")

# clean up: drop the "T" from the ID, drop cols
usgs_alt_list <- usgs_alt_list %>% 
  mutate(gage_id=as.numeric(gsub("^T",replacement = "", ID))) %>% 
         #ID = gsub("^T", replacement = "", ID)) %>% 
  select(gage_id, ID:LONGITUDE, FLOW_YRS_POST_1950:REF_END_YEAR) 

# look at reference?
table(usgs_alt_list$FINAL_REFERENCE) # 30=Y here, 
# the Y are included in reference data but pre-regulation, 
# so some stretch of years is a mix of ref/altered

# load the list of data already downloaded from FFC (n=576) data we already have:
load("data_output/usgs_altered_ffc_list.rda")
usgs_alt_ffc <- names(usgs_ffc_alt) %>% as_tibble() %>% 
  mutate(gage_id=as.numeric(value)) %>% select(-value)
rm(usgs_ffc_alt)

# join these lists to metadata?
usgs_alt_list <- left_join(usgs_alt_ffc, usgs_alt_list, by=c("gage_id"))

# get XY and date begin/end
usgs_alt_final <- left_join(usgs_alt_list, ca_usgs_gages %>% 
                              select(ID, lat, lon, date_begin, date_end, count_nu), by=c("ID")) %>% 
  #add column for "ALTERED"
  mutate(CEFF_type="ALT") %>% 
  select(-c(lat:count_nu, geometry)) %>% 
  select(gage_id, ID:CEFF_type)

table(usgs_alt_final$CEFF_type)

# cleanup:
rm(usgs_alt_list, usgs_alt_ffc)

# Read in Ref Gages Lists --------------------------------------------------

# this is from the CEFF Database used for stream classification/eflows
gage_223 <- read_csv("data/usgs/gages_ref_223_period_record.csv") %>% 
  mutate(CEFF = TRUE,
         ID = paste0("T", gage))

# gagesII, has all US, filter to CA only
gages2_ca <- read_xlsx("data/usgs/gages_II_March2013_Info.xlsx") %>% 
  filter(STATE=="CA")

# make quick map of ALL gages
gages2_ca_sf <- gages2_ca %>% select(STAID, ID, LAT_GAGE, LNG_GAGE, STATE, COUNTYNAME_SITE, CLASS, AGGECOREGION, HYDRO_DISTURB_INDX) %>% 
  st_as_sf(coords = c("LNG_GAGE","LAT_GAGE"), 
           remove = F, crs=4326)

# mapview(gages2_ca_sf, zcol="HYDRO_DISTURB_INDX")
# mapview(gages2_ca_sf, zcol="CLASS")

# usgs list based on gagesII
gages_usgs <- read_xlsx("data/usgs/gages_ca_USGS_reference_screen_Aug2016_ref_only.xlsx") %>% filter(!is.na(FINAL_REFERENCE))

## MERGE REF DATASETS
# join gage lists
gages_usgs_ceff <- left_join(gages_usgs, gage_223, by=c("ID")) %>% 
  #add column for "ALTERED"
  mutate(CEFF_type="REF",
         gage_id = as.numeric(gsub("^T", "", ID)))

# filter to columns of interest, get same names from altered dataset:
colnames_keep <- names(usgs_alt_final)

# join w all ca dataset to get same columns
usgs_ref_final <- gages_usgs_ceff %>% 
  select(any_of(colnames_keep)) %>% 
  select(gage_id, ID:CEFF_type)

# clean up
rm(gage_223, gages2_ca, gages2_ca_sf, gages_usgs, gages_usgs_ceff)

# Combine Ref and Alt Datasets --------------------------------------------

# clean one weird row format
usgs_ref_final <- usgs_ref_final %>% mutate(REF_END_YEAR=as.numeric(REF_END_YEAR))

# bind together
usgs_final_all <- bind_rows(usgs_ref_final, usgs_alt_final)
rm(usgs_ref_final, usgs_alt_final)

# check
summary(usgs_final_all)

# make spatial to make a map
usgs_final_all <- st_as_sf(usgs_final_all, coords = c("LONGITUDE","LATITUDE"), 
         remove = F, crs=4326)

# make a map
mapview(usgs_final_all)


# Save out Gages ----------------------------------------------------------

save(usgs_final_all, file = "data_output/01_usgs_all_gages.rda")


# Look at CEFF DB ----------------------------------------------------------

# # need to be connected via vpn to the CWS server:
# mdblink <- "/Volumes/projects/environmental_flows/DATA/hydrogeomorph_classification/California_Hydro_Geomorphic_Classification.mdb"
# 
# # see table names:
# mdb.get(mdblink, tables=TRUE)
# 
# # get single table
# ref_gages <- mdb.get(mdblink, tables="UCD_Ref_Gages_CA_Hydrologic_Classification") %>% 
#   # clean names w janitor
#   clean_names() %>% 
#   dplyr::select(-shape) # drop shape field
# 
# # try with sf
# ref_gages_sf <- st_as_sf(ref_gages, coords = c("longdd","latdd"), 
#                          remove = F, crs=4326)
# names(ref_gages_sf)
# 
# mapview(ref_gages_sf)


