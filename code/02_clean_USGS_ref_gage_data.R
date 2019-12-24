# 02 Read Hydroclassification/Flows DB
## R. Peek
## Creates dataset of USGS reference stations

## DATA OUT:
### - gages_final (all distinct usgs gages from TNC/CEFF, n=250)
### "02_gages_final.rda"


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)
library(mapview)
#library(tmap)
library(lubridate)
library(Hmisc)
library(janitor)


# Read in Ref Gages Lists --------------------------------------------------

# this is from the CEFF Database used for stream classification/eflows
gage_223 <- read_csv("data/gages_ref_223_period_record.csv") %>% 
  mutate(CEFF = TRUE,
         ID = paste0("T", gage))

# gagesII, has all US, filter to CA only
gages2 <- read_xlsx("data/gages_II_March2013_Info.xlsx") %>% 
  filter(STATE=="CA")

# make quick map of ALL gages
gages2_sf <- gages2 %>% select(STAID, ID, LAT_GAGE, LNG_GAGE, STATE, COUNTYNAME_SITE, CLASS, AGGECOREGION, HYDRO_DISTURB_INDX) %>% 
  st_as_sf(coords = c("LNG_GAGE","LAT_GAGE"), 
           remove = F, crs=4326)
# save(gages2_sf, file = "data_output/gages2_sf.rda")
# mapview(gages2_sf, zcol="HYDRO_DISTURB_INDX")
# mapview(gages2_sf, zcol="AGGECOREGION")
# mapview(gages2_sf, zcol="CLASS")

# usgs list based on gagesII
gages_usgs <- read_xlsx("data/gages_ca_USGS_reference_screen_Aug2016_ref_only.xlsx") %>% filter(!is.na(FINAL_REFERENCE))

# Merge Datasets ----------------------------------------------------------

#gages_ceff_usgs <- left_join(gage_223, gages_usgs, by=c("ID"))
#gages_ceff_usgs <- inner_join(gage_223, gages_usgs, by=c("ID"))
gages_usgs_ceff <- left_join(gages_usgs, gage_223, by=c("ID"))

# pull it together 
gages_all <- left_join(gages_usgs_ceff, gages2, by=c("ID"))

# look for non-matching records between datasets
# gages in USGS but not in ceff 223
# usgs_gages_anti_223 <- anti_join(gages_usgs, gage_223, by=c("ID"))
# 
# # gages in CEFF but not in USGS
# ceff_gages_anti_usgs <- anti_join(gage_223, gages_usgs, by=c("ID"))
# 
# # gages in CEFF but not in gages2
# ceff_gages_anti_gage2 <- anti_join(gage_223, gages2, by=c("ID"))

# filter: 
gages_final <-gages_all %>% select(-c(data, DECISION_NOTES:GAGES_II_SCREENING_COMMENTS, STAID, STANAME, HUC02:NAWQA_SUID, PCT_DIFF_NWIS:last_col()))

# map
gages_final <- st_as_sf(gages_final, coords = c("LONGITUDE","LATITUDE"), 
         remove = F, crs=4326)

mapview(gages_final)


# Save out Gages ----------------------------------------------------------

save(gages_final, file = "data_output/02_gages_final_250.rda")

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

# Connect to Geopackage ---------------------------------------------------

# dbcon <- src_sqlite("data_output/eflows_bmi.gpkg", create = F)
# src_tbls(dbcon) # see tables in DB
# 
# # check layers
# dsn_link <- paste0(here::here(),"/data_output/eflows_bmi.gpkg")
# st_layers(dsn_link)
# 
# # read 93 Ref gages
# ref_gages <- st_read(dsn = dbcon, "gages_ref_20190315", as_tibble=TRUE) #%>% 
# # select(-geometry) %>%  
# # st_as_sf(coords=c("point_x", "point_y"), crs=4326)
# 
# gages <- tbl(dbcon, "gages_final_ref_20180703")  %>% 
#   collect() %>% 
#   filter(!is.na(LATITUDE)) %>% 
#   st_as_sf(coords = c("LONGITUDE","LATITUDE"), remove = F, crs=4326)


