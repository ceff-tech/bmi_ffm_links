


library(tidyverse) # yes
library(sf)
library(tictoc) # timing stuff
library(tidylog) # good for logging what happens

options(scipen = 100)



# ALTERATION DATA --------------------------------------------------------------

# alteration list from Ted
usgs_alt_list <- read_csv("data/usgs/usgs_gages_final_altered_list.csv")

# clean up: drop the "T" from the ID, drop cols
usgs_alt_list <- usgs_alt_list %>% 
  mutate(gage_id=as.numeric(gsub("^T",replacement = "", ID))) %>% 
  #ID = gsub("^T", replacement = "", ID)) %>% 
  select(gage_id, ID:LONGITUDE, FLOW_YRS_POST_1950:REF_END_YEAR) 

# look at reference?
table(usgs_alt_list$FINAL_REFERENCE) # 30=Y here, 


# FINAL DATA FOR FFC ------------------------------------------------------

# final gage list used in FFC
# includes all reference gages (n=250) from gages2 database
# includes all alteration gages from Ted (but only the 576 of 814 that returned data from FFC)
load("data_output/01_usgs_all_gages.rda") # final gages list
table(usgs_final_all$CEFF_type)


# DATA FROM SAM/BELLE 05112020 --------------------------------------------

usgs_nonref <- read_csv("data/usgs/impaired_locations_10grps_20200511.csv")

# cross ref gages with gage list from ted:
setdiff(usgs_alt_list$gage_id, usgs_nonref$ID) # only one diff


# JOIN DATA ---------------------------------------------------------------

# join
usgs_nonref_joined <- left_join(usgs_alt_list, usgs_nonref, by=c("gage_id"="ID"))

# make spatial
usgs_nonref_joined <- st_as_sf(usgs_nonref_joined, coords = c("LONGITUDE","LATITUDE"), crs=4326, remove=FALSE)

# map
library(mapview)
mapview(usgs_nonref_joined, zcol="ward", cex=5)


# JOIN WHAT GAGES HAVE FFC METRICS ----------------------------------------

load("data_output/02_usgs_all_ffm_data.rda")
ffc_list <- g_all_percentiles %>% distinct(gage_id, .keep_all=TRUE) %>% 
  select(list_id, comid, gage_id)

ffc_dat_list <- left_join(ffc_list, usgs_final_all, by=c("gage_id")) %>% 
  distinct(list_id, .keep_all=TRUE) 

ffc_dat_list <- left_join(ffc_dat_list, usgs_nonref, by=c("gage_id"="ID")) %>% 
  # switch CEFF types
  mutate(CEFF_type = case_when(
    !is.na(ward) ~ "ALT",
    TRUE ~ "REF"
  )) %>% 
  select(-c(count_nu, end_yr, x, y))

ffc_dat_list %>% filter(!is.na(ward)) %>% group_by(CEFF_type) %>% tally
ffc_dat_list %>% group_by(CEFF_type) %>% tally

write_csv(ffc_dat_list, path="data/usgs/final_gage_ffc_list.csv")  
  
