# match nearest BMI site to nearest USGS site

library(tidyverse)
library(sf)
library(lubridate)
library(viridis)
library(here)
library(mapview)

# Data --------------------------------------------------------------------

load("data_output/05_selected_bmi_stations_w_comids.rda") # filtered to distinct
load("data_output/03_selected_h12_contain_bmi_gage.rda") # h12
#load("data_output/05_mainstems_us_ds_selected_gages.rda") # filtered to all in same H12

# generated from first code chunk/section below
load("data_output/07_mainstems_bmi_selected_gages.rda") # selected mainstem comids
load("data_output/07_selected_bmi_stations_w_comids.rda") # from code below
load("data_output/07_selected_usgs_gages.rda") # distinct gages
load("data/07_umbrella_sp_regions.rda")

# Filter to Distinct BMI Stations -----------------------------------

bmi_coms_na <- bmi_coms %>% st_drop_geometry() %>%
  filter(is.na(SiteStatus)) %>% distinct(StationCode, .keep_all = T) %>%
  select(StationCode, longitude, latitude, ID, comid, SiteStatus) # 72

bmi_coms_nona <- bmi_coms %>% st_drop_geometry() %>%
  filter(!is.na(SiteStatus)) %>%
  distinct(StationCode, SiteStatus) # 70
 
bmi_coms_clean <- bmi_coms %>% st_drop_geometry() %>%
  filter(!is.na(SiteStatus)) %>%
  distinct(StationCode, SiteStatus, .keep_all = T) # 70
 
# now join just site status back in to the NA group in stations that match:
bmi_coms_nona_stat <- left_join(bmi_coms_na, bmi_coms_nona) %>%
  filter(is.na(SiteStatus)) # keep only NAs since these are unique and not duplicated in other dataset

# now bind them back together: # n=142
bmi_coms_clean2 <- bind_rows(bmi_coms_clean, bmi_coms_nona_stat) %>%
  st_as_sf(coords=c("longitude","latitude"), crs=4326, remove=F)

# double check unique:
bmi_coms_clean2 %>% distinct(StationCode) %>% tally()
class(bmi_coms_clean2)
 
mapview(bmi_coms_clean2)
 
## rm old layers and resave:
rm(bmi_coms, bmi_coms_clean, bmi_coms_na, bmi_coms_nona, bmi_coms_nona_stat)
bmi_coms <- bmi_coms_clean2
rm(bmi_coms_clean2)

# mapview
mapview(bmi_coms)

# save out as shp
st_write(bmi_coms, "data_output/07_selected_bmi_stations_w_comid.shp")

## save 
save(bmi_coms, file = "data_output/07_selected_bmi_stations_w_comids.rda")

## how many unique bmi/gages? 46
bmi_coms %>% st_drop_geometry %>% distinct(ID) %>% tally()

# drop the other gages from orig set:
sel_gages_bmi <- sel_gages_bmi %>% filter(ID %in% bmi_coms$ID)

# write out
st_write(sel_gages_bmi, "data_output/07_selected_usgs_gages.shp")
save(sel_gages_bmi, file = "data_output/07_selected_usgs_gages.rda")

mainstems <- mainstems %>% filter(gageID %in% bmi_coms$ID) # n=1278
save(mainstems, file="data_output/07_mainstems_bmi_selected_gages.rda")

# Quick Map Summary -------------------------------------------------------

# 142 specific BMI stations (unique), 34 H12s, 46 USGS Gages
mapview(sel_gages_bmi, col.regions="magenta") + 
  mapview(mainstems, color="darkblue", cex=2.5) +
  mapview(bmi_coms, col.regions="orange2", cex=4)

# Add Random Samples on Streamline ----------------------------------------

# mainstems_sample <- st_line_sample(st_transform(mainstems, 3310), density = 2/1000 ) # pt every 500m
# mainstems_sample <- st_cast(mainstems_sample, "POINT")
# 
# mapview(mainstems_sample) + mapview(sel_gages_bmi, col.regions="yellow") + 
#   mapview(mainstems, color="skyblue3", lwd=3)
# 
# sel_gages_bmi_3310 <- st_transform(sel_gages_bmi, 3310)
# 
# 
# closest <- list()
# for(i in seq_len(nrow(sel_gages_bmi_3310))){
#   closest[[i]] <- mainstems_sample[which.min(
#     st_distance(mainstems_sample, sel_gages_bmi_3310[i,]))]
# }

# Find Closest Sites Along Streamline -------------------------------------

# first convert to UTM (metric)
sel_gages_bmi_3310 <- st_transform(sel_gages_bmi, 3310)
mainstems_3310 <- st_transform(mainstems, 3310)
bmi_coms_3310 <- st_transform(bmi_coms, 3310)

# find nearest COMID
#bmi_nearest <- st_nearest_feature(bmi_coms_3310, mainstems_3310)
#bmi_nearest <- mainstems_3310[bmi_nearest,]

# find nearest GAGE
bmi_nearest <- st_nearest_feature(sel_gages_bmi_3310, bmi_coms_3310)
bmi_nearest <- bmi_coms_3310[bmi_nearest,]

# map
mapview(sel_gages_bmi, col.regions="blue") + 
  mapview(bmi_coms, col.regions="gray", cex=4) +
  mapview(bmi_nearest, col.regions="orange", cex=6)
  
# find distance from bmi sites to usgs sites
#tst <- st_distance(sel_gages_bmi, bmi_coms, by_element = F) %>% as_tibble()
# https://github.com/r-spatial/sf/issues/799

# save out the "NEAREST" site
save(bmi_nearest, file="data_output/07_selected_bmi_nearest_usgs_stations.rda")


# Link Regions ------------------------------------------------------------

# read in fish regions:
#ca_sp_regions <- st_read("data/umbrella_sp_regions.shp")

# save back out for easier spatial:
#save(ca_sp_regions, file = "data/07_umbrella_sp_regions.rda")

# spatial join gage sites with regions, adds ATTRIBUTES, retains ALL pts if left=TRUE,
sel_gages_bmi_3310 <- st_join(sel_gages_bmi_3310, left = TRUE, ca_sp_regions["huc_region"])

mapview(sel_gages_bmi_3310, zcol="huc_region") + 
  mapview(bmi_coms, col.regions="gray", cex=4) +
  mapview(bmi_nearest, col.regions="orange", cex=6)
