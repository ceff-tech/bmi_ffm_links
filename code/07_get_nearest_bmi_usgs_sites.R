# match nearest BMI site to nearest USGS site

library(tidyverse)
library(sf)
library(lubridate)
library(viridis)
library(here)
library(mapview)

# Data --------------------------------------------------------------------

load("data_output/selected_bmi_stations_w_comids.rda") # filtered to distinct
load("data_output/mainstems_bmi_selected_gages.rda") # filtered to bmi only
#load("data_output/mainstems_us_ds_selected_gages.rda") # all w/in same huc12
load("data_output/selected_usgs_gages.rda")
#load(paste0(here(),"/data_output/sel_bmi_and_gages.rda")) # all BMI sites and selected h12 gages
load(paste0(here(),"/data_output/selected_h12_contain_bmi_gage.rda"))

# Filter to Distinct BMI Stations -----------------------------------

# bmi_coms_na <- bmi_coms %>% st_drop_geometry() %>% 
#   filter(is.na(SiteStatus)) %>% distinct(StationCode, .keep_all = T) %>%  
#   select(-c(SiteStatus:Eco_III_2010))
# 
# bmi_coms_nona <- bmi_coms %>% st_drop_geometry() %>% 
#   filter(!is.na(SiteStatus)) %>% 
#   distinct(StationCode, SiteStatus) # 129
# 
# bmi_coms_clean <- bmi_coms %>% st_drop_geometry() %>% 
#   filter(!is.na(SiteStatus)) %>% 
#   distinct(StationCode, SiteStatus, .keep_all = T) # 129
# 
# # now join just site status back in to the NA group in stations that match:
# bmi_coms_nona_stat <- left_join(bmi_coms_na, bmi_coms_nona) %>% 
#   filter(is.na(SiteStatus)) # keep only NAs since these are unique and not duplicated in other dataset
# 
# # now bind them back together:
# bmi_coms_clean2 <- bind_rows(bmi_coms_clean, bmi_coms_nona_stat) %>% 
#   st_as_sf(coords=c("lon","lat"), crs=4326, remove=F)
# 
# # double check unique:
# bmi_coms_clean2 %>% distinct(StationCode) %>% tally()
# class(bmi_coms_clean2)
# 
# mapview(bmi_coms_clean2)
# 
# # rm old layers and resave:
# rm(bmi_coms, bmi_coms_clean, bmi_coms_na, bmi_coms_nona, bmi_coms_nona_stat)
# bmi_coms <- bmi_coms_clean2
# rm(bmi_coms_clean2)
# 
# # save 
# save(bmi_coms, file = "data_output/selected_bmi_stations_w_comids.rda")

## how many unique bmi/gages? 55
#bmi_coms %>% st_drop_geometry %>% distinct(ID) %>% tally()

# drop the other gages from orig set:
#sel_gages_bmi <- sel_gages_bmi %>% filter(ID %in% bmi_coms$ID)
#save(sel_gages_bmi, file = "data_output/selected_usgs_gages.rda")

#mainstems <- mainstems %>% filter(gageID %in% bmi_coms$ID)
#save(mainstems, file="data_output/mainstems_bmi_selected_gages.rda")

# Quick Map Summary -------------------------------------------------------

# 191 specific stations (unique)
mapview(sel_gages_bmi, col.regions="magenta") + 
  mapview(mainstems, color="darkblue", cex=2.5) +
  mapview(bmi_coms, col.regions="orange2", cex=4)

# Add Random Samples on Streamline ----------------------------------------

mainstems_sample <- st_line_sample(st_transform(mainstems, 3310), density = 2/1000 ) # pt every 500m
mainstems_sample <- st_cast(mainstems_sample, "POINT")

mapview(mainstems_sample) + mapview(sel_gages_bmi, col.regions="yellow") + 
  mapview(mainstems, color="skyblue3", lwd=3)

sel_gages_bmi_3310 <- st_transform(sel_gages_bmi, 3310)


closest <- list()
for(i in seq_len(nrow(sel_gages_bmi_3310))){
  closest[[i]] <- mainstems_sample[which.min(
    st_distance(mainstems_sample, sel_gages_bmi_3310[i,]))]
}

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
save(bmi_nearest, file="data_output/bmi_nearest_usgs_stations.rda")



# Link Regions ------------------------------------------------------------

# read in fish regions:
regions <- st_read("data/umbrella_sp_regions.shp")

# spatial join gage sites with regions, adds ATTRIBUTES, retains ALL pts if left=TRUE,
sel_gages_bmi_3310 <- st_join(sel_gages_bmi_3310, left = FALSE, regions["huc_region"])

mapview(sel_gages_bmi_3310, zcol="huc_region") + 
  mapview(bmi_coms, col.regions="gray", cex=4) +
  mapview(bmi_nearest, col.regions="orange", cex=6)
