# 03 Linking BMI with GAGES


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
#library(tmap)
library(lubridate)

# Load Data ---------------------------------------------------------------

#load("data_output/bmi_stations_distinct_samples.rda") # all distinct samples
load("data_output/bmi_stations_distinct_xy_methods.rda") # all distinct samples
load("data_output/bmi_cleaned_all.rda") # all data
load("data_output/gages_final_250.rda") # all gages
load("data_output/huc12_sf.rda") # h12s

# add watershed area in sqkm
h12 <- h12 %>% 
  mutate(h12_area_sqkm=Shape_Area/1e6)

# check class
class(bmi_stations)

# update one col
gages_final <- gages_final %>% 
  mutate(REF_END_YEAR=as.integer(REF_END_YEAR))


# Make Data Spatial -------------------------------------------------------

bmi_clean <- bmi_clean %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) # make spatial

# check projs are same
st_crs(bmi_clean)
st_crs(bmi_stations)
st_crs(gages_final)

# Intersect BMI/Gages by H12 ----------------------------------------------

# Add H12 to points to BMI and Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE)
bmi_h12 <- st_join(bmi_stations, left = FALSE, h12[c("HUC_12","h12_area_sqkm")]) #

gages_h12 <- st_join(gages_final, left=FALSE, h12[c("HUC_12")]) %>% 
  select(ID, HUC_12) %>% as_tibble() %>% select(-geometry)
class(gages_h12)

# now join based on H12: how many are in same?
bmi_gages <- inner_join(bmi_h12, gages_h12, by="HUC_12")
class(bmi_gages)

# so 703 possible links between bmi_gages, in 117 HUC12's

# how many gages?
gages_bmi <- gages_final %>% filter(ID %in% bmi_gages$ID)

# now need to look at distance and timing (filter by gage years)

# select H12s that have points inside:
h12_bmi <- h12[bmi_gages, ]

# map
mapview(bmi_gages, col.regions="orange", layer.name="Benthos", alpha=0.5, cex=9) +
  mapview(gages_bmi, col.regions="blue", layer.name="Gages", cex=4) + 
  mapview(h12_bmi, layer="H12", color="darkblue", col.regions=NA, alpha=0.8, lwd=1)


# Gages in Same Time As BMI -----------------------------------------------

# so 110 gages meet temporal scale
gages_final2 <- gages_final %>% filter(REF_END_YEAR>1994)

# of those how many are close to bmi sites?


# Look for Nearest Gages --------------------------------------------------

# look for nearest gages (need to use UTM proj)
library(RANN)

## TRANSFORM TO SAME DATUM
bugs_filt_distinct <- st_transform(bugs_filt_distinct, crs = 3310) # use CA Teal albs metric
ref_gages <- st_transform(ref_gages, crs=3310)

