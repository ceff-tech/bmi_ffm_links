# 05 add Regions to Data


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(tidylog)


# Load Data ---------------------------------------------------------------

# CSCI Data
bmi_csci_por_trim <- read_rds("data_output/04_selected_csci_ffm_por_trim.rds")

# make spatial
bmi_csci_por_trim <- bmi_csci_por_trim %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4269, remove=F)

# CA REGIONS: these are from fish umbrella but maybe use something different (ecoregions?)
ca_sp_regions <- read_sf("data/spatial/umbrella_sp_regions.shp") %>% 
  st_transform(4269)


# Prep Spatial Data -------------------------------------------------------

# check crs:
st_crs(bmi_csci_por_trim)
st_crs(ca_sp_regions)

# Spatial Join ------------------------------------------------------------

# join with regions and add huc_region, make sure both df are in 4269
bmi_csci_por_trim <- st_join(bmi_csci_por_trim, left = TRUE, ca_sp_regions["huc_region"])

# make a simpler layer for just editing:
bmi_csci_sites <- bmi_csci_por_trim %>%
  dplyr::distinct(StationCode, site_id, .keep_all = TRUE)
length(unique(bmi_csci_sites$StationCode))

# view and update w mapedit
mapview(bmi_csci_sites, col.regions="orange") + 
  mapview(ca_sp_regions)

library(mapedit)
library(leafpm)
library(leaflet)

## use this to select features (returns a list of stationcodes)
# selectMap(
#   leaflet() %>%
#     addTiles() %>%
#     addPolygons(data=ca_sp_regions, layerId = ~huc_region, color = "orange") %>%
#     addCircleMarkers(data = bmi_csci_sites, layerId = ~StationCode)
#   )

## sites to add to central valley
cvalley_add <- c("514FC1278", "514RCR001", "534DCC167")

## sites to add to great_basin
gbasin_add <- c("603MAM004", "630PS0005")

## sites to add to southcoast
scoast_add <- c("628PS1307","628PS1179","719MISSCK","719TRMDSS","719FCA001")

# Amargosa site is "609PS0053" = mojave?

# so 294 NA's in each
summary(as.factor(bmi_csci_por$huc_region))
summary(as.factor(bmi_csci_por_trim$huc_region))

# use case_when to replace
bmi_csci_por_trim <- bmi_csci_por_trim %>%
  mutate(huc_region = case_when(
    StationCode %in% cvalley_add ~ "central_valley",
    StationCode %in% gbasin_add ~ "great_basin",
    StationCode %in% scoast_add ~ "south_coast",
    TRUE ~ huc_region))

bmi_csci_por <- bmi_csci_por %>%
  mutate(huc_region = case_when(
    StationCode %in% cvalley_add ~ "central_valley",
    StationCode %in% gbasin_add ~ "great_basin",
    StationCode %in% scoast_add ~ "south_coast",
    TRUE ~ huc_region))

# only NAs are now for AMARGOSA site
summary(as.factor(bmi_csci_por_trim$huc_region))
table(bmi_csci_por_trim$huc_region)

## map and double check:
mapview(bmi_csci_por_trim, zcol="huc_region", layer.name="Selected Sites", viewer.suppress=FALSE) +
  mapview(ca_sp_regions, zcol="huc_region", layer.name="HUC Regions", alpha.regions=0.1)

# Make GAGE/BMI geoms -----------------------------------------------------

# make SF geometry fields for BMI
bmi_csci_por_bmi <- bmi_csci_por %>% 
  rename("geom_bmi"=geometry) 

# make a USGS geom field
bmi_csci_por_usgs <- bmi_csci_por %>% st_drop_geometry() %>% 
  st_as_sf(., coords=c("LONGITUDE","LATITUDE"), crs = 4326, remove=FALSE) %>% 
  # rename the geometry col
  rename("geom_usgs"=geometry)

# quick view
mapview(bmi_csci_por_bmi, cex=7, col.regions="orange", 
        layer.name="Selected BMI comids") +
  mapview(bmi_csci_por_usgs, col.regions="skyblue", cex=4, color="blue2", layer.name="Selected USGS Gages") +
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines")
