library(sf)
library(nhdplusTools)
library(mapview)
library(tidyverse)


# FIRST GET A STARTING POINT ----------------------------------------------

# get starting point for river
start_point <- st_sfc(st_point(c(-120.24414, 36.77130)), crs = 4269) %>% 
  st_transform(3310)
point2 <- st_sfc(st_point(c(-120.97509, 37.34909)), crs = 4269) %>% 
  st_transform(3310)
point3 <- st_sfc(st_point(c(-120.82642, 37.27896)), crs = 4269) %>% 
  st_transform(3310)
st_crs(start_point)

# view starting point:
mapview(start_point) + mapview(point2) + mapview(point3)

# NEXT GET COMID FOR USE IN NHDTOOLs --------------------------------------

start_comid <- discover_nhdplus_id(start_point) # get the NHD ID of the river segment for start point, needs to be a SF formatted point
start_comid
(point2_comid <- discover_nhdplus_id(point2))
(point3_comid <- discover_nhdplus_id(point3))

# THEN USE COMID AND SF POINT TO GET STREAMLINES --------------------------

# make nhd format for getting streamlines:
nldi_feature <- list(featureSource = "comid", featureID = start_comid)
nldi_feature2 <- list(featureSource = "comid", featureID = point2_comid)
nldi_feature3 <- list(featureSource = "comid", featureID = point3_comid)

# get data 
tst <- navigate_nldi(nldi_feature = nldi_feature,
              mode="downstreamMain",
              data_source = "")

tst2 <- navigate_nldi(nldi_feature = nldi_feature2,
                      mode="upstreamTributaries",distance_km = 70,
                      data_source = "")

tst3 <- navigate_nldi(nldi_feature = nldi_feature3,
                      mode="upstreamMain",
                      data_source = "")

# map
mapview(tst, color="darkgreen", legend=F) +
mapview(tst2, color="blue", legend=F) + 
  mapview(tst3, color="purple", legend=F)


# drop a segment
tst3_drop <- tst3 %>% filter(!nhdplus_comid==17091269)




# MERGE and GET METADATA --------------------------------------------------

flowdata <- rbind(tst, tst2, tst3)

mapview(flowdata, color="darkgreen")



# GET NHDPLUS ATTRIBUTES --------------------------------------------------

mainstem_attribs <- subset_nhdplus(comids = tst$nhdplus_comid,
               output_file ="nhdplus.gpkg",
               nhdplus_data = "download",
               overwrite = TRUE)


# read in and plot:
st_layers("nhdplus.gpkg")

# read into R
tst_flowline <- sf::read_sf(dsn="nhdplus.gpkg",layer="NHDFlowline_Network")
tst_catchment <- sf::st_read("nhdplus.gpkg", 
                             "CatchmentSP")
tst_waterbody <- sf::st_read("nhdplus.gpkg", 
                             "NHDWaterbody")


mapview(tst_flowline, legend=F) + mapview(tst_waterbody, legend=F)
