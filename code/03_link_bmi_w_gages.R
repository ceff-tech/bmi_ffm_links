# 03 Linking BMI with GAGES

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
#library(tmap)
library(lubridate)

# Load Data ---------------------------------------------------------------

load("data_output/bmi_final_station_list.rda")
load("data_output/bmi_cleaned_all.rda") # all data
load("data_output/gages_final_250.rda") # all gages
load("data_output/huc12_sf.rda") # h12s

# add watershed area in sqkm
h12 <- h12 %>% 
  mutate(h12_area_sqkm=Shape_Area/1e6)

# check class
class(bmi_all_sites)

# update one col
gages_final <- gages_final %>% 
  mutate(REF_END_YEAR=as.integer(REF_END_YEAR))

# Make Data Spatial -------------------------------------------------------

bmi_clean <- bmi_clean %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) # make spatial

bmi_all_sites <- bmi_all_sites %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=F) # make spatial

# check projs are same
st_crs(bmi_clean)
st_crs(bmi_all_sites)
st_crs(gages_final)

# Gages in Same Time As BMI -----------------------------------------------

# so 110 gages meet temporal scale
gages_final2 <- gages_final %>% filter(REF_END_YEAR>1994)

# Intersect BMI/Gages by H12 ----------------------------------------------

# how many 
# Add H12 to points to BMI and Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE)
bmi_h12 <- st_join(bmi_all_sites, left = FALSE, h12[c("HUC_12","h12_area_sqkm")]) #

gages_h12 <- st_join(gages_final2, left=FALSE, h12[c("HUC_12")]) %>% 
  select(ID, HUC_12) %>% as_tibble() %>% select(-geometry)
class(gages_h12)

# now join based on H12: how many are in same? 421 hucs?
sel_bmi_gages <- inner_join(bmi_h12, gages_h12, by="HUC_12") %>% distinct(StationCode, .keep_all = T)

# number of unique h12s?
length(unique(factor(sel_bmi_gages$HUC_12))) # 59 unique h12
length(unique(sel_bmi_gages$ID)) # 59 unique gages
length(unique(sel_bmi_gages$StationCode)) # 59 unique gages
# so 298 possible BMI sites, 53 gages, in 53 HUC12's

# how many gages?
sel_gages_bmi <- gages_final2 %>% filter(ID %in% sel_bmi_gages$ID)

# select H12s that have points inside:
sel_h12_bmi <- h12[sel_bmi_gages, ]

save(sel_h12_bmi, file="data_output/selected_h12_contain_bmi_gage.rda")
#save(sel_h12_bmi, sel_gages_bmi, sel_bmi_gages, file = "data_output/selected_dat_by_h12_filter.rda")


# Get BMI COMIDs ----------------------------------------------------------

#bmi_comids <- readxl::read_excel("data/BMI_COMIDs_for_Ryan.xlsx")
#sel_bmi_gagestst <- sel_bmi_gages %>% left_join(., bmi_comids, by="StationCode")

# Mapview -----------------------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList)

m1 <- mapview(sel_bmi_gages, col.regions="orange", layer.name="Benthos", alpha=0.5, cex=9) +
  mapview(sel_gages_bmi, col.regions="blue", layer.name="Gages", cex=4) + 
  mapview(sel_h12_bmi, layer="H12", color="dodgerblue", col.regions="cyan", alpha=0.8, lwd=1)

# add measure option  
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")



# Look for Nearest Gages --------------------------------------------------

# look for nearest gages (need to use UTM proj)
library(RANN)

## TRANSFORM TO SAME DATUM
# sel_bmi_sf <- st_transform(sel_bmi_gages, crs = 3310) # use CA Teal albs metric
# sel_gages_sf <- st_transform(sel_gages_bmi, crs=3310)


# get coordinate matrices, could be points and lines or lines and points
sel_bmi_coords <- do.call(rbind, st_geometry(sel_bmi_sf))
sel_bmi_coords <- cbind(sel_bmi_coords, 1:nrow(sel_bmi_coords), sel_bmi_sf$StationCode) %>% as_tibble() %>% 
  mutate(V3=as.integer(V3))
sel_graph_coords <- do.call(rbind, st_geometry(sel_gages_sf))

# fast nearest neighbour search for single nearest
sel_closest <- nn2(sel_bmi_coords[,1:2], sel_graph_coords, k = 1, searchtype = "standard")
sel_closest <- sel_closest %>% bind_cols() %>% as_tibble() %>% 
  left_join(., as_tibble(sel_bmi_coords[,c(3:4)]), by=c("nn.idx"="V3")) %>% 
  dplyr::rename(StationCode=V4)
# join with spatial data
sel_closest_sf <- left_join(sel_closest, sel_bmi_sf,  by=c("StationCode")) %>% 
  st_as_sf(., sf_column_name="geometry", crs=3310)


# fast search for radius 
sel_closest_5k <- nn2(sel_bmi_coords[,1:2], sel_graph_coords, k=3, searchtype = "radius", radius = 5000) # in meters
sel_closest_5k <- sapply(sel_closest_5k, cbind) %>% as_tibble() %>% 
  left_join(., as_tibble(sel_bmi_coords[,c(3:4)]), by=c("nn.idx"="V3")) %>% dplyr::rename(StationCode=V4) %>% 
  # filter NAs
  filter(!is.na(StationCode))

# join with spatial data
sel_closest_5k_sf <- left_join(sel_closest_5k, sel_bmi_sf,  by=c("StationCode")) %>% 
  st_as_sf(., sf_column_name="geometry", crs=3310)


# MAP IT (closest)
m2 <- mapview(sel_bmi_gages, col.regions="orange", layer.name="Benthos", alpha=0.5, cex=3) +
  mapview(sel_gages_bmi, col.regions="blue", layer.name="Gages", cex=3, alpha=0.5) + 
  mapview(sel_h12_bmi, layer="H12", color="dodgerblue", col.regions="cyan", alpha=0.8, lwd=1) +
  mapview(sel_closest_sf, layer.name="Nearest BMI site to Gage", cex=9, col.regions="maroon") +
  mapview(sel_closest_5k_sf, layer.name="5k Radius BMI to Gage", cex=9, col.regions="salmon")

# add measure option  
m2@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# Look for Upstream Sections from Point -----------------------------------

#st_layers("data_output/eflows_bmi.gpkg")
#gages <- st_read(dsn = "data_output/eflows_bmi.gpkg",layer = "gages_ref_20190315",  as_tibble=TRUE, geometry_column="geometry")

#install.packages("devtools")
#devtools::install_github("USGS-R/nhdplusTools")
library(nhdplusTools)

# pick a specific point
#start_point <- st_sfc(st_point(c(-121.057, 38.852)), crs = 4269)
#start_comid <- discover_nhdplus_id(st_sfc(sel_gages_sf$geometry)[1])

# get the comid for the BMI points w no comids using purrr
bmi_segs <- sel_bmi_gages %>% filter(is.na(comid)) %>% select(StationCode, lat, lon, ID, comid)

bmi_missing_coms <- bmi_segs %>% split(.$StationCode) %>% 
  map(~discover_nhdplus_id(.x$geometry))

# flatten
bmi_segs_df <- bmi_missing_coms %>% flatten_df() %>% t() %>% as.data.frame() %>% rownames_to_column("StationCode") %>% rename("comid"=V1)
#save(bmi_segs_df, file = "data_output/sel_bmi_missing_comids.rda")

bmi_comids_rev <- bind_rows(bmi_segs_df, bmi_comids)
# rejoin to get full comids
sel_bmi_gages <- sel_bmi_gages %>% left_join(., bmi_comids_rev, by="StationCode")
summary(sel_bmi_gages)

save(sel_bmi_gages, sel_gages_bmi, file="data_output/sel_bmi_and_gages.rda")

## TRANSFORM TO SAME DATUM
# sel_bmi_sf <- st_transform(sel_bmi_gages, crs=3310) # use CA Teal albs metric
# sel_gages_sf <- st_transform(sel_gages_bmi, crs=3310)
# save(sel_gages_sf, sel_bmi_sf, file = "data_output/sel_gages_bmi_sf_3310.rda")


# usgs_segs <- sel_gages_bmi %>% split(.$ID) %>%
#   map(~discover_nhdplus_id(.x$geometry))

# search by a single comid
# nldi_feature <- list(featureSource = "comid",
#                      featureID = sel_gages_sf$NHDV1_COMID[[1]])
# discover_nldi_navigation(nldi_feature)
# 
# # get all upstream comid segments
# flowline_usgs <- navigate_nldi(nldi_feature = nldi_feature,
#                                 mode = "upstreamMain", 
#                                 data_source = "")

# use purrr
coms <- sel_gages_bmi$NHDV2_COMID
coms_list <- map(coms, ~list(featureSource = "comid", featureID=.x))
coms_list[[40]] # tst check

# test against function:
#feat_check <- map(coms_list, ~discover_nldi_navigation(.x))

# test with mainstem segs
mainstems <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                          mode="upstreamMain",
                                          data_source = ""))

mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                           mode="downstreamMain",
                                           distance_km = 15,
                                           data_source = ""))

# IT WORKSSSSSS!!!!!
# mapview(mainstems, col.regions="blue", col="blue", legend=F) #+ 


# make a single flat layer
mainstems_flat <- mainstemsDS %>%
  set_names(., sel_gages_sf$ID) %>%
  map2(sel_gages_sf$ID, ~mutate(.x, gageID=.y))

# bind together
mainstems_flat <- do.call(what = sf:::rbind.sf,
          args = mainstems_flat)

mainstems_us <- mainstems_flat
mainstems_ds <- mainstems_flat
rm(mainstems_flat)

save(mainstems_us, mainstems_ds, file = "data_output/gages_nhd_flowlines_mainstems.rda")

mapview(mainstems_ds) + mapview(mainstems_us, color="purple")

# Load NHD files ----------------------------------------------------------

load("data_output/sel_bmi_and_gages.rda")
load("data_output/gages_nhd_flowlines_mainstems.rda")
load("data_output/selected_h12_contain_bmi_gage.rda")

mapview(mainstems_ds, color="slateblue", legend=F) +
  mapview(mainstems_us, color="darkblue", legend=F) +
  mapview(sel_gages_bmi, col.regions="cyan", cex=5) + 
  mapview(sel_bmi_gages, col.regions="orange", cex=7)


# Look into COMIDs for bmi ------------------------------------------------

# all stations us of gage:
bmi_us_coms <- sel_bmi_gages %>% filter(comid %in% mainstems_us$nhdplus_comid)

# all stations 15km downstream on mainstem
bmi_ds_coms <- sel_bmi_gages %>% filter(comid %in% mainstems_ds$nhdplus_comid)


m3 <- mapview(bmi_ds_coms, cex=6, col.regions="orange", layer.name="Selected BMI D/S") +  
  mapview(mainstems_ds, color="darkblue", cex=3, layer.name="NHD D/S Flowline 15km", legend=F)+
  mapview(bmi_us_coms, cex=6, col.regions="yellow", layer.name="Selected BMI U/S") +  
  mapview(mainstems_us, color="slateblue", cex=3, layer.name="NHD U/S Flowline", legend=F)+
  mapview(sel_gages_bmi, col.regions="cyan", cex=7, layer.name="Selected USGS Gages") + 
  mapview(sel_bmi_gages, col.regions="gray", cex=3, alpha=0.5, layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


m4 <- mapview(bmi_ds_coms, cex=6, zcol="SiteStatus", layer.name="Selected BMI D/S") +  
  mapview(mainstems_ds, color="darkblue", cex=3, layer.name="NHD D/S Flowline 15km", legend=F)+
  mapview(bmi_us_coms, cex=6, zcol="SiteStatus", layer.name="Selected BMI U/S") +  
  mapview(mainstems_us, color="slateblue", cex=3, layer.name="NHD U/S Flowline", legend=F)+
  mapview(sel_gages_bmi, col.regions="cyan", cex=7, layer.name="Selected USGS Gages") + 
  mapview(sel_bmi_gages, col.regions="gray", cex=3, alpha=0.5, layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m4@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")



# Make into Geopackage ----------------------------------------------------

# use above to get a geopackage for each gage
# subset_gpkg <-subset_nhdplus(comids = mainstems_flat$nhdplus_comid,
#                              output_file = "data_output/selected_nhd/sel_gages_upstream_flowlines.gpkg",
#                              #output_file = tempfile(fileext = ".gpkg"),
#                              nhdplus_data = "download", overwrite = T)

# read in pieces
dbcon <- src_sqlite("data_output/selected_nhd/sel_gages_upstream_flowlines.gpkg", create = F) 
src_tbls(dbcon) # see tables in DB

dbpath <- "data_output/selected_nhd/sel_gages_upstream_flowlines.gpkg"

flowline <- read_sf(dbpath, "NHDFlowline_Network")
catchment <-read_sf(dbpath, "CatchmentSP")
#waterbody <- read_sf(dbpath, "NHDWaterbody")

sel_gages_sf <- st_transform(sel_gages_sf, 4326)

# plot(st_geometry(flowline), col = "blue")
# plot(sel_gages_sf$geometry, cex = 1.5, bg = "red", col="gray60", lwd = 2, pch=21, add = TRUE)
# plot(st_geometry(catchment), add = TRUE, border = alpha("gray", 0.5))
# plot(st_geometry(waterbody), col = "cyan", add = TRUE)

#mapview(sel_closest_sf, col.regions="orange") + 
mapview(sel_bmi_sf, col.regions="maroon", cex=8) + 
  mapview(bmi_h12, col.regions="gray", cex=4) +
  mapview(flowline, col.regions="red", cex=9)+ 
  mapview(sel_gages_sf, col.regions="skyblue")
  
# figure out upstream segs, reqs rename/recapping
flowline %>% 
  rename(COMID=comid, Pathlength=pathlength, 
         LENGTHKM=lengthkm, Hydroseq=hydroseq, 
         LevelPathI=levelpathi, DnHydroseq=dnhydroseq) %>% 
  get_UT(., "8944527")

# COMID, Pathlength, LENGTHKM, Hydroseq, LevelPathI, DnHydroseq

# check to see if comids in flowline ids
keep_coms <- flowline %>% filter(comid %in% bmi_segs_df$comid) %>% left_join(., bmi_segs_df, by = "comid")
#class(keep_coms)

# make a selected BMI station only:
sel_bmi_final <- sel_bmi_sf %>% filter(StationCode %in% keep_coms$StationCode)

# map of everything
m3 <- mapview(keep_coms, cex=8, color="red", layer.name="Selected BMI COMIDs") +  
  mapview(flowline, col.regions="blue", cex=3, layer.name="NHD U/S Flowline", legend=F)+
  mapview(sel_gages_sf, col.regions="skyblue", cex=8, layer.name="Selected USGS Gages") + 
  mapview(sel_bmi_sf, col.regions="gray", cex=3, alpha=0.5, layer.name="Other BMI Sites in H12") + 
  mapview(sel_bmi_final, col.regions="orange", cex=6, layer.name="Selected BMI Sites") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")
  
m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save out data:

save(keep_coms, flowline, sel_gages_sf, sel_bmi_sf, sel_bmi_final, sel_h12_bmi, file = "data_output/final_selected_gages_bmi_alldat.rda")


# Get Watershed Data ------------------------------------------------------

# get list of hucs for download:
sel_h12s <- sel_h12_bmi %>% pull(HUC_12) %>% str_sub(1,4) %>% unique() %>% sort()

# download nhdplus for given watershed:
#nhd_data <- download_nhdplushr(nhd_dir = "data_output/selected_nhd/", hu_list = sel_h12s, download_files = TRUE)

hr_data <- get_nhdplushr("data_output/selected_nhd/", out_gpkg = file.path("data_output/selected_nhd", "nhd_hr_selected.gpkg"), layers = NULL)

(layers <- st_layers(hr_data))
unlink(hr_data)


# Make geopackage from flowlines ------------------------------------------

# create a temp gpkg of this watershed, only works "online/download"
subset_gpkg <-subset_nhdplus(comids = flowline$nhdplus_comid, overwrite = TRUE,
                             # make it perm saved file
                             output_file = "data_output/selected_nhd/test_nhdplus.gpkg",
                             # use webservice:
                             nhdplus_data = "download")

tst_flowline <- sf::read_sf("data_output/selected_nhd/test_nhdplus.gpkg",
                            "NHDFlowline_Network")
tst_catchment <- sf::st_read("data_output/selected_nhd/test_nhdplus.gpkg", 
                             "CatchmentSP")
tst_waterbody <- sf::st_read("data_output/selected_nhd/test_nhdplus.gpkg", 
                             "NHDWaterbody")

# check catch area
nldi_names<- c("COMID", "LENGTHKM", "FTYPE", "TerminalFl", "FromNode", "ToNode", "TotDASqKM", "areasqkm", "StartFlag", "StreamOrde", "StreamCalc", "TerminalPa", "Pathlength", "Divergence", "Hydroseq", "LevelPathI", "geom")

tst_flowline <- tst_flowline %>% select(tolower(nldi_names)) %>% 
  setNames(nldi_names)

catchment_area <- prepare_nhdplus(tst_flowline, 0, 0,
                                  purge_non_dendritic = FALSE, warn = FALSE) %>%
  left_join(., select(tst_flowline, COMID, areasqkm), by = "COMID") %>%
  select(ID = COMID, toID = toCOMID, area = areasqkm)

new_da <- calculate_total_drainage_area(catchment_area) %>% bind_cols(., "da"=catchment_area)
catchment_area2 <- bind_cols(catchment_area, "DA"=new_da) %>% right_join(tst_flowline, by=c("ID"="COMID")) %>% 
  st_as_sf(., sf_column_name="geom", crs=4326)

mapview(catchment_area2, zcol="DA")

