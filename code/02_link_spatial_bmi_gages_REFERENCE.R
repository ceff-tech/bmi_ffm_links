# 02 Spatially Linking BMI with REFERENCE GAGES
## R. Peek
## Spatially link the BMI station data with the reference USGS gages

## DATA OUT:
### - sel_h12_bmi (all huc12s with gage/bmi sites inside them, n=53)
    ### "data_output/02_selected_h12_contain_bmi_gage.rda"
### - sel_bmi_gages, sel_gages_bmi (selected bmi sites and gages that are in same H12, n=244 and n=59)
    ### "data_output/02_selected_bmi_and_gages_same_h12.rda"
### - bmi_comids (all bmi station COMIDs, generated w nhdtools, n=3873)
    ### "data_output/02_bmi_all_stations_comids.rda"
### - bmi_coms_dat, bmi_coms_final
    ### "02_final_bmi_stations_dat_reference.rda"
### - mainstems_us, mainstems_ds (generated w nhdtools, using 15km ds)
    ### "data_output/02_selected_nhd_flowlines_mainstems.rda"

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
#devtools::install_github("USGS-R/nhdplusTools")
library(nhdplusTools)
#library(tmap)

# Load Data ---------------------------------------------------------------

load("data_output/00_bmi_cleaned_all.rda") # all data
load("data_output/00_bmi_cleaned_stations_distinct_xy.rda") # distinct bmi_clean_stations
#load("data_output/01_bmi_stations_ref_nonref_metadata.rda") # all possible bmi stations (may not have data): bmi_stations_metadat
load("data_output/01_bmi_cleaned_stations_w_site_status.rda") # bmi_clean_stations_ss (includes site status)
load("data_output/01_gages_reference_final_250.rda") # all gages
load("data_output/huc12_sf.rda") # CA h12s

# add watershed area in sqkm
h12 <- h12 %>% 
  mutate(h12_area_sqkm=Shape_Area/1e6)

# check class, should both be SF
class(bmi_clean_stations) # all sites 
class(bmi_clean_stations_ss) # site status

# update one col
gages_final <- gages_final %>% 
  mutate(REF_END_YEAR=as.integer(REF_END_YEAR))

# Make Data Spatial -------------------------------------------------------

# make into an SF object
bmi_clean <- bmi_clean %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F)

# stations with site status
bmi_clean_stations_ss <- bmi_clean_stations_ss %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=F)

# check projs are same (should all be 4326)
st_crs(bmi_clean)
st_crs(bmi_clean_stations)
st_crs(bmi_clean_stations_ss)
st_crs(gages_final)

# FILTER 01. Gages in Same Time As BMI ------------------------------------

# all BMI data starts after 1994, so filter to gages with data > 1994
gages_ref_filt <- gages_final %>% filter(REF_END_YEAR>1994) # gives 110 REF gages

# plot
# mapview(bmi_clean_stations, col.regions="orange", cex=4) + mapview(bmi_clean_stations_ss, zcol="SiteStatus") + 
#   mapview(gages_ref_filt, col.regions="skyblue4")

# FILTER 02. Intersect BMI/Gages by H12 ----------------------------------

# filter to gages and BMI stations that occur in SAME H12

# First need to add H12 associated with BMI/Gages pts 
# This adds ATTRIBUTES, retains ALL pts if left=TRUE)
# using distinct BMI stations
bmi_h12 <- st_join(bmi_clean_stations, left = TRUE, h12[c("HUC_12","h12_area_sqkm")]) #

# add H12 to gages data set
gages_h12 <- st_join(gages_ref_filt, left=TRUE, h12[c("HUC_12")]) %>% 
  select(ID, HUC_12, LATITUDE, LONGITUDE) %>% st_drop_geometry()
class(gages_h12) # note we dropped the sf/geometry class here

# Now join based on H12: note can only join if one is sf & one is dataframe
# how many bmi stations vs HUC12s?
sel_bmi_gages <- inner_join(bmi_h12, gages_h12, by="HUC_12") %>% 
  #distinct(StationCode, .keep_all = T)
  distinct(StationCode, ID, .keep_all = T)
# yields n=244 BMI stations in same HUC12 as a REFERENCE GAGE

# number of unique h12s?
length(unique(factor(sel_bmi_gages$HUC_12))) # 53 unique h12
length(unique(sel_bmi_gages$ID)) # 59 unique gages
length(unique(sel_bmi_gages$StationCode)) # 210 unique BMI Stations

# filter to gages in same HUC12 that we have BMI data for:
# e.g., 59 gages but only 55 have associated BMI site with them
sel_gages_bmi <- gages_ref_filt %>% filter(ID %in% sel_bmi_gages$ID)

# create the H12 layer of only selected H12s (have points inside)
sel_h12_bmi <- h12[sel_bmi_gages, ]

#save(sel_h12_bmi, file="data_output/02_selected_h12_contain_bmi_gage.rda")

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

# BMI COMIDs: EXISTING DATA -----------------------------------

# get preloaded list of COMIDs from Raffi
bmi_comids <- readxl::read_excel("data/BMI_COMIDs_for_Ryan.xlsx")

# join with the selected BMI sites in same HUC12 as gage
sel_bmi_gages <- sel_bmi_gages %>% left_join(., bmi_comids, by="StationCode")

# BMI COMIDS: GET NEW/MISSING COMIDS --------------------------

library(nhdplusTools)

## TRANSFORM TO SAME DATUM
sel_bmi_gages <- st_transform(sel_bmi_gages, crs = 3310) # use CA Teal albs metric
sel_gages_bmi <- st_transform(sel_gages_bmi, crs=3310)

# TEST with a Single Value: 
#start_point <- st_sfc(st_point(c(-121.057, 38.852)), crs = 4269)
#discover_nhdplus_id(start_point)
#start_comid <- discover_nhdplus_id(st_sfc(sel_gages_bmi$geometry)[1])

# get the comid for the BMI points w no comids using purrr
bmi_segs <- sel_bmi_gages %>% filter(is.na(comid)) %>% select(StationCode, latitude, longitude, ID, comid)

# drop sf geometry first to make mutate and add ID column
bmi_missing_coms <- bmi_segs %>% st_drop_geometry() %>% as.data.frame()
bmi_missing_coms <- bmi_missing_coms %>% rowid_to_column() %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>% 
  st_transform(3310) %>% group_split(rowid) %>%  
  map(~discover_nhdplus_id(.x$geometry))

# check one by one:
#discover_nhdplus_id(bmi_segs$geometry[1]) # 17683290

# flatten into single dataframe instead of list
bmi_segs_df <-bmi_missing_coms %>% flatten_dfc() %>% t() %>% as.data.frame() %>% 
  rename("comid"=V1) %>% 
  mutate(StationCode = bmi_segs$StationCode)

# don't really need to save this out
#save(bmi_segs_df, file = "data_output/02_selected_bmi_missing_comids.rda")

# rejoin
bmi_comids <- bind_rows(bmi_segs_df, bmi_comids)

# update the BMI station COMIDs file:
write_rds(bmi_comids, path="data_output/02_bmi_all_stations_comids.rds")

# rejoin to get full comids
sel_bmi_gages <- sel_bmi_gages %>% left_join(., bmi_comids, by="StationCode") %>% 
  # remove old col and rename:
  select(-comid.x) %>% rename(comid=comid.y)
summary(sel_bmi_gages)

# find out how many unique BMI stations?
length(unique(sel_bmi_gages$StationCode)) # some of these have multiple gages associated with them
# find out how many unique USGS gages?
length(unique(sel_bmi_gages$ID)) # 59 total gages, should match sel_gages_bmi

save(sel_bmi_gages, sel_gages_bmi, file="data_output/02_selected_bmi_and_gages_same_h12.rda")


# GET UPSTREAM FLOWLINES --------------------------------------------------

## TRANSFORM TO UTM DATUM
sel_bmi_sf <- st_transform(sel_bmi_gages, crs=3310) # use CA Teale/Albs metric
sel_gages_sf <- st_transform(sel_gages_bmi, crs=3310)

# get all USGS gage COMIDs for given NHD segement
usgs_segs <- sel_gages_sf %>% split(.$ID) %>%
  map(~discover_nhdplus_id(.x$geometry))

# use purrr to loop through get missing COMIDs
coms <- sel_gages_sf$NHDV2_COMID
coms_list <- map(coms, ~list(featureSource = "comid", featureID=.x))
coms_list[[40]] # test a single element

# make sure no duplicate comids
coms_list %>% 
  purrr::map(~ length(.x$featureID)>1) %>% 
  unlist() %>% table() # should all be FALSE

# Get all mainstem segments UPSTREAM of gage
mainstemsUS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                          mode="upstreamMain",
                                          data_source = ""))

# make a single flat layer
mainstems_flat_us <- mainstemsUS %>%
  set_names(., sel_gages_bmi$ID) %>%
  map2(sel_gages_bmi$ID, ~mutate(.x, gageID=.y))

# bind together
mainstems_us <- do.call(what = sf:::rbind.sf,
                        args = mainstems_flat_us)

# rm temp files
rm(mainstemsUS, mainstems_flat_us)

# GET DOWNSTREAM FLOWLINES ------------------------------------------------

# Get all mainstem segments DOWNSTREAM of gage (15 km limit)
mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                           mode="downstreamMain",
                                           distance_km = 15,
                                           data_source = ""))

# make a single flat data frame (not a list)
mainstems_flat_ds <- mainstemsDS %>%
  set_names(., sel_gages_bmi$ID) %>%
  map2(sel_gages_bmi$ID, ~mutate(.x, gageID=.y))

# bind together
mainstems_ds <- do.call(what = sf:::rbind.sf,
                        args = mainstems_flat_ds)

# rm temp files
rm(mainstems_flat_ds, mainstemsDS)

# save out US and DS
save(mainstems_us, mainstems_ds, file = "data_output/02_selected_nhd_flowlines_mainstems.rda")

# make a map
# mapview(mainstems, col.regions="blue", col="blue", legend=F, lwd=2.5) +
#   mapview(mainstemsDS, color="skyblue4", lwd=4, legend=F) + 
#   mapview(sel_bmi_gages, col.regions="orange", legend=F) + 
#   mapview(sel_gages_bmi, col.regions="dodgerblue", legend=F, cex=5) 

# FILTER TO BMI SITES IN USGS MAINSTEM COMIDS -----------------------------

# all stations us of gage:
bmi_coms_us <- sel_bmi_gages %>% 
  filter(comid %in% mainstems_us$nhdplus_comid) %>% 
  mutate(to_gage = "US")
  
# all stations 15km downstream on mainstem
bmi_coms_ds <- sel_bmi_gages %>% 
  filter(comid %in% mainstems_ds$nhdplus_comid) %>% 
  mutate(to_gage = "DS")

# combine US and DS
bmi_coms_final <- rbind(bmi_coms_ds, bmi_coms_us)

# distinct stations:
bmi_coms_final %>% st_drop_geometry() %>% 
  distinct(StationCode, ID) %>% tally() # 161
bmi_coms_final %>% st_drop_geometry() %>% distinct(comid) %>% tally() # 106

# Make a FINAL MAP -------------------------------------------------------

# create a final map of selected gages and bmi + huc12 + flowlines

# this map of all sites selected U/S and D/S
m3 <- mapview(bmi_coms_us, cex=6, col.regions="orange", layer.name="Selected BMI D/S") +  
  mapview(mainstems_ds, color="darkblue", cex=3, layer.name="NHD D/S Flowline 15km", legend=F)+
  mapview(bmi_coms_us, cex=6, col.regions="yellow", layer.name="Selected BMI U/S") +  
  mapview(mainstems_us, color="slateblue", cex=3, layer.name="NHD U/S Flowline", legend=F)+
  mapview(sel_gages_bmi, col.regions="cyan", cex=7, layer.name="Selected USGS Gages") + 
  # these are all gages in same H12
  mapview(sel_bmi_gages, col.regions="gray", cex=3, alpha=0.5, layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# SAVE OUT ----------------------------------------------------------------

# pull BMI sites and get list of data, first join with orig full dataset:
bmi_coms_dat <- left_join(bmi_coms_final, st_drop_geometry(bmi_clean), by="StationCode") %>% select(-latitude.y, -longitude.y) %>% 
  rename(lat = latitude.x, lon = longitude.x) %>% 
  # drop NAs (72 sites: is.na(bmi_coms_dat$SampleID)
  filter(!is.na(SampleID))

# now look at how many unique samples are avail: n=266 unique samples
bmi_coms_dat %>% as.data.frame() %>% group_by(SampleID) %>% distinct(SampleID) %>% tally

# now look at how many unique stations: n=142 stations
bmi_coms_dat %>% as.data.frame() %>% group_by(StationCode) %>% distinct(StationCode) %>% tally

# save out
save(bmi_coms_dat, bmi_coms_final, file = "data_output/02_final_bmi_stations_dat_reference.rda")

# OLD CODE BELOW ----------------------------------------------------------

# everything below here is extra code, doesn't do anything necessary to the rest of the analysis. This is all code to get watershed/flowline stuff. 
# was run as experimental tests to make sure comid joins made sense.

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

sel_gages_bmi <- st_transform(sel_gages_bmi, 4326)

# plot(st_geometry(flowline), col = "blue")
# plot(sel_gages_bmi$geometry, cex = 1.5, bg = "red", col="gray60", lwd = 2, pch=21, add = TRUE)
# plot(st_geometry(catchment), add = TRUE, border = alpha("gray", 0.5))
# plot(st_geometry(waterbody), col = "cyan", add = TRUE)

#mapview(sel_closest_sf, col.regions="orange") + 
mapview(sel_bmi_gages, col.regions="maroon", cex=8) + 
  mapview(bmi_h12, col.regions="gray", cex=4) +
  mapview(flowline, col.regions="red", cex=9)+ 
  mapview(sel_gages_bmi, col.regions="skyblue")
  
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
sel_bmi_final <- sel_bmi_gages %>% filter(StationCode %in% keep_coms$StationCode)

# map of everything
m3 <- mapview(keep_coms, cex=8, color="red", layer.name="Selected BMI COMIDs") +  
  mapview(flowline, col.regions="blue", cex=3, layer.name="NHD U/S Flowline", legend=F)+
  mapview(sel_gages_bmi, col.regions="skyblue", cex=8, layer.name="Selected USGS Gages") + 
  mapview(sel_bmi_gages, col.regions="gray", cex=3, alpha=0.5, layer.name="Other BMI Sites in H12") + 
  mapview(sel_bmi_final, col.regions="orange", cex=6, layer.name="Selected BMI Sites") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")
  
m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save out data:

save(keep_coms, flowline, sel_gages_bmi, sel_bmi_gages, sel_bmi_final, sel_h12_bmi, file = "data_output/final_selected_gages_bmi_alldat.rda")


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

# Look for Nearest Gages By RADIUS -------------------------------------------
# this section no longer applies, not useful to search by distance
# # look for nearest gages (need to use UTM proj)
# library(RANN)
# 
# ## TRANSFORM TO SAME DATUM
# sel_bmi_sf <- st_transform(sel_bmi_gages, crs = 3310) # use CA Teal albs metric
# sel_gages_sf <- st_transform(sel_gages_bmi, crs=3310)
# 
# # get coordinate matrices, could be points and lines or lines and points
# sel_bmi_coords <- do.call(rbind, st_geometry(sel_bmi_sf))
# sel_bmi_coords <- cbind(sel_bmi_coords, 1:nrow(sel_bmi_coords), sel_bmi_sf$StationCode) %>% as_tibble() %>% 
#   mutate(V3=as.integer(V3))
# sel_graph_coords <- do.call(rbind, st_geometry(sel_gages_sf))
# 
# # fast nearest neighbour search for single nearest
# sel_closest <- nn2(sel_bmi_coords[,1:2], sel_graph_coords, k = 1, searchtype = "standard")
# sel_closest <- sel_closest %>% bind_cols() %>% as_tibble() %>% 
#   left_join(., as_tibble(sel_bmi_coords[,c(3:4)]), by=c("nn.idx"="V3")) %>% 
#   dplyr::rename(StationCode=V4)
# # join with spatial data
# sel_closest_sf <- left_join(sel_closest, sel_bmi_sf,  by=c("StationCode")) %>% 
#   st_as_sf(., sf_column_name="geometry", crs=3310)
# 
# 
# # fast search for radius 
# sel_closest_5k <- nn2(sel_bmi_coords[,1:2], sel_graph_coords, k=3, searchtype = "radius", radius = 5000) # in meters
# sel_closest_5k <- sapply(sel_closest_5k, cbind) %>% as_tibble() %>% 
#   left_join(., as_tibble(sel_bmi_coords[,c(3:4)]), by=c("nn.idx"="V3")) %>% dplyr::rename(StationCode=V4) %>% 
#   # filter NAs
#   filter(!is.na(StationCode))
# 
# # join with spatial data
# sel_closest_5k_sf <- left_join(sel_closest_5k, sel_bmi_sf,  by=c("StationCode")) %>% 
#   st_as_sf(., sf_column_name="geometry", crs=3310)
# 
# 
# # MAP IT (closest)
# m2 <- mapview(sel_bmi_gages, col.regions="orange", layer.name="Benthos", alpha=0.5, cex=3) +
#   mapview(sel_gages_bmi, col.regions="blue", layer.name="Gages", cex=3, alpha=0.5) + 
#   mapview(sel_h12_bmi, layer="H12", color="dodgerblue", col.regions="cyan", alpha=0.8, lwd=1) +
#   mapview(sel_closest_sf, layer.name="Nearest BMI site to Gage", cex=9, col.regions="maroon") +
#   mapview(sel_closest_5k_sf, layer.name="5k Radius BMI to Gage", cex=9, col.regions="salmon")
# 
# # add measure option  
# m2@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")
# 
