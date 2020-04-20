# 02 Spatially Linking BMI with ALL GAGES
## R. Peek 2020

## Spatially link the BMI station data with the USGS gage data using multiple spatial filters

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
library(lubridate)
#devtools::install_github("USGS-R/nhdplusTools")
library(nhdplusTools)
#library(tmap)

# 01. Load Data ---------------------------------------------------------------

load("data_output/00_bmi_cleaned_all.rda") # all data
load("data_output/00_bmi_stations_distinct.rda") # distinct bmi stations
load("data_output/01_bmi_stations_distinct_status.rda") # bmi_stations w site status
load("data_output/01_usgs_all_gages.rda") # final gages list
load("data_output/00_usgs_ca_all_daily_flow_gages.rda") # all gages for metadata
# bmi_comids <- readRDS("data_output/02_bmi_all_stations_comids.rds")
load("data_output/huc12_sf.rda") # CA h12s


ca_usgs_gages <- ca_usgs_gages %>% 
  mutate(gage_id=as.numeric(site_id),
         ID = paste0("T", site_id)) %>%
  dplyr::select(gage_id, ID, station_nm:geometry)

# 02. Make Data Spatial -------------------------------------------------------

# make spatial

# add watershed area in sqkm
h12 <- h12 %>% 
  mutate(h12_area_sqkm=Shape_Area/1e6) %>% 
  st_transform(4269)

bmi_clean <- bmi_clean %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>% 
  st_transform(4269)

bmi_stations_distinct <- bmi_stations_distinct %>% 
  st_transform(4269)

# stations with site status
bmi_stations_distinct_status <- bmi_stations_distinct_status %>%
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=F) %>%
  st_transform(4269)

# make a gages dataset
gages <- usgs_final_all %>% st_transform(4269)

# check projs are same
st_crs(bmi_clean)
st_crs(bmi_stations_distinct)
st_crs(bmi_stations_distinct_status)
st_crs(gages)

# 03. FILTER-Gages in Same Time As BMI -----------------------------------

# join
gages <- left_join(gages, st_drop_geometry(ca_usgs_gages[,c(1,12:14)]), by="gage_id") %>% 
  # add end year
  mutate(end_yr = year(date_end),
         end_yr = if_else(is.na(end_yr), REF_END_YEAR, end_yr))

# Filter to same temporal scale as BMI data, years must be post 1994 (n=483 remaining)
gages_all_filt <- gages %>% filter(end_yr > 1994)

table(gages_all_filt$CEFF_type) # ALT=353, REF=130

# plot
#mapview(bmi_clean_stations, col.regions="orange", cex=4) + 
  #mapview(bmi_clean_stations_ss, zcol="SiteStatus") + 
  #mapview(gages_all_filt, col.regions="skyblue4")

# 04. FILTER-Intersect BMI/Gages by H12 -----------------------------------

# Add H12 to points to BMI and Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE), using BMI DISTINCT STATIONS
bmi_h12 <- st_join(bmi_stations_distinct, left = TRUE, h12[c("HUC_12","h12_area_sqkm")])

# Add H12 to all gages
gages_h12 <- st_join(gages_all_filt, left=TRUE, h12[c("HUC_12")]) %>%
  st_drop_geometry()

# now join based on H12: how many bmi stations vs HUC12s? n=2188 stations
sel_bmi_gages <- inner_join(bmi_h12, gages_h12, by="HUC_12") %>% 
  distinct(StationCode, ID, .keep_all = T)
  #st_drop_geometry %>% distinct(StationCode, ID, .keep_all = T) # gets 1000 sites!

# view
#mapview(sel_bmi_gages)

# number of unique h12s?
length(unique(factor(sel_bmi_gages$HUC_12))) # unique h12=208
length(unique(sel_bmi_gages$ID)) # unique gages=266
length(unique(sel_bmi_gages$StationCode)) # unique BMI Stations=747

# Get Selected Gages ONLY:  # n=266
sel_gages_bmi <- gages_all_filt %>% filter(ID %in% sel_bmi_gages$ID) %>% 
  distinct(ID, .keep_all = T)

# select H12s that have points inside: # n=209
sel_h12_bmi <- h12[sel_bmi_gages, ]

# save out
write_rds(sel_h12_bmi, path="data_output/03_selected_h12_all_gages.rds")
write_rds(sel_gages_bmi, path="data_output/03_selected_usgs_h12_all_gages.rds")
write_rds(sel_bmi_gages, path="data_output/03_selected_bmi_h12_all_gages.rds")

# * Map of Filtered Gages ------------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList)

m1 <- mapview(sel_bmi_gages, col.regions="orange", layer.name="Benthos", alpha=0.5, cex=9) +
  mapview(sel_gages_bmi, col.regions="blue", layer.name="Gages", cex=4) + 
  mapview(sel_h12_bmi, layer="H12", color="dodgerblue", col.regions="cyan", alpha=0.8, lwd=1, legend=FALSE)

# add measure option  
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# 05. BMI COMIDS: GET NEW/MISSING COMIDS --------------------------

# library(nhdplusTools)
# 
# ## TRANSFORM TO SAME DATUM
# sel_bmi_gages <- st_transform(sel_bmi_gages, crs = 3310) # use CA Teale albs metric
# sel_gages_bmi <- st_transform(sel_gages_bmi, crs=3310)
# 
# # get COMIDs for everything
# bmi_segs <- st_transform(bmi_stations_distinct, crs=3310) %>% 
#   select(StationCode, longitude, latitude) %>% 
#   mutate(comid=NA)
# 
# # drop sf geometry first to make mutate and add ID column
# bmi_missing_coms <- bmi_segs %>% st_drop_geometry() %>% as.data.frame()
# bmi_missing_coms <- bmi_missing_coms %>% 
#   st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>% 
#   st_transform(3310) %>% group_split(StationCode) %>%
#   set_names(., bmi_segs$StationCode) %>%
#   map(~discover_nhdplus_id(.x$geometry))
# 
# # flatten into single dataframe instead of list
# bmi_segs_df <-bmi_missing_coms %>% flatten_dfc() %>% t() %>% 
#   as.data.frame() %>% 
#   rename("comid"=V1) %>% rownames_to_column(var = "StationCode")
# 
# # rm COMIDs starting with "V"
# bmi_comids <- bmi_segs_df %>% filter(!grepl("^V", StationCode))
# 
# # update with original BMI station COMIDs file:
# # get preloaded list of COMIDs from Raffi
# bmi_comids2 <- readxl::read_excel("data/bmi/bmi_comids_for_ryan.xlsx") %>%
#   rename(comid2=comid)
# 
# # join BMI comids together and see if there's overlap
# bmi_comids_bind <- left_join(bmi_comids, bmi_comids2, by="StationCode")
# 
# # check and see only the differences:
# # bmi_comids_bind[!(bmi_comids_bind$comid %in% bmi_comids_bind$comid2),] %>% View()
# 
# write_rds(bmi_comids_bind, path="data_output/02_bmi_all_stations_comids.rds")

# * Load And Join -----------------------------------------------------------

# load previous stuff run from Step 05 BMI COMIDS
bmi_comids <- read_rds("data_output/03_bmi_all_stations_comids.rds")

# rejoin with the selected BMI sites in same HUC12 as gage
sel_bmi_gages <- sel_bmi_gages %>% left_join(., bmi_comids, by="StationCode") # n=2188

summary(sel_bmi_gages$comid) # no missing

write_rds(sel_bmi_gages, path="data_output/03_selected_bmi_h12_all_gages.rds")

# 06. GET GAGE COMIDS --------------------------------------------------

# if any comids missing use code below

## TRANSFORM TO UTM datum for flowlines
# sel_bmi_sf <- st_transform(sel_bmi_gages, crs=3310) # use CA Teale albs metric
# sel_gages_sf <- st_transform(sel_gages_bmi, crs=3310)
# 
# # get the COMID for each gage in list
# usgs_segs <- sel_gages_bmi %>% split(.$site_id) %>%
#   map(~discover_nhdplus_id(.x$geometry))
# 
# # now have a list of all the missing COMIDs, check for dups
# usgs_segs %>% 
#   purrr::map_lgl(~ length(.x)>1) %>% 
#   #table() # 3 are FALSE
#   .[.==TRUE] # get values that are TRUE
# 
# # view comids
# usgs_segs["11186000"]
# 
# # fix 326 and 327 which pull two segs
# usgs_segs["11186000"] <- 14971709
# usgs_segs["11186001"] <- 14971711
# usgs_segs["11404240"] <- 2775510
# 
# # double check again:
# usgs_segs %>% 
#   purrr::map_lgl(~ length(.x)>1) %>% table() # all FALSE
# 
# # save the USGS station COMIDs file:
# write_rds(usgs_segs, path="data_output/02_selected_usgs_gages_comids.rds")
# 

# 07. GET UPSTREAM FLOWLINES FROM GAGE --------------------------------------------------

# use a list of comids to make a list to pass to the nhdplusTools function
coms_list <- map(sel_gages_sf$NHDV1_COMID, ~list(featureSource = "comid", featureID=.x))
coms_list[[226]] # tst check, should list feature source and featureID

# Get Mainstem Segs, needed to do in chunks if needed and rbind
mainstemsUS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                          mode="upstreamMain",
                                          data_source = ""))

# transform the sf layer to match mainstems crs (4326)
sel_gages_sf <- sel_gages_sf %>% st_transform(4326)

# make a single flat layer
mainstems_flat_us <- mainstemsUS %>%
  set_names(., sel_gages_sf$gage_id) %>%
  map2(sel_gages_sf$gage_id, ~mutate(.x, gageID=.y))


# bind together
mainstems_us <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_us, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_us <- mainstems_us %>% 
  mutate(from_gage = "US")

rm(mainstems_flat_us, mainstemsUS)

# 08. GET DOWNSTREAM FLOWLINES FROM GAGE ------------------------------------------------

# get NHD segments downstream of selected USGS gages, 10 km buffer
mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                           mode="downstreamMain",
                                           distance_km = 10,
                                           data_source = ""))

# make a single flat layer
mainstems_flat_ds <- mainstemsDS %>%
  set_names(., sel_gages_sf$gage_id) %>%
  map2(sel_gages_sf$gage_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_ds <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_ds, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_ds <- mainstems_ds %>% 
  mutate(from_gage = "DS")

rm(mainstems_flat_ds, mainstemsDS)

# bind all mainstems
mainstems_all <- rbind(mainstems_us, mainstems_ds)

# 09. SAVE OUT STREAMLINES FOR GAGES ------------------------------------------

save(mainstems_all, file="data_output/03_selected_nhd_mainstems_gages.rda")

save(mainstems_us, mainstems_ds, file = "data_output/03_selected_nhd_mainstems_gages_us_ds.rda")

# * PREVIEW MAP ----------------------------------------------------------

sel_bmi_gages<-readRDS("data_output/03_selected_bmi_h12_all_gages.rds")
sel_gages_bmi<-readRDS("data_output/03_selected_usgs_h12_all_gages.rds")
sel_h12_bmi<-readRDS("data_output/03_selected_h12_all_gages.rds")
load("data_output/03_selected_nhd_mainstems_gages.rda")

# mapview breaks but mapdeck WORKS
library(mapdeck)
set_token(Sys.getenv("MAPBOX_TOKEN"))

mapdeck(
  style=mapdeck_style("dark")
) %>% 
  add_path(data = mainstems_all, stroke_colour = "from_gage", tooltip="nhdplus_comid", auto_highlight = TRUE) %>% 
  add_sf(data = sel_gages_bmi, 
         fill_colour="#00EEEE", tooltip="site_id", 
         layer_id="USGS Gages", radius=500) %>% 
  add_sf(data = st_transform(sel_bmi_gages, 4326), fill_colour="#EE7600", radius=300, tooltip="StationCode",
         layer_id="BMI Sites")


# 10. FILTER TO BMI SITES IN USGS MAINSTEM COMIDS -----------------------------

# get distinct segs only
mainstems_distinct <- mainstems_all %>% distinct(nhdplus_comid, .keep_all=TRUE)

# all BMI comids that occur in list of mainstem NHD comids: (n=648)
bmi_coms_final <- sel_bmi_gages %>% 
  dplyr::filter(comid %in% as.integer(mainstems_distinct$nhdplus_comid))

# distinct comid/station/gages combinations:
bmi_coms_final %>% st_drop_geometry() %>% 
  distinct(StationCode, ID) %>% tally() # n=648

# distinct COMIDs
bmi_coms_final %>% st_drop_geometry() %>% distinct(comid) %>% tally() # 346

# distinct GAGES
bmi_coms_final %>% st_drop_geometry() %>% distinct(ID) %>% tally() #

# Measuring Nearest and Line Lengths --------------------------------------

# get a site, mainstem river and gage
bmi1 <- st_transform(bmi_coms_final[1,], 4326) # the site
ln1 <- mainstems_distinct %>% filter(gageID==11532500) # mainstem
gage1 <- sel_gages_bmi %>% filter(gage_id==11532500) # the gage

# calculates nearest point from each riverline segment to single point
pts_nearest <- st_nearest_points(ln1, bmi1)

# find shortest difference from point to nearest line and cast to point
pt_best <- st_cast(pts_nearest[which.min(st_length(pts_nearest))], "POINT")[1]

# quick map
mapview(pt_best, col.regions="orange") + 
  mapview(bmi1, color="red") + 
  mapview(ln1, color="green")

# this generates poins on line that has lat/lon 
ln_points <- ln1 %>% st_transform(3310) %>% 
  group_by(nhdplus_comid) %>%
  # with lat/lon: units::set_units(25, m)
  st_segmentize(., dfMaxLength = units::set_units(25, m)) %>% 
  st_sf() %>%
  st_cast('POINT') %>% 
  ungroup()

# mm1 <- mapview(ln_points) + mapview(ln_points2)
# mm1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# find index of point closest to BMI site
ln_pt_best <- which.min(st_distance(st_transform(bmi1, 4326), st_transform(ln_points, 4326)))
ln_pt_nearest <- ln_points[ln_pt_best,]

# make a segment from split pt to end
segment1 <- ln_points[ln_pt_best:nrow(ln_points),] %>% 
  group_by(nhdplus_comid) %>% 
  dplyr::summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") %>% 
  ungroup()

# make a segment from split to beginning
segment2 <- ln_points[1:ln_pt_best,] %>% 
  group_by(nhdplus_comid) %>% 
  dplyr::summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") %>% 
  ungroup %>% 
  st_union() %>% # merge back into a single line
  st_as_sf()

# make a final map
mapview(segment1, lwd=5, color="orange", legend=FALSE) +
  mapview(segment2, lwd=5, color="blue", legend=FALSE) +
  mapview(bmi1, col.regions="red") + 
  mapview(ln_pt_nearest, col.regions="yellow")+
  mapview(ln1, lwd=2, color="green")


# 11. FINAL MAP -------------------------------------------------------

# create a final map of selected gages and bmi + huc12 + flowlines

# get all BMI not selected...check why not on map
bmi_not_selected <- sel_bmi_gages %>% filter(!as.character(comid) %in% mainstems_all$nhdplus_comid) # should be 352

# get all gages selected
gages_selected <- sel_gages_bmi %>% 
  #distinct(gage_id, .keep_all=TRUE) #%>% 
  filter(gage_id %in% bmi_coms_final$gage_id)

# this map of all sites selected U/S and D/S
m3 <- mapview(bmi_coms_final, cex=6, col.regions="orange", layer.name="Selected BMI comids") +  
  mapview(mainstems_distinct, zcol="from_gage", cex=3, layer.name="NHD Flowlines")+
  mapview(gages_selected, col.regions="cyan", cex=7, layer.name="Selected USGS Gages") + 
  # these are all bmi in same H12 but not selected
  mapview(bmi_not_selected, col.regions="gray", cex=3.2, alpha=0.5, layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save this final map out as:"map_of_final_gages_bmi_stations_all_gages"
#mapshot(m3, url = paste0(here::here(),"/figs/03_map_of_final_bmi_stations_gages_h12s.html"))

# need to go through and add some stations that fall just out side (didn't snap with mainstem), and create 
# better layer showing "selected" USGS gages, and those that aren't paired.


# SAVE OUT ----------------------------------------------------------------

# pull BMI sites and get list of data, first join with orig full dataset:
bmi_coms_dat <- left_join(bmi_coms_final, st_drop_geometry(bmi_clean), by="StationCode") %>% 
  select(-latitude.y, -longitude.y) %>% 
  rename(lat = latitude.x, lon = longitude.x)

# now look at how many unique samples are avail: n=915 unique samples
bmi_coms_dat %>% as.data.frame() %>% group_by(SampleID) %>% distinct(SampleID) %>% tally

# now look at how many unique stations: n=489 stations
bmi_coms_dat %>% as.data.frame() %>% group_by(StationCode) %>% distinct(StationCode) %>% tally

# save out
save(bmi_coms_dat, bmi_coms_final, file = "data_output/03_selected_final_bmi_stations_dat_all_gages.rda")
