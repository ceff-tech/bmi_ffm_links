# 03 Spatially Linking BMI with ALL GAGES
## R. Peek 2020

## Spatially link the BMI station data with the USGS gage data using multiple spatial filters

# For only map, start at Step 10.

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

# FISH REGIONS
ca_sp_regions <- read_sf("data/spatial/umbrella_sp_regions.shp", as_tibble = T)

# ALL BMI DATA CLEANED
load("data_output/00_bmi_cleaned_all.rda") # all data

# ALL BMI SAMPLES W CSCI SCORES
load("data_output/00_bmi_samples_distinct_csci.rda")

# ALL BMI DISTINCT STATIONS
load("data_output/00_bmi_stations_distinct.rda") # distinct bmi stations

# ALL GAGES W FFC DATA
load("data_output/01_usgs_all_gages.rda") # final gages list

# HUC12s
load("data_output/huc12_sf.rda") # CA h12s

# BMI COMIDs (from Section 05)
bmi_comids <- readRDS("data_output/03_bmi_all_stations_comids.rds")

# 02. Make Data Spatial -------------------------------------------------------

# make spatial
bmi_clean <- bmi_clean %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F)

bmi_samples_distinct_csci <- bmi_samples_distinct_csci %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F)

bmi_stations_distinct <- bmi_stations_distinct %>% 
  st_transform(4326)

gages <- usgs_final_all %>% st_transform(4326)
rm(usgs_final_all)

ca_sp_regions <- ca_sp_regions %>% st_transform(4326)

# check projs are same
st_crs(bmi_clean)
st_crs(bmi_stations_distinct)
st_crs(gages)
st_crs(h12)
st_crs(ca_sp_regions)

# 03. FILTER-Gages in Same Time As BMI -----------------------------------

# Filter to same temporal scale as BMI data, years must be post 1994 (n=483 remaining)
gages_all_filt <- gages %>% filter(end_yr > 1994)

table(gages_all_filt$CEFF_type) # ALT=353, REF=130

# 04. FILTER-Intersect BMI/Gages by H12 -----------------------------------

# Add H12 to points to BMI and Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE), using BMI DISTINCT STATIONS
bmi_h12 <- st_join(bmi_stations_distinct, left = TRUE, h12[c("HUC_12")])

# Add H12 to all gages
gages_h12 <- st_join(gages_all_filt, left=TRUE, h12[c("HUC_12")]) %>%
  st_drop_geometry()

# now join based on H12: what BMI stations share same H12 as USGS gage? (N=1000)
sel_bmi_gages <- inner_join(bmi_h12, gages_h12, by="HUC_12") %>% 
  distinct(StationCode, ID, .keep_all = T) # n=1000

# number of unique?
length(unique(factor(sel_bmi_gages$HUC_12))) # h12=208
length(unique(sel_bmi_gages$ID)) # gages=266
length(unique(sel_bmi_gages$StationCode)) # BMI Stations=747

# make sure these have CSCI scores: of those in same H12, how many have CSCI scores? N=552
sel_bmi_gages_csci <- left_join(sel_bmi_gages, st_drop_geometry(bmi_samples_distinct_csci)[,c(1:2,5,12:14)], by="StationCode") %>% 
  filter(!is.na(csci)) %>% 
  distinct(StationCode, ID, .keep_all=TRUE)

# number of unique?
length(unique(factor(sel_bmi_gages_csci$HUC_12))) # h12=163
length(unique(sel_bmi_gages_csci$ID)) # gages=207
length(unique(sel_bmi_gages_csci$StationCode)) # BMI Stations=419

# Get Selected Gages ONLY:  # n=207 (that have CSCI scores)
sel_gages_bmi <- gages_all_filt %>% 
  filter(ID %in% sel_bmi_gages_csci$ID) %>% 
  distinct(ID, .keep_all = T)

# select H12s that have points inside: # n=163
sel_h12_bmi <- h12[sel_bmi_gages_csci, ]
sel_h12_gages <- h12[gages_all_filt, ]

# * Map of Filtered Gages ------------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList)

# a map of all gages and BMI stations that fall within the same H12

# get the gages not selected
gages_not_selected <- gages_all_filt %>% 
  filter(!ID %in% sel_bmi_gages_csci$ID)

table(sel_gages_bmi$CEFF_type) # ALT=151  REF=56
table(gages_not_selected$CEFF_type) # ALT=194  REF=74

# get bmi NOT selected with CSCI
bmi_not_selected <- bmi_samples_distinct_csci %>% 
  filter(!is.na(csci)) %>% 
  filter(!StationCode %in% sel_bmi_gages_csci$StationCode) %>% 
  distinct(StationCode, .keep_all=TRUE)

# this map of all sites selected U/S and D/S
m1 <- mapview(sel_bmi_gages_csci, cex=6, col.regions="orange", 
              layer.name="Selected BMI Stations") +  
  mapview(sel_gages_bmi, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all bmi or gages in same H12 but not selected
  mapview(gages_not_selected, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(bmi_not_selected, col.regions="gold2", color="gray20", cex=3.2, 
          layer.name="Other BMI Sites w CSCI Scores") + 
  mapview(bmi_stations_distinct, col.regions="gray", color="gray20", cex=3, 
          layer.name="All BMI Sites") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=FALSE, layer.name="HUC12") + 
  mapview(sel_h12_gages, col.regions="gray50", alpha.region=0.1, 
          color="darkblue", legend=FALSE, layer.name="HUC12 Gages")

m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

## NOTES
# - Bear River Site doesn't have CSCI: Station 515PS0622 (Gage T11424000)
# - MF/Marble Kaweah Sites: Gage T11206500 and Gage T11208000 suitable, add Stations 553WER224, 553KRMAPC
# - Alameda Site needs to be included: Gage T11172945, Station 204ALA525 (double check that Arroyo Hondo stays in too)
# - Arroyo Seco Gage T11152050 and Station 309SET look good but slightly off line


# * Save Out -----------------------------------------------------------------

# save out
write_rds(sel_h12_bmi, path="data_output/03_selected_h12_all_gages.rds")
write_rds(sel_gages_bmi, path="data_output/03_selected_usgs_h12_all_gages.rds")
write_rds(sel_bmi_gages_csci, path="data_output/03_selected_bmi_h12_all_gages_csci.rds")
write_rds(sel_bmi_gages, path="data_output/03_selected_bmi_h12_all_gages.rds")

# 05. BMI COMIDS: GET NEW/MISSING COMIDS --------------------------

# IF NEEDED

# library(nhdplusTools)
#  
# ## TRANSFORM TO SAME DATUM
# sel_bmi_gages <- st_transform(sel_bmi_gages, crs = 3310) # use CA Teale albs metric
# sel_gages_bmi <- st_transform(sel_gages_bmi, crs=3310)
#  
# # Create dataframe for looking up COMIDS (here use all stations)
# bmi_segs <- st_transform(bmi_stations_distinct, crs=3310) %>%
#   select(StationCode, longitude, latitude) %>%
#   mutate(comid=NA)
# 
# # use nhdtools to get comids
# bmi_all_coms <- bmi_segs[c(1:10),] %>%
#   group_split(StationCode) %>%
#   set_names(., bmi_segs$StationCode[1:10]) %>%
#   map(~discover_nhdplus_id(.x$geometry))
#  
# # flatten into single dataframe instead of list
# bmi_segs_df <-bmi_all_coms %>% flatten_dfc() %>% t() %>%
#   as.data.frame() %>%
#   rename("comid"=V1) %>% rownames_to_column(var = "StationCode")
#  
# # rm COMIDs starting with "V"
# bmi_comids <- bmi_segs_df %>% filter(!grepl("^V", StationCode))
# 
# # save out
# write_rds(bmi_comids, path="data_output/03_bmi_all_stations_comids.rds")
# 
# # clean up
# rm(bmi_all_coms, bmi_segs_df, bmi_segs)

# * Load And Join -----------------------------------------------------------

# load previous stuff run from Step 05 BMI COMIDS
#bmi_comids <- read_rds("data_output/03_bmi_all_stations_comids.rds")

# rejoin with the selected BMI sites in same HUC12 as gage
#sel_bmi_gages <- sel_bmi_gages %>% left_join(., bmi_comids, by="StationCode") # n=2188

# summary(sel_bmi_gages_csci$comid) # no missing

#write_rds(sel_bmi_gages, path="data_output/03_selected_bmi_h12_all_gages.rds")

# 06. GET GAGE COMIDS --------------------------------------------------

# if any comids missing use code below

## TRANSFORM TO UTM datum for flowlines
# sel_bmi_gages <- st_transform(sel_bmi_gages, crs=3310) # use CA Teale albs metric
# sel_gages_bmi <- st_transform(sel_gages_bmi, crs=3310)
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
# save the USGS station COMIDs file:
# write_rds(usgs_segs, path="data_output/02_selected_usgs_gages_comids.rds")

# 07. GET UPSTREAM FLOWLINES FROM GAGE --------------------------------------------------

## TRANSFORM TO UTM datum for flowlines
sel_bmi_gages_csci <- st_transform(sel_bmi_gages_csci, crs=3310) # use CA Teale albs metric
sel_gages_bmi <- st_transform(sel_gages_bmi, crs=3310)

# use a list of comids to make a list to pass to the nhdplusTools function
# important to use NHDV2 COMID here or it will skip/miss out on sites
coms_list <- map(sel_gages_bmi$NHDV2_COMID, ~list(featureSource = "comid", featureID=.x))
coms_list[[200]] # tst check, should list feature source and featureID

# Get Mainstem Segs, needed to do in chunks if needed and rbind
mainstemsUS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                          mode="upstreamMain",
                                          data_source = ""))

# check length (for NAs?)
mainstemsUS %>% 
     purrr::map_lgl(~ length(.x)>1) %>% table()

# transform the sf layer to match mainstems crs (4326)
sel_gages_bmi <- sel_gages_bmi %>% st_transform(4326)

# make a single flat layer
mainstems_flat_us <- mainstemsUS %>%
  set_names(., sel_gages_bmi$gage_id) %>%
  map2(sel_gages_bmi$gage_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_us <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_us, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_us <- mainstems_us %>% 
  mutate(from_gage = "US")

# rm temp files
rm(mainstems_flat_us, mainstemsUS)

# save as both for now
save(mainstems_us, file = "data_output/03_selected_nhd_mainstems_gages_us_ds.rda")


# * ADD ADDITIONAL GAGE SITES? ----------------------------------------------

### ADD ADDITIONAL SITES?
# T11206500, T11208000, T11152050

gages_to_add <- sel_gages_bmi %>% filter(ID %in% c("T11206500", "T11208000", "T11152050", "T11153650"))

# double check?
missing_segs <- gages_to_add %>% split(.$ID) %>%
  map(~discover_nhdplus_id(.x$geometry))

gages_to_list <- map(gages_to_add$NHDV1_COMID, ~list(featureSource = "comid", featureID=.x))

gages_to_list # tst check, should list feature source and featureID

# Get Mainstem Segs, needed to do in chunks if needed and rbind
mainstemsUS_miss <- map(gages_to_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="upstreamMain",
                                             data_source = ""))

# check length (for NAs (==FALSE))
mainstemsUS_miss %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

mainstems_miss_us <- mainstemsUS_miss %>%
  set_names(., gages_to_add$ID) %>%
  map2(gages_to_add$ID, ~mutate(.x, gageID=.y))
mainstems_miss_us <- sf::st_as_sf(data.table::rbindlist(mainstems_miss_us, use.names = TRUE, fill = TRUE))

mapview(mainstems_miss_us)

# 08. GET DOWNSTREAM FLOWLINES FROM GAGE ------------------------------------------------

# get NHD segments downstream of selected USGS gages, 10 km buffer
mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                           mode="downstreamMain",
                                           distance_km = 10,
                                           data_source = ""))

# check length (for NAs?)
mainstemsDS %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

# make a single flat layer
mainstems_flat_ds <- mainstemsDS %>%
  set_names(., sel_gages_bmi$gage_id) %>%
  map2(sel_gages_bmi$gage_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_ds <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_ds, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_ds <- mainstems_ds %>% 
  mutate(from_gage = "DS")

rm(mainstems_flat_ds, mainstemsDS)

# save as both
save(mainstems_us, mainstems_ds, file = "data_output/03_selected_nhd_mainstems_gages_us_ds.rda")

# bind all mainstems
mainstems_all <- rbind(mainstems_us, mainstems_ds)

# 09. SAVE OUT STREAMLINES FOR GAGES ------------------------------------------

save(mainstems_all, file="data_output/03_selected_nhd_mainstems_gages.rda")

# * PREVIEW MAP ----------------------------------------------------------

# # mapview breaks but mapdeck WORKS
# library(mapdeck)
# set_token(Sys.getenv("MAPBOX_TOKEN"))
# 
# mapdeck(
#   style=mapdeck_style("dark")
# ) %>% 
#   add_path(data = mainstems_all, stroke_colour = "from_gage", tooltip="nhdplus_comid", auto_highlight = TRUE) %>% 
#   add_sf(data = sel_gages_bmi, 
#          fill_colour="#00EEEE", tooltip="site_id", 
#          layer_id="USGS Gages", radius=800) %>% 
#   add_sf(data = st_transform(sel_bmi_gages_csci, 4326), fill_colour="#EE7600", radius=500, tooltip="StationCode",
#          layer_id="BMI Sites")

# 10. FILTER TO BMI SITES IN USGS MAINSTEM COMIDS -----------------------------

# reload sites/data here
sel_bmi_gages_csci <- readRDS("data_output/03_selected_bmi_h12_all_gages_csci.rds")
sel_bmi_gages <- readRDS("data_output/03_selected_bmi_h12_all_gages.rds")
sel_gages_bmi <- readRDS("data_output/03_selected_usgs_h12_all_gages.rds")
sel_h12_bmi <- readRDS("data_output/03_selected_h12_all_gages.rds")
load("data_output/03_selected_nhd_mainstems_gages.rda")

# get distinct segs only
mainstems_distinct <- mainstems_all %>% distinct(nhdplus_comid, .keep_all=TRUE)

# all BMI comids that occur in list of mainstem NHD comids: (n=353)
sel_bmi_coms_final <- sel_bmi_gages_csci %>% 
  filter(comid %in% as.integer(mainstems_distinct$nhdplus_comid))

# distinct comid/station/gages combinations:
sel_bmi_coms_final %>% st_drop_geometry() %>% 
  distinct(StationCode, ID) %>% tally() # n=353

# distinct BMI COMIDs
sel_bmi_coms_final %>% st_drop_geometry() %>% distinct(comid) %>% tally() # 220

# distinct GAGES COMIDS
sel_bmi_coms_final %>% st_drop_geometry() %>% distinct(ID) %>% tally() # 156

# 11. FINAL MAP -------------------------------------------------------

# create a final map of selected gages and bmi + huc12 + flowlines

# get all BMI not selected...check why not on map
bmi_not_selected <- sel_bmi_gages_csci %>% filter(!as.character(comid) %in% mainstems_distinct$nhdplus_comid) # should be 199 (loss of 64% of data)

# get all gages selected (n=156)
gages_selected <- sel_gages_bmi %>% 
  filter(gage_id %in% sel_bmi_coms_final$gage_id)

# get the gages not selected (n=51)
gages_not_selected <- sel_gages_bmi %>% 
  filter(!gage_id %in% sel_bmi_coms_final$gage_id)

table(gages_selected$CEFF_type) # ALT=112  REF=44

# this map of all sites selected U/S and D/S
m2 <- mapview(sel_bmi_coms_final, cex=6, col.regions="orange", 
              layer.name="Selected BMI comids") +  
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(gages_selected, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all bmi or gages in same H12 but not selected
  mapview(gages_not_selected, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(bmi_not_selected, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="HUC12")

m2@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save this final map out as:"map_of_final_gages_bmi_stations_all_gages"
#mapshot(m2, url = paste0(here::here(),"/figs/03_map_of_final_bmi_stations_gages_h12s.html"))


# ADD SITES MANUALLY ------------------------------------------------------

# TO ADD: these sites were added because they likely didn't snap to NHD line
## - Arroyo Seco, ID = T11152050, StationCode = 309SET
## - SANTA GERTRUDIS, ID = T11042900, Station Code = 902MCGSxx
## - ARROYO TRABUCO, StationCode = "901M14134"
## - CAJON CK, ID = T11063510, StationCode = 801RB8483
## - CAJON CK, ID = T11063510, StationCode = 801RB8396
## - CAJON CK, ID = T11063510, StationCode = SMCR8_327
## - SAN GABRIEL R = T11085000, StationCode = SGUR010
## - MARBLE F KAWEAH = T11208000 , StationCode = 553WER224

# TO ADD MAYBE??
## - AMARGOSA = T10251300, StationCode = 609PS0053 # this is about 12 km d/s, no inputs btwn
## - TUOLUMNE = T11290000, StationCode = 535CR0910 # this is about 12 km d/s, but no inputs btwn

# TO DROP: these sites were dropped because they are not co-located properly, to far, off channel, etc
## - ID=T11048600, StationCode=801RB8593
## - ID=T11048553. StationCode=801RB8593
## StationCode=412LARSCO, LALT501, 901ATCTCx, 901TCSMP1, 
## ID = T11087020 (7 sites over 15km downstream)
## StationCode = 403FCA038, 403STC019

# do the thing:
sel_bmi_coms_final_v2 <- st_drop_geometry(sel_bmi_coms_final) %>% bind_rows(
  # original final dataset (keep)
  .,
  # update with these records (n=13)
  filter(st_drop_geometry(sel_bmi_gages_csci), 
         StationCode %in% c("309SET", "902MCGSxx", "901M14134","801RB8483", 
                            "801RB8396", "SMCR8_327", "SGUR010",  "553WER224",
                            # the maybes
                            "609PS0053", "535CR0910"))
) %>% 
  # now filter out the stuff we don't want (n=21)
  filter(!StationCode %in% c("801RB8593", "412LARSCO", "LALT501", "901ATCTCx", "901TCSMP1",
                             "403FCA038", "403STC019"), 
         !ID %in% c("T11087020")) 

# re-make the geom
sel_bmi_coms_final_v2 <- st_as_sf(sel_bmi_coms_final_v2, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

### MAP AGAIN

# not selected bmi
bmi_not_selected_v2 <- sel_bmi_gages_csci %>% filter(!as.character(StationCode) %in% sel_bmi_coms_final_v2$StationCode) # n=203

# get all gages selected (n=160)
gages_selected_v2 <- sel_gages_bmi %>% 
  filter(ID %in% sel_bmi_coms_final_v2$ID)

# get the gages not selected (n=47)
gages_not_selected_v2 <- sel_gages_bmi %>% 
  filter(!ID %in% sel_bmi_coms_final_v2$ID)

table(gages_selected_v2$CEFF_type) # ALT=116  REF=44

# this map of all sites selected U/S and D/S
m3 <- mapview(sel_bmi_coms_final_v2, cex=6, col.regions="orange", 
              layer.name="Selected BMI comids") +  
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(gages_selected_v2, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all bmi or gages in same H12 but not selected
  mapview(gages_not_selected_v2, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(bmi_not_selected, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# SAVE OUT ----------------------------------------------------------------

# load the full dataset from 00
load("data_output/00_bmi_cleaned_all.rda") # all data

# pull BMI sites and get list of data, first join with orig full dataset:
bmi_coms_dat <- left_join(sel_bmi_coms_final_v2, bmi_clean %>% select(StationCode, SampleID, MM:problemFinalID), by=c("StationCode", "SampleID")) 

# now look at how many unique samples are avail: n=270 unique samples
bmi_coms_dat %>% st_drop_geometry() %>% distinct(SampleID) %>% tally

# now look at how many unique stations: n=270 stations
bmi_coms_dat %>% st_drop_geometry() %>% distinct(StationCode) %>% tally

# summary
summary(bmi_coms_dat)
hist(bmi_coms_dat$MM) # what months?
# if trim to summer months how many records do we lose?
bmi_coms_dat_trim <- bmi_coms_dat %>% filter(MM>4 & MM<10) 
# lose 56% data if trim Jul - Sep
# lose 45% data if trim Jun - Sep
hist(bmi_coms_dat_trim$MM)

# bmi_may <- bmi_coms_dat %>% filter(MM==5)
# class(bmi_may)
# mapview(bmi_may)
# save out
save(bmi_coms_dat, bmi_coms_dat_trim, sel_bmi_coms_final_v2, file = "data_output/03_selected_final_bmi_stations_dat_all_gages.rda")


# Z-ARCHIVE: Measuring Nearest and Line Lengths --------------------------------------

# this is mostly experimental code snapping points to lines and updating/measuring distances

# get a site, mainstem river and gage
bmi1 <- st_transform(sel_bmi_coms_final[1,], 4326) # the site
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

