# 02b Spatially Linking BMI & selected USGS Gages by NHD Flowlines
## R. Peek 2020

## Spatially link the BMI station data with the USGS FFC gages that occur in same flowline and h12

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
library(glue)
library(here)
library(lubridate)
library(beepr) # to tell us when stuff is done

#devtools::install_github("USGS-R/nhdplusTools")
library(nhdplusTools)

# 01. Load Data ---------------------------------------------------------------

# selected HUC12s
sel_h12_bmi <- read_rds("data_output/02a_sel_h12_w_bmi_csci.rds")
sel_h12_gages <- read_rds("data_output/02a_sel_h12_w_ffc_gages.rds")

# selected bmi and gages
sel_gages_bmi <- read_rds("data_output/02a_sel_ffc_gages_by_h12.rds")
sel_bmi_gages_csci <- read_rds("data_output/02a_sel_bmi_stations_csci_by_h12.rds")
sel_bmi_station_gages_h12 <- read_rds("data_output/02a_sel_bmi_stations_h12.rds")

# BMI COMIDs
bmi_comids <- readRDS("data_output/02b_bmi_stations_comids_revised.rds") %>% 
  st_drop_geometry() %>% select(StationCode, starts_with("COMID"))

# mainstem flowlines
load("data_output/02b_sel_gage_mainstems_all.rda")

# 02. TIDY UP -------------------------------------------------------------

# MERGE COMIDS with BMIs
sel_bmi_gages_csci <- left_join(sel_bmi_gages_csci, bmi_comids, by="StationCode")
sel_bmi_station_gages_h12 <- left_join(sel_bmi_station_gages_h12, bmi_comids, by="StationCode")

# get distinct segs that are DS only (either DD or DS)
mainstems_distinct <- mainstems_all %>% 
  filter(from_gage %in% c("DS", "DD")) %>% 
  distinct(nhdplus_comid, .keep_all=TRUE)

# quick view
mapview(mainstems_distinct, color="darkblue") + mapview(sel_gages_bmi, col.region="skyblue") +
  mapview(sel_bmi_station_gages_h12, col.regions="orange", cex=2)

# 03. FILTER TO MAINSTEM COMIDS DS of GAGE -------------------------------

# select all BMI COMIDs that occur in downstream mainstem NHD comids: (n=889)
sel_bmi_coms_final <- 
  sel_bmi_gages_csci %>% # has CSCI for all samples
  #sel_bmi_station_gages_h12 %>%  # doesn't have CSCI for all samples
  filter(COMID %in% mainstems_distinct$nhdplus_comid)


## 03b. DESCRIBE SITES SELECTED -------------------------------------------------

# distinct comid/station/gages combinations:
sel_bmi_coms_final %>% st_drop_geometry() %>% 
  distinct(StationCode, site_id) %>% tally() 
# if using sel_bmi_station_gages_h12: n=839 (but only half have CSCI?)
# if using sel_bmi_gages_csci: n=478 (but all have CSCI)

# distinct BMI COMIDs
sel_bmi_coms_final %>% st_drop_geometry() %>% distinct(COMID) %>% tally() # 296 (n=210 w CSCI)

# distinct GAGES COMIDS
sel_bmi_coms_final %>% st_drop_geometry() %>% distinct(site_id) %>% tally() # 277 (n=228 w CSCI)

# get all BMI not selected
bmi_not_selected <- sel_bmi_gages_csci %>% filter(!COMID %in% mainstems_distinct$nhdplus_comid) # n=905 (loss of 50% of data)

# get all gages selected (n=228)
gages_selected <- sel_gages_bmi %>% 
  filter(site_id %in% sel_bmi_coms_final$site_id)

# get the gages not selected (n=187)
gages_not_selected <- sel_gages_bmi %>% 
  filter(!site_id %in% sel_bmi_coms_final$site_id)

# get the hucs selected (n=139)
hucs_selected <- sel_h12_bmi %>% 
  filter(HUC_12 %in% sel_bmi_coms_final$HUC_12)

# get the hucs not selected (n=112)
hucs_not_selected <- sel_h12_bmi %>% 
  filter(!HUC_12 %in% sel_bmi_coms_final$HUC_12)

# 04. FINAL MAP -------------------------------------------------------

# set mapview so we can save to html
mapviewOptions(fgb = FALSE)

# this map of all sites selected U/S and D/S
m1 <- mapview(sel_bmi_coms_final, cex=6, col.regions="orange", 
              layer.name="Selected BMI comids") +  
  mapview(mainstems_all %>% filter(from_gage=="UM"), color="forestgreen", cex=3, 
          layer.name="NHD Flowlines US") +
  mapview(mainstems_distinct, color="steelblue", cex=3, 
          layer.name="NHD Flowlines DS") +
  mapview(gages_selected, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all bmi or gages in same H12 but not selected
  mapview(gages_not_selected, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(bmi_not_selected, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other BMI Sites in H12") + 
  mapview(hucs_selected, col.regions="orange3", alpha.region=0.1, 
          color="orange", legend=F, layer.name="Selected HUC12") +
  mapview(hucs_not_selected, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="Other HUC12")

m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save out
#mapshot(m1, url = paste0(here::here(),"/figs/02c_map_of_selected_bmi_gage_h12s_DS.html"))


# 05. ADD/DROP SITES MANUALLY ---------------------------------------------------

## UPDATED 2020-12-14
# visually inspect the map above, and add/revise sites

gages_to_drop <- c("11298000","11297500","11316600",
                   "10344500", "11408870", "11408880",
                   "11389800", "11403200", "11403200",
                   "11404300")
gages_to_add <- c("11044800", # De Luz Ck: StationCode: 902S00117 or 902SMROB8
                  "11063510", # Cajon Ck: BMI: SMCR8_327, 801RB8396, 801RB8483
                  "11123000", # Santa Ynez (site is 12km d/s): 314SYP
                  "11152300", # Salinas R NR Chualar: 309SAC
                  "11143200", # Carmel River:307CMU
                  "11159200", # Corralitos Ck: 305CAW057, 305SCC
                  "11481000" # Mad Ck, bmi site just upstream: 109PS0162
                  )
csci_to_drop <- c("801RB8593", "540SJR001")
csci_to_add <- c(
  "905SDBDN9", "902S00117", "902SMROB8", "801SAR351",
  "SMCR8_327", "801RB8396","314SYP","603CE0782",
  "305PS0034", "113GAR084", "113GAR109", "109PS0162")

# pull out csci sites to add first
sel_bmi_coms_add_csci <- sel_bmi_gages_csci %>%
  filter(StationCode %in% csci_to_add) #%>% st_drop_geometry()

# now add gages that are missing:
sel_gages_add <- sel_gages_bmi %>% filter(site_id %in% gages_to_add)
sel_gages_add2 <- sel_bmi_gages_csci %>% st_drop_geometry() %>% 
  filter(site_id %in% gages_to_add) %>%
  mutate(StationCode = case_when(
    StationCode=="305CRCBVR" ~ "305CAW057",
    TRUE ~ StationCode
  )) %>% 
  # copy one extra row to add station
  bind_rows(., sel_bmi_gages_csci %>% filter(StationCode=="305CRCBVR")) %>% 
  # fix and replace StationCodes to corrected pair
  mutate(StationCode = case_when(
    StationCode=="305CRCBVR" ~ "305SCC",
    TRUE ~ StationCode
  ))
  # need to replace 305CRCBVR with 305CAW057, 305SCC
  # need to add SMCR8_327, 801RB8396 to Cajon
  filter(!StationCode %in% c("109WE1051", "305CRCBVR","902DLCDLM", "902WE0888"))

# bind back with final list
sel_bmi_coms_final_v2 <- 
  bind_rows(sel_bmi_coms_final_v2, sel_gages_add) %>% 
  # filter out the stuff we don't want
  filter(!StationCode %in% csci_to_drop, 
         !site_id %in% gages_to_drop) 

# re-make the geom using bmi stations
sel_bmi_coms_final_v2 <- st_as_sf(sel_bmi_coms_final_v2, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

### MAP AGAIN

# not selected bmi
bmi_not_selected_v2 <- sel_bmi_gages_csci %>% filter(!StationCode %in% sel_bmi_coms_final_v2$StationCode) # n=303

# get all gages selected (n=326)
gages_selected_v2 <- sel_gages_bmi %>% 
  filter(site_id %in% sel_bmi_coms_final_v2$site_id)

# get the gages not selected (n=89)
gages_not_selected_v2 <- sel_gages_bmi %>% 
  filter(!site_id %in% sel_bmi_coms_final_v2$site_id)

# get hucs selected (n=205)
hucs_selected_v2 <- sel_h12_bmi %>% 
  filter(HUC_12 %in% sel_bmi_coms_final_v2$HUC_12)

# this map of all sites selected U/S and D/S
m3 <- mapview(sel_bmi_coms_final_v2, cex=6, col.regions="orange", 
              layer.name="Selected BMI comids") +  
  mapview(mainstems_all, zcol="from_gage", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(gages_selected_v2, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all bmi or gages in same H12 but not selected
  mapview(gages_not_selected_v2, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(bmi_not_selected, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other BMI Sites in H12") + 
  mapview(hucs_selected_v2, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# SAVE OUT ----------------------------------------------------------------

# load the full dataset from 00
load("data_output/00_bmi_cleaned_all.rda") # all data

# pull BMI sites and get list of data, first join with orig full dataset:
bmi_coms_dat <- left_join(sel_bmi_coms_final_v2, st_drop_geometry(bmi_clean) %>% select(StationCode, SampleID, MM:problemFinalID), by=c("StationCode", "SampleID")) 

# now look at how many unique CSCI samples are avail: n=437 unique samples
bmi_coms_dat %>% st_drop_geometry() %>% distinct(SampleID) %>% tally

# now look at how many unique stations: n=270 stations
bmi_coms_dat %>% st_drop_geometry() %>% distinct(StationCode) %>% tally

# now look at how many unique gageID: n=326
bmi_coms_dat %>% st_drop_geometry() %>% distinct(site_id, .keep_all=TRUE) %>% tally()

# see how many are ref vs alt
ref_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_ref_gages_list.csv")
alt_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_alt_gages_list.csv")

# add CEFF alt type
bmi_coms_dat <- bmi_coms_dat %>% 
  mutate(CEFF_type = case_when(
    bmi_coms_dat$site_id %in% ref_gages$site_id ~ "REF",
    bmi_coms_dat$site_id %in% alt_gages$site_id ~ "ALT"
  ))

# look at gages by type
bmi_coms_dat %>% 
  st_drop_geometry() %>% distinct(site_id, .keep_all=TRUE) %>% group_by(CEFF_type) %>% tally()

#ALT         245
#REF          81

# summary
summary(bmi_coms_dat)
hist(bmi_coms_dat$MM) # what months?

# if trim to summer months how many records do we lose? (14% of data)
bmi_coms_dat_trim <- bmi_coms_dat %>% filter(MM>4 & MM<10) 
hist(bmi_coms_dat_trim$MM)

# if trimming we lose a few gages: 
bmi_coms_dat_trim %>% st_drop_geometry() %>% distinct(site_id, .keep_all=TRUE) %>% count(CEFF_type)

#ALT 221
#REF 78

# save out
save(bmi_coms_dat, bmi_coms_dat_trim, sel_bmi_coms_final_v2, file = "data_output/03_selected_final_bmi_stations_dat_all_gages.rda")

