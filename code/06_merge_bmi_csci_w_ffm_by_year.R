## 06 Calc BMI Metrics and Merge with Flow datasets
## R. Peek
# Calc bug metrics with CSCI and merge with flow metrics for respective datasets (annual, lag1, lag2, POR)

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(tidyverse)
library(tidylog)

# Load Data ---------------------------------------------------------------

# bmi data:
### bmi_coms_dat (all data for selected site pairs), 
### bmi_coms_final (just coms and id)
### bmi_coms_dat_trim (all data for selected site pairs btwn Jun-Sep)
load("data_output/03_selected_final_bmi_stations_dat_all_gages.rda") 

# FISH REGIONS
ca_sp_regions <- read_sf("data/spatial/umbrella_sp_regions.shp", as_tibble = T)

# nhd streamlines
load("data_output/03_selected_nhd_mainstems_gages.rda") # mainstems_all

# get all functional flow metric data (percentiles, alt status, ffmetrics)
load("data_output/02_usgs_all_ffm_data.rda")

# Set Basemaps ------------------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Make BMI POR FF Dataset -----------------------------------------------

# make gage_id as character for join:
sel_bmi_coms_final_v2 <- sel_bmi_coms_final_v2 %>% 
  mutate(gage_id_c = gsub("^T", "", ID))

# join together selected csci data with ffm alteration status data
bmi_csci_por <-  inner_join(sel_bmi_coms_final_v2, g_all_alt,
                            #by=c("comid")) #%>% # n=2688
                            #by=c("comid", "gage_id_c"="gage_id")) # %>% # n=1550
                            # since only want observed data at USGS gage:
                            by=c("gage_id_c"="gage_id")) %>%   # n=7719
  distinct(SampleID, metric, gage_id, .keep_all=TRUE) %>% 
  rename(comid_bmi = comid.x, comid_ffc = comid.y) # n=7337

# see how many distinct sites
length(unique(bmi_csci_por$gage_id_c)) #Gages (n=154)
length(unique(bmi_csci_por$StationCode)) # BMI Stations (n=267)

# how many of each gage type
bmi_csci_por %>% st_drop_geometry() %>% 
  dplyr::distinct(ID, .keep_all=TRUE) %>% 
  group_by(CEFF_type) %>%  tally() # ALT = 116, REF = 38

# and originally? : so we lost 6 ref sites :(
sel_bmi_coms_final_v2 %>% st_drop_geometry() %>% 
  dplyr::distinct(ID, .keep_all=TRUE) %>% 
  group_by(CEFF_type) %>%  tally() # ALT = 116, REF = 44

# Lagged Dataset ----------------------------------------------------------


# make a lagged dataset, need to look for WYs + 1 equal to the actual bmi year:

# this took way too long to figure out
## LAG 1
bmi_flow_metrics_lag1 <- bmi_flow_metrics_all %>% 
  dplyr::filter(YYYY == WYs+1)

## LAG 2
bmi_flow_metrics_lag2 <- bmi_flow_metrics_all %>% 
  dplyr::filter(YYYY == WYs+2)

# make POR dataset
bmi_flow_metrics_por <- bmi_flow_metrics_all %>% 
  dplyr::filter(WYs==1900)

# Now add CSCI: but need to regen sampleID for CSCI data (station_YMD_samplemethod_replicate)
bmi_csci_flow_por <- bmi_csci_flow_por %>% 
  mutate(SampleID=paste0(StationCode, "_", year(sampledate), sprintf("%02d", month(sampledate)), sprintf("%02d", day(sampledate)), "_", collectionmethodcode, "_", fieldreplicate)) %>% st_drop_geometry()

# annual and lagged data
bmi_csci_flow_yrs <- bmi_csci_flow_yrs %>% 
  mutate(SampleID=paste0(StationCode, "_", year(sampledate), sprintf("%02d", month(sampledate)), sprintf("%02d", day(sampledate)), "_", collectionmethodcode, "_", fieldreplicate)) 

# make a distinct SampleID list of csci
csci_only_por <- bmi_csci_flow_por %>% select(SampleID, csci, csci_percentile, mmi, mmi_percentile) %>% 
  distinct(SampleID, .keep_all = T)

# distinct for csci_only_yrs
csci_only_yrs <- bmi_csci_flow_yrs %>% select(SampleID, csci, csci_percentile, mmi, mmi_percentile) %>% 
  distinct(SampleID, .keep_all = T)

# now join
bmi_flow_metrics_por_csci <- left_join(bmi_flow_metrics_por, csci_only_por)
bmi_flow_metrics_ann_csci <- left_join(bmi_flow_metrics_annual, csci_only_yrs)
bmi_flow_metrics_lag1_csci <- left_join(bmi_flow_metrics_lag1, csci_only_yrs)
bmi_flow_metrics_lag2_csci <- left_join(bmi_flow_metrics_lag2, csci_only_yrs)

save(bmi_flow_metrics_por_csci, file="data_output/06_selected_bmi_flow_metrics_w_csci_POR.rda")
save(bmi_flow_metrics_ann_csci, file="data_output/06_selected_bmi_flow_metrics_w_csci_ANN.rda")
save(bmi_flow_metrics_lag1_csci, file="data_output/06_selected_bmi_flow_metrics_w_csci_LAG1.rda")
save(bmi_flow_metrics_lag2_csci, file="data_output/06_selected_bmi_flow_metrics_w_csci_LAG2.rda")


# zz: Calc Bug Metrics --------------------------------------------------------

# only need to do this once, don't rerun, takes a fair bit of time
# first filter to years of interest

bmi_filt <- bmi_dat %>% st_drop_geometry() %>% 
  filter(YYYY %in% c(1993:2016))

library(purrr)

bugs_split <- bmi_filt %>%
  split(.$SampleID) %>%
  map(~BMI(.x)) # make into BMI object

bugs_samp <- bugs_split %>%
  map(~sample(.x)) # subsample to 500 individuals and aggregate

bugs_agg <- bugs_samp %>%
  map(~aggregate(.x))

# Calculate metrics at SAFIT Level 1
bug_metrics <- bugs_agg %>%
  map(~BMIall(.x, effort=1))

# make clean station set:
bmi_sampleids <- bmi_filt %>% distinct(SampleID, .keep_all = T) %>%
  select(1:4, 8:24)

# flatten and rejoin with station data:
bmi_metrics_df <- bug_metrics %>%
  do.call(what = rbind) %>%
  remove_rownames() %>%
  inner_join(., bmi_sampleids, by="SampleID")

# clean workspace, rm old bits
rm(bugs_agg, bugs_samp, bugs_split, bug_metrics, bmi_filt)

# fix the names and cols, add USGS ID
bmi_metrics_df <- bmi_metrics_df %>% 
  select(-c(latitude.x, longitude.x, LONGITUDE, latitude.y, longitude.y)) %>% 
  # join the USGS station ID back in:
  left_join(., bmi_coms[,c(1:3,7)], by="StationCode") %>% 
  select(-geometry)

# all IDs accounted for?
dim(bmi_metrics_df[!is.na(bmi_metrics_df$ID),])[1]

# SAVE IT
#save(bmi_metrics_df, file="data_output/06_selected_bmi_metrics_at_gage_sites.rda")

