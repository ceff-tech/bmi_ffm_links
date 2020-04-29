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
### sel_bmi_coms_final_v2 (just the sites)
### bmi_coms_dat_trim (all data for selected site pairs btwn Jun-Sep)
load("data_output/03_selected_final_bmi_stations_dat_all_gages.rda") 

# POR joined data w huc regions
#load("data_output/05_selected_bmi_csci_por_trim_w_huc_region.rda")
#load("data_output/05_selected_bmi_csci_por_w_huc_region.rda")

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

# make gage_id as character for join, read date and filter to TRIM Months (May-Sep)
sel_bmi_coms_final_v2 <- sel_bmi_coms_final_v2 %>% 
  mutate(gage_id_c = gsub("^T", "", ID))

sel_bmi_coms_final_trimmed <- sel_bmi_coms_final_v2 %>% 
  mutate(gage_id_c = gsub("^T", "", ID)) %>% 
  separate(SampleID, into=c("site", "sampledate"), sep = "_", remove = FALSE) %>% 
  mutate(sampledate = lubridate::mdy(sampledate)) %>% 
  mutate(sampledate = if_else(is.na(sampledate), lubridate::mdy("06282009"), sampledate)) %>% 
  select(-site) %>% 
  filter(lubridate::month(sampledate)>4, lubridate::month(sampledate)<10)

# check the layers above match: 
bmi_coms_dat_trim %>% st_drop_geometry() %>% distinct(SampleID, ID) %>% dim() # trimmed data (n=300):
bmi_coms_dat %>% st_drop_geometry() %>% distinct(SampleID, ID) %>% dim() # this should be 349

# Make ANNUAL Dataset -----------------------------------------------------

#g_all_ffc %>% group_by(gage_id, Year) %>% distinct() %>% dim()

# get raw functional flow metrics (for every year)
ffm <- g_all_ffc %>% mutate(Year=as.integer(Year)) %>% 
  distinct() # filter out duplication

# make it long not wide for joins:
ffm <- pivot_longer(ffm, cols= c(DS_Dur_WS:Peak_Fre_5), names_to = "ffm_metric", values_to = "ffm_value") %>% 
  # drop nas
  filter(!is.na(ffm_value))

## MAKE UNTRIMMED DATA SET (ALL SAMPLE BMI MONTHS)
# untrimmed = 349
bmi_sampleid <- bmi_coms_dat %>% st_drop_geometry() %>% 
  mutate(gage_id_c = gsub("^T", "", ID)) %>% 
  dplyr::distinct(SampleID, ID, .keep_all = TRUE)

# join for annual data by GAGE ID, and BMI Sample YEAR = USGS Year
bmi_csci_ffm_ann <- left_join(bmi_sampleid, ffm, by=c("gage_id_c"="gage_id", "YYYY"="Year"))


# Make LAGGED Dataset -----------------------------------------------------

# years -1  -2
lag_yrs_1 <- unique(bmi_sampleid$YYYY) - 1
lag_yrs_2 <- unique(bmi_sampleid$YYYY) - 2

# make lag data
ffm_lag1 <- ffm %>% filter(Year %in% lag_yrs_1) %>% 
  mutate(year_flow = Year-1) # add for labeling purposes
ffm_lag2 <- ffm %>% filter(Year %in% lag_yrs_2) %>% 
  mutate(year_flow = Year-2) # add for labeling purposes

# rejoin
bmi_csci_ffm_lag1 <- left_join(bmi_sampleid, ffm_lag1, by=c("gage_id_c"="gage_id", "YYYY"="Year"))
bmi_csci_ffm_lag2 <- left_join(bmi_sampleid, ffm_lag2, by=c("gage_id_c"="gage_id", "YYYY"="Year"))

# make just sampleID dataset (trimmed = 300)
bmi_sampleid_trimmed <- bmi_coms_dat_trim %>% st_drop_geometry() %>% 
  mutate(gage_id_c = gsub("^T", "", ID)) %>% 
  dplyr::distinct(SampleID, ID, .keep_all = TRUE)

# join for annual data by GAGE ID, and BMI Sample YEAR = USGS Year
bmi_csci_ffm_ann_trimmed <- left_join(bmi_sampleid_trimmed, ffm, by=c("gage_id_c"="gage_id", "YYYY"="Year"))

# years -1  -2
lag_yrs_1_trim <- unique(bmi_sampleid_trimmed$YYYY) - 1
lag_yrs_2_trim <- unique(bmi_sampleid_trimmed$YYYY) - 2

# make lag data
ffm_lag1_trimmed <- ffm %>% filter(Year %in% lag_yrs_1_trim) %>% 
  mutate(year_flow = Year-1) # add for labeling purposes
ffm_lag2_trimmed <- ffm %>% filter(Year %in% lag_yrs_2_trim) %>% 
  mutate(year_flow = Year-2) # add for labeling purposes

# rejoin
bmi_csci_ffm_lag1_trim <- left_join(bmi_sampleid_trimmed, ffm_lag1_trimmed, by=c("gage_id_c"="gage_id", "YYYY"="Year"))
bmi_csci_ffm_lag2_trim <- left_join(bmi_sampleid_trimmed, ffm_lag2_trimmed, by=c("gage_id_c"="gage_id", "YYYY"="Year"))



# SAVE OUT ----------------------------------------------------------------


save(bmi_csci_ffm_ann, file="data_output/06_selected_bmi_csci_ffm_ann.rda")
save(bmi_csci_ffm_ann_trimmed, file="data_output/06_selected_bmi_csci_ffm_ann_trim.rda")
save(bmi_csci_ffm_lag1, file="data_output/06_selected_bmi_csci_ffm_lag1.rda")
save(bmi_csci_ffm_lag1_trim, file="data_output/06_selected_bmi_csci_ffm_lag1_trim.rda")
save(bmi_csci_ffm_lag2, file="data_output/06_selected_bmi_csci_ffm_lag2.rda")
save(bmi_csci_ffm_lag2_trim, file="data_output/06_selected_bmi_csci_ffm_lag2_trim.rda")


# zz: Calc Bug Metrics --------------------------------------------------------

# only need to do this once, don't rerun, takes a fair bit of time
# first filter to years of interest
library(CSCI)
library(BMIMetrics)

bmi_filt <- bmi_coms_dat %>% st_drop_geometry() #%>% 
  #filter(YYYY %in% c(1993:2017))

library(purrr)

bugs_split <- bmi_filt %>%
  split(.$SampleID) %>%
  map(~BMI(.x)) # make into BMI object

bugs_samp <- bugs_split %>%
  map(~sample(.x)) # subsample to 500 individuals and aggregate

# aggregate across these splits (this takes awhile)
bugs_agg <- bugs_samp %>%
  map(~aggregate(.x))

# Calculate metrics at SAFIT Level 1
bug_metrics <- bugs_agg %>%
  map(~BMIall(.x, effort=1))

# make clean station set:
bmi_sampleids <- bmi_filt %>% distinct(SampleID, ID, .keep_all = T) %>%
  select(StationCode:HUC_12, ID, SampleID:sampledate)

# flatten and rejoin with station data:
bmi_metrics_df <- bug_metrics %>%
  do.call(what = rbind) %>%
  remove_rownames() %>%
  inner_join(., bmi_sampleids, by="SampleID")

# clean workspace, rm old bits
rm(bugs_agg, bugs_samp, bugs_split, bug_metrics, bmi_filt)

# all IDs accounted for? (should be equal to total number of rows in dataframe)
dim(bmi_metrics_df[!is.na(bmi_metrics_df$ID),])[1]

# SAVE IT
save(bmi_metrics_df, file="data_output/06_selected_bmi_csci_and_bug_metrics.rda")

