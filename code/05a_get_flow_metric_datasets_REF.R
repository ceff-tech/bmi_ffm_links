# 05 Merge BMI Data with Flow Data by year
## R. Peek
## Link the BMI data by flow data with a lag, annual, and POR

## DATA OUT:
### bmi_coms, file="data_output/05_selected_bmi_stations_w_comids.rda" (the comids for selected BMI stations n=224)
### bmi_csci_flow_por, file="data_output/05_selected_bmi_stations_w_csci_flow_por.rda" (period of record flow associated with available CSCI data, n=194)
### bmi_csci_flow_yrs, file="data_output/05_selected_bmi_stations_w_csci_flow_years.rda" (flow data with avail csci scores, 1,2yr lags n=432)
### bmi_final_dat, file="data_output/05_selected_bmi_cleaned_w_data.rda" (cleaned data w site status, n=27567)
### flow_por_wide, flow_por, file="data_output/05_selected_usgs_flow_metrics_POR.rda" (ref gage data for period of record, n=223)
### mainstems, file="data_output/05_mainstems_us_ds_selected_gages.rda" (mainstem NHD flowlines, us and ds, n=1581)

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(tidyverse)
library(tidylog)

# Load Data ---------------------------------------------------------------

load("data_output/00_bmi_cleaned_all.rda") # bmi_clean
load("data_output/01_bmi_cleaned_stations_w_site_status.rda") # bmi_clean_stations_ss (site status)
load("data_output/02_selected_bmi_and_gages_same_h12.rda") # sel_bmi_gages, sel_gages_bmi
load("data_output/02_selected_nhd_flowlines_mainstems.rda") # mainstems_us, mainstems_ds
load("data_output/02_selected_h12_contain_bmi_gage.rda") # all h12s w bmi and gage: sel_h12_bmi
load("data_output/02_final_bmi_stations_dat_reference.rda") # bmi_coms_dat (all data for selected), bmi_coms_final (just coms and id)
bmi_coms <- read_rds("data_output/02_bmi_all_stations_comids.rds") # just bmi_coms, comids for all BMI sites

# re order cols
bmi_coms_final <- bmi_coms_final %>% 
  select(StationCode, longitude, latitude, HUC_12, 
         h12_area_sqkm, ID:comid, geometry) %>% 
  distinct(StationCode, ID, .keep_all=TRUE) # n=157

# make a mainstems all file
mainstems_all <- rbind(mainstems_us, mainstems_ds)
rm(mainstems_ds, mainstems_us)

# load mapview bases
# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# * Get BMI Site Status by Comid ----------------------------------------

# join with status:
bmi_coms_final <- bmi_coms_final %>% 
  left_join(., bmi_clean_stations_ss[, c(1:2)], by="StationCode")

# Join BMI Sites with BMI Data --------------------------------------------

# pull BMI sites and get list of data, first join with orig full dataset:
bmi_final_dat <- bmi_coms_dat %>%
  dplyr::select(-c(benthiccollectioncomments,
                   percentsamplecounted:gridsvolumeanalyzed,
                   discardedorganismcount, benthiclabeffortcomments,
                   resqualcode:personnelcode_labeffort,
                   samplecomments, effortqacode)) %>%
  st_drop_geometry() # drop sf class

# now look at how many unique samples are avail: n=299 unique samples
bmi_final_dat %>%  distinct(SampleID, ID) %>% tally

# now look at how many unique stations: n=139 stations
bmi_final_dat %>% distinct(StationCode) %>% tally

# now look at how many unique USGS gages: n=49 stations
bmi_final_dat %>% distinct(ID) %>% tally

# Get/Join with Reference Flow Data --------------------------------------

# get reference flow data from FFC
load("data/ref_gage_annFlow_stats_long.rda")

# need to add "T" to the gageID
flow_long <- dat_long %>% mutate(ID=paste0("T", gage)) %>% 
  # filter out NA's?
  filter(!is.na(data)) %>% 
  ungroup() %>% 
  select(ID, gage, stream_class, stat:YrRange) %>% 
  filter(!stat=="Avg", !stat=="CV", !stat=="Std") # drop these if need be

# rm old dataset
rm(dat_long)

# * Summarize Metrics for Period of Record ---------------------------------

# set ID vars for flow metrics
flow_idvars <- flow_long %>% group_by(ID, stat) %>% 
  distinct(ID, stat, maxYr, minYr)

# now avg for PERIOD OF RECORD for CSCI comparison
flow_por <- flow_long %>% 
  select(-YrRange, -gage, -year, -stream_class) %>% 
  group_by(ID, stat) %>% 
  summarize_at(vars(data), mean, na.rm=T) %>% 
  # rejoin yr ranges
  left_join(., flow_idvars)

# unique metrics? (should be 31 not including annual/cv/std)
length(unique(flow_por$stat))

# make wide for join
flow_por_wide <- flow_por %>% 
  pivot_wider(names_from=stat, values_from=data)
#values_fill=list(data=0))
# can fill missing values with zero if preferred: values_fill=list(data=0)

# check number unique gages:
flow_por_wide %>% distinct(ID) %>% dim # should be 223

# * Set 1 & 2 Year Lags for BMI Sites ---------------------------------------

# need to pull flow data from:
### 2 years prior, 
### 1 year prior, 
### Same year as BMI collection date

# make vector of years and BMI_ids
bmi_yrs <- bmi_final_dat %>% group_by(SampleID) %>% pull(YYYY) %>% unique()
(bmi_yrs_2 <- bmi_yrs - 2) # set lag 2
(bmi_yrs_1 <- bmi_yrs - 1) # set lag 1

# now combine and order:
bmi_years <- combine(bmi_yrs, bmi_yrs_1, bmi_yrs_2) %>% sort() %>% unique() # 25 total years to match with flow record
bmi_years # 1993:2017

# rm old files
rm(bmi_yrs, bmi_yrs_1, bmi_yrs_2)

# * Filter Reference Flow Data to Years of Interest -----------------------

# now match with flow data (only goes through 2016)
flow_by_years_bmi <- flow_long %>% 
  filter(year %in% bmi_years) # 1993:2017

# make wide
flow_by_years_bmi_wide <- flow_by_years_bmi %>% 
  pivot_wider(names_from=stat, values_from=data)

flow_by_years_bmi_wide %>% distinct(ID) %>% dim # should be 106 gages match same years

# save flow data out for annual match
save(flow_by_years_bmi, flow_by_years_bmi_wide, file="data_output/05a_selected_ref_flow_by_years_of_bmi.rda")

# Get CSCI Data -----------------------------------------------------------

# see what data exist against CSCI scores currently avail (from Raffi)
csci1 <- read_csv("data/csci_core.csv") %>% 
  mutate(sampledate=as.Date(sampledate)) %>% 
  select(sampleid, stationcode, sampledate, collectionmethodcode, fieldreplicate, count, csci, csci_percentile)
csci2 <- read_csv("data/csci_core_v2.csv") %>% 
  rename(stationcode=StationCode) %>%
  mutate(sampledate=mdy(sampledate)) %>% 
  select(sampleid, stationcode, sampledate, collectionmethodcode, fieldreplicate, count, csci, csci_percentile)

# join together
csci<-bind_rows(csci1, csci2) %>% 
  mutate(sampleyear=year(sampledate))

# rm old files
rm(csci1, csci2)

# now have n=4034 unique samples
csci %>%  distinct(sampleid) %>% tally()

# match against existing sites irrespective of sampleid
bmi_csci <- inner_join(bmi_coms_final, csci, by=c("StationCode"="stationcode")) %>% # n=296
  distinct(sampleid, ID, .keep_all = T)

# bmi_csci <- anti_join(bmi_coms_final, csci, by=c("StationCode"="stationcode"))
# write_csv(bmi_csci, path = "data/ref_bmi_sites_missing_csci_data.csv")

# how many unique matches?
length(unique(bmi_coms_final$StationCode)) 
# 139 stations (but some w mult gage matches)
length(unique(bmi_csci$StationCode)) # only 133 matches, so missing 6

# view Site Status
bmi_csci %>% st_drop_geometry() %>% 
  group_by(SiteStatus) %>% tally()

# drop site Status of "stressed" as these may skew correlations with flow metrics
bmi_csci <- bmi_csci %>% filter(SiteStatus %in% c("Intermediate", "Reference", NA)) # should be 252

# JOIN with Flow Period of Record (POR) ---------------------------------

bmi_csci_flow_por <- inner_join(bmi_csci, flow_por_wide, by="ID")

# look at distinct
bmi_csci_flow_por %>% st_drop_geometry() %>% distinct(sampleid, ID) %>% tally()

# filter to BMI sites that have data in the flow time range, but doesn't really matter for POR?
bmi_csci_flow_por_overlap <- bmi_csci_flow_por %>%
  filter(sampleyear >= minYr & sampleyear<= maxYr)

length(unique(bmi_csci_flow_por_overlap$StationCode)) # 100 stations
length(unique(bmi_csci_flow_por_overlap$ID)) # 38 gages

# JOIN with Flow by BMI Lag Years ----------------------------------------

bmi_csci_flow_yrs <- inner_join(bmi_csci, flow_by_years_bmi_wide, by=c("ID")) %>% 
  # filter to same year as BMI + 2 yr lag
  filter(sampleyear == year | sampleyear == year+1 | sampleyear==year+2)

# double check, should have 3 entries per sampleid
# bmi_csci_flow_yrs %>% select(StationCode, sampleid, sampleyear, year) %>% View()

# filter to sites that have data in the flow time range?
bmi_csci_flow_yrs %>% st_drop_geometry() %>% distinct(StationCode) %>% tally() # 101 BMI sites
bmi_csci_flow_yrs %>% st_drop_geometry() %>% distinct(sampleid, year) %>% tally() # 522 separate data points
bmi_csci_flow_yrs %>% st_drop_geometry() %>% distinct(ID) %>% tally() # 38 gages

# how many NA's across records?
bmi_csci_flow_yrs %>% drop_na(SP_Tim:Peak_Mag_20) %>% dim() # end up with 145 records, would drop 76% of data

# EXPORT Cleaned/Joined Flow/BMI Data -----------------------------------------------------

save(bmi_csci_flow_por, file="data_output/05a_selected_ref_bmi_w_csci_flow_por.rda")
save(bmi_csci_flow_yrs, file="data_output/05a_selected_ref_bmi_w_csci_flow_yrs.rda")
save(bmi_final_dat, file="data_output/05a_selected_ref_bmi_final_dat.rda")
save(flow_por_wide, flow_por, file="data_output/05a_all_ref_usgs_flow_metrics_por.rda")

