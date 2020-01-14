## 06 Calc BMI Metrics and Merge with Flow datasets
## R. Peek
# Calc bug metrics with CSCI and merge with flow metrics for respective datasets (annual, lag1, lag2, POR)

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(CSCI)
library(BMIMetrics)
library(lubridate)

# Data --------------------------------------------------------------------


load("data_output/05b_selected_bmi_stations_w_comids.rda")
load("data_output/05b_mainstems_us_ds_selected_gages.rda")
load("data_output/05b_selected_bmi_stations_w_csci_flow_por.rda")
load("data_output/05b_selected_bmi_stations_w_csci_flow_years.rda")
load("data_output/05b_selected_bmi_cleaned_w_data.rda")
load("data_output/05b_selected_usgs_flow_metrics_POR.rda")
load("data_output/05b_selected_flow_by_years_of_bmi.rda")

# see first code chunk to generate this data:
load("data_output/06_selected_bmi_metrics_at_gage_sites.rda")

# make non-sf: 
bmi_csci_flow_yrs <- bmi_csci_flow_yrs %>% st_drop_geometry() %>% as.data.frame

# Calc Bug Metrics --------------------------------------------------------

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
save(bmi_metrics_df, file="data_output/06_selected_bmi_metrics_at_gage_sites.rda")

# Join Bug Metrics with Ann Flow Data -------------------------------------

# get flow POR and add to annual
flow_por_wide <- flow_por_wide %>% mutate(WY=as.factor("POR"))

# join but make year a "factor"
flow_by_years_bmi_wide <- flow_by_years_bmi_wide %>% 
  mutate(WY=as.factor(year)) %>% 
  bind_rows(., flow_por_wide) %>% 
  mutate(WY=as.factor(WY))

# check distrib of years
table(flow_by_years_bmi_wide$WY)

# join with flow data by gage ID, then filter to years BMI data collected
bmi_flow_metrics_all <- left_join(bmi_metrics_df, flow_by_years_bmi_wide, by="ID") %>% 
  # fix years so Period of record shows up as 1900 and can be filtered out for annual vs. non
  mutate(WYs = as.integer(ifelse(WY=="POR", "1900", as.character(WY))))

table(bmi_flow_metrics_all$WY)
table(bmi_flow_metrics_all$WYs)

# make an annual dataset where flow data matches same year of bug data: 
bmi_flow_metrics_annual <- bmi_flow_metrics_all %>% 
  dplyr::filter(YYYY == WYs )#| YYYY == WYs-1 | YYYY==WYs-2)

table(bmi_flow_metrics_annual$WYs)

# make a lagged dataset, need to look for WYs + 1 equal to the actual bmi year:
# bmi_flow_metrics_all %>% select(SampleID, YYYY, WY, WYs) %>% head(n=40)
# bmi_flow_metrics_all %>% 
#   dplyr::filter(YYYY == WYs+1) %>% select(SampleID, YYYY, WY, WYs) %>% head(n=40)
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
