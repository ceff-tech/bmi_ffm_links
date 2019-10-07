# 06_merge_flow_csci_w_bug metrics

# calc bug metrics and merge with flow metrics

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(CSCI)
library(BMIMetrics)
library(lubridate)

# Data --------------------------------------------------------------------


load("data_output/selected_bmi_stations_w_comids.rda")
load("data_output/maintems_us_ds_selected_gages.rda")
load("data_output/selected_bmi_stations_w_csci_flow_por.rda")
load("data_output/selected_bmi_stations_w_csci_flow_annual.rda")
load("data_output/selected_bmi_stations_w_data.rda")
load("data_output/selected_usgs_flow_metrics_POR.rda")
load("data_output/selected_flow_by_years_of_bmi.rda")
load("selected_bmi_metrics_at_gage_sites.rda")

# make non-sf: 
bmi_csci_flow_yrs <- bmi_csci_flow_yrs %>% st_drop_geometry() %>% as.data.frame


# Calc Bug Metrics --------------------------------------------------------

# first filter to years of interest

bmi_filt <- bmi_dat %>%
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
bmi_sampleids <- bmi_filt %>% st_drop_geometry() %>% distinct(SampleID, .keep_all = T) %>%
  select(1:4, 8:24)

# flatten and rejoin with station data:
bmi_metrics_df <- bug_metrics %>%
  do.call(what = rbind) %>%
  remove_rownames() %>%
  inner_join(., bmi_sampleids, by="SampleID")

# clean workspace, rm old bits
rm(bugs_agg, bugs_samp, bugs_split, bug_metrics, bmi_filt)

# SAVE IT
save(bmi_metrics_df, file="selected_bmi_metrics_at_gage_sites.rda")


# Join Bug Metrics with Ann Flow Data -------------------------------------

# get flow POR and add to annual
flow_por_wide <- flow_por_wide %>% mutate(WY=as.factor("POR"))

# join but make year a "factor"
flow_by_years_bmi_wide <- flow_by_years_bmi_wide %>% 
  mutate(WY=as.factor(year)) %>% 
  bind_rows(., flow_por_wide) %>% 
  mutate(WY=as.factor(WY))

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

bmi_flow_metrics_lag1 <- bmi_flow_metrics_all %>% 
  dplyr::filter(YYYY == WYs+1)

bmi_flow_metrics_lag2 <- bmi_flow_metrics_all %>% 
  dplyr::filter(YYYY == WYs+2)

# make POR dataset
bmi_flow_metrics_por <- bmi_flow_metrics_all %>% 
  dplyr::filter(WYs==1900)

# Now add CSCI: but need to regen sampleID for CSCI data (station_YMD_samplemethod_replicate)
bmi_csci_flow <- bmi_csci_flow %>% 
  mutate(SampleID=paste0(StationCode, "_", year(sampledate), sprintf("%02d", month(sampledate)), sprintf("%02d", day(sampledate)), "_", collectionmethodcode, "_", fieldreplicate)) %>% st_drop_geometry()

# make a distinct SampleID list of csci
csci_only <- bmi_csci_flow %>% select(SampleID, csci, csci_percentile) %>% 
  distinct(SampleID, .keep_all = T)

# now join
bmi_flow_metrics_por_csci <- left_join(bmi_flow_metrics_por, csci_only)
bmi_flow_metrics_ann_csci <- left_join(bmi_flow_metrics_annual, csci_only)
bmi_flow_metrics_lag1_csci <- left_join(bmi_flow_metrics_lag1, csci_only)
bmi_flow_metrics_lag2_csci <- left_join(bmi_flow_metrics_lag2, csci_only)

save(bmi_flow_metrics_por_csci, file="data_output/selected_bmi_flow_metrics_w_csci_POR.rda")
save(bmi_flow_metrics_ann_csci, file="data_output/selected_bmi_flow_metrics_w_csci_ANN.rda")
save(bmi_flow_metrics_lag1_csci, file="data_output/selected_bmi_flow_metrics_w_csci_LAG1.rda")
save(bmi_flow_metrics_lag2_csci, file="data_output/selected_bmi_flow_metrics_w_csci_LAG2.rda")
