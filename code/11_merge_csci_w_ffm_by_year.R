## 11 Merge Data by Year for Plotting

# once appropriate metrics have been identified, merge FFM (raw metrics) with 
# biological data (CSCI/ASCI) on annual scale to look at relationships.

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList, fgb = FALSE)

library(lubridate)
library(tidyverse)
library(tidylog)

# Load Data ---------------------------------------------------------------

# BMI data:
load("data_output/05_bmi_csci_por_trim_ecoreg.rda")

# rename for ease of use and drop sf
bmi_csci_por_trim <- bmi_csci_por_trim_ecoreg %>% st_drop_geometry()

# REVISED ECO REGIONS: csci data w ecoregions (see 05b)
# just ecoregions:
# eco_revised <- read_rds("data/spatial/ecoregions_combined_L3.rds")
#bmi_csci_por_trim_ecoreg: all trimmed CSCI data with ecoregions
load("data_output/05_bmi_csci_por_trim_ecoreg.rda")
ecoregs_bmi_stations <- bmi_csci_por_trim_ecoreg %>% select(StationCode, US_L3_mod, geometry) %>% distinct(.keep_all=TRUE)
mapview(ecoregs_bmi_stations, zcol="US_L3_mod")


# get Functional Flow Metric data 
#ffc_dat <- read_rds(file = url("https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_alteration.rds"))
ffm_dat <- read_rds <- read_rds("data_output/usgs_combined_ffc_results_long.rds")

# Make ANNUAL Dataset -----------------------------------------------------

## CAN ONLY USE RAW FFM DATA WITH ANNUAL BECAUSE ALTERATION STATUS IS CALCULATED BASED ON PERCENTILES NOT ON SINGLE YEARS

# See number of BMI years by station-pair:
csci_ecoreg <- bmi_csci_por_trim_ecoreg %>% st_drop_geometry() %>% 
  select(StationCode, US_L3_mod, site_id:lon, date_begin:sampledate, comid_ffc) %>% 
  group_by(SampleID, site_id, YYYY) %>% tally() # n=714 pair years

# what about number of years?
bmi_csci_por_trim_ecoreg %>% st_drop_geometry() %>% 
  select(StationCode, US_L3_mod, site_id:lon, date_begin:sampledate, comid_ffc) %>% 
  group_by(StationCode, site_id) %>% distinct(YYYY, .keep_all=TRUE) %>% tally() %>% 
  arrange(desc(n)) %>% filter(n>1) %>%  View() 
 # n=431 pairs with at least one unique year, 95 with >1 year of data

# untrimmed = 349

# join for annual data by GAGE ID, and BMI Sample YEAR = USGS Year
csci_ffm_ann <- bmi_csci_por_trim_ecoreg %>% st_drop_geometry() %>% 
  select(StationCode, US_L3_mod, latitude:HUC_12, site_id, station_nm, lat, lon, date_begin:sampledate, date_begin:sampledate) %>% 
  left_join(., ffm_dat, by=c("site_id"="gageid", "YYYY"="year"))


# Make LAGGED Dataset -----------------------------------------------------

# years -1  -2
lag_yrs_1 <- unique(csci_ffm_ann$YYYY) - 1
lag_yrs_2 <- unique(csci_ffm_ann$YYYY) - 2

# make lag data
ffm_lag1 <- ffm_dat %>% filter(year %in% lag_yrs_1) %>% 
  mutate(year_flow = year-1) # add for labeling purposes
ffm_lag2 <- ffm_dat %>% filter(year %in% lag_yrs_2) %>% 
  mutate(year_flow = year-2) # add for labeling purposes

# rejoin LAG1
csci_ffm_lag1 <- bmi_csci_por_trim_ecoreg %>% st_drop_geometry() %>% 
  select(StationCode, US_L3_mod, latitude:HUC_12, site_id, station_nm, lat, lon, date_begin:sampledate, date_begin:sampledate) %>%
  left_join(., ffm_lag1, by=c("site_id"="gageid", "YYYY"="year"))

# rejoin LAG2
csci_ffm_lag2 <- bmi_csci_por_trim_ecoreg %>% st_drop_geometry() %>% 
  select(StationCode, US_L3_mod, latitude:HUC_12, site_id, station_nm, lat, lon, date_begin:sampledate, date_begin:sampledate) %>%
  left_join(., ffm_lag2, by=c("site_id"="gageid", "YYYY"="year"))

# SAVE OUT ----------------------------------------------------------------

save(csci_ffm_ann, file="data_output/11_csci_ffm_ann_trim.rda")
save(csci_ffm_lag1, file="data_output/11_csci_ffm_lag1_trim.rda")
save(csci_ffm_lag2, file="data_output/11_csci_ffm_lag2_trim.rda")

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

