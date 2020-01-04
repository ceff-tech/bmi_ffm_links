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

# Load Data ---------------------------------------------------------------

load("data_output/00_bmi_cleaned_all.rda") # bmi_clean
load("data_output/01_bmi_cleaned_stations_w_site_status.rda") # bmi_clean_stations_ss (site status)
load("data_output/03_selected_bmi_and_gages_same_h12_all_gages.rda") # sel_bmi_gages, sel_gages_bmi
load("data_output/03_selected_nhd_flowlines_mainstems_all_gages.rda") # mainstems_us, mainstems_ds
sel_h12 <- read_rds("data_output/03_selected_h12_all_gages.rds") # all h12s w bmi and gage: sel_h12_bmi

load("data_output/03_final_bmi_stations_dat_all_gages.rda") # bmi_coms_dat (all data for selected), bmi_coms_final (just coms and id)
bmi_coms <- read_rds("data_output/02_bmi_all_stations_comids.rds") # just bmi_coms, comids for all BMI sites

# re order cols
bmi_coms_final <- bmi_coms_final %>% select(StationCode, longitude, latitude, HUC_12, h12_area_sqkm, ID:to_gage, geometry)

# make a mainstems all file
mainstems_all <- rbind(mainstems_us, mainstems_ds) %>% 
  rename(from_gage=to_gage)

# rm old layers
rm(mainstems_ds, mainstems_us)

# make a new layer of "unselected" bmi sites
bmi_not_selected <- sel_bmi_gages %>% filter(!as.character(comid) %in% mainstems_all$nhdplus_comid) # should be 561 = (2188 total -  1627 selected)

# join with status:
bmi_coms_final <- bmi_coms_final %>% left_join(bmi_clean_stations_ss[, c(1:2)], by="StationCode")

# load mapview bases
# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Filter to Selected Data Set --------------------------------------------

# use selected dataset and drop unnecessary cols
bmi_final_dat <- bmi_coms_dat %>% 
  select(-c(benthiccollectioncomments, percentsamplecounted:gridsvolumeanalyzed, discardedorganismcount,
            benthiclabeffortcomments, resqualcode:personnelcode_labeffort, samplecomments, effortqacode))

# now look at how many unique samples are avail: n=1507 unique samples
bmi_final_dat %>% 
  st_drop_geometry() %>% # need to remove sf class to make dataframe easier to work with
  distinct(SampleID) %>% tally

# now look at how many unique stations: n=792 stations
bmi_final_dat %>% st_drop_geometry %>% distinct(StationCode) %>% tally

# Merge with Flow Dat -----------------------------------------------------

# need to get gage data for all gages here:
load("data/ref_gage_annFlow_stats_long.rda")

# need to add "T" to the gageID
flow_long <- dat_long %>% mutate(ID=paste0("T", gage)) %>% 
  # filter out NA's?
  filter(!is.na(data)) %>% 
  ungroup() %>% 
  select(ID, gage, stream_class, stat:YrRange)

# rm old dataset
rm(dat_long)

# Avg Metrics for Period of Record ----------------------------------------

# set ID vars for flow metrics
flow_idvars <- flow_long %>% group_by(ID, stat) %>% distinct(ID, stat, maxYr, minYr)

# now avg for PERIOD OF RECORD for CSCI comparison
flow_por <- flow_long %>% select(-YrRange, -gage, -year, -stream_class) %>% group_by(ID, stat) %>% 
  summarize_at(vars(data), mean, na.rm=T) %>% 
  # rejoin yr ranges
  left_join(., flow_idvars)

# unique metrics? (should be 34)
length(unique(flow_por$stat))

# make wide for join
flow_por_wide <- flow_por %>% 
  pivot_wider(names_from=stat, values_from=data)
#values_fill=list(data=0))
# can fill missing values with zero if preferred: values_fill=list(data=0)

# check number unique gages:
flow_por_wide %>% distinct(ID) %>% dim # should be 223

# Get Flow Record only in Same Year/lag as BMI Sites ----------------------

# need to pull 2 years prior, 1 year prior, and same year as BMI site data

# make vector of years and BMI_ids
bmi_yrs <- bmi_final_dat %>% group_by(SampleID) %>% pull(YYYY) %>% unique()
(bmi_yrs_2 <- bmi_yrs - 2) # set lag 2
(bmi_yrs_1 <- bmi_yrs - 1) # set lag 1

# now combine and order:
bmi_years <- combine(bmi_yrs, bmi_yrs_1, bmi_yrs_2) %>% sort() %>% unique() # 25 total years to match with flow record
#bmi_years # 1993:2017

# rm old files
rm(bmi_yrs, bmi_yrs_1, bmi_yrs_2)

# now match with flow data (only goes through 2016)
flow_by_years_bmi <- flow_long %>% 
  filter(year %in% bmi_years) # 1993:2017

# make wide
flow_by_years_bmi_wide <- flow_by_years_bmi %>% 
  pivot_wider(names_from=stat, values_from=data)

flow_by_years_bmi_wide %>% distinct(ID) %>% dim # should be 106 gages match same years

# save flow data out for annual match
save(flow_by_years_bmi, flow_by_years_bmi_wide, file="data_output/05_selected_flow_by_years_of_bmi.rda")

# Get CSCI Data -----------------------------------------------------------

# see what data exist against CSCI scores currently avail (from Raffi)
csci <- read_csv("data/csci_core.csv") 
csci %>% 
  distinct(sampleid) %>% tally()

# match against existing sites:
bmi_csci <- inner_join(bmi_coms, csci, by=c("StationCode"="stationcode")) %>% 
  distinct(globalid, .keep_all = T)

# how many unique matches?
length(unique(bmi_csci$StationCode))
bmi_csci %>% st_drop_geometry() %>% group_by(SiteStatus) %>% tally()

# look at CSCI
hist(bmi_csci$csci_percentile)

# look at CSCI percentile by Site Status (not avail for all sites)
ggplot() + geom_boxplot(data=bmi_csci, aes(x=SiteStatus, y=csci_percentile))

# function to get data
stat_box_data <- function(y, upper_limit = max(bmi_csci$csci_percentile)) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}

# plot CSCI percentile no NAs
ggplot(data=filter(bmi_csci, !is.na(SiteStatus)), aes(x=SiteStatus, y=csci_percentile)) + 
  geom_boxplot(aes(fill=SiteStatus), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text",cex=3, hjust=1, vjust=0.9) +
  ylab("CSCI (Percentile)") + xlab("Site Status")+
  theme_bw()

# plot CSCI percentile w/ NAs
ggplot(data=bmi_csci, aes(x=SiteStatus, y=csci_percentile)) + 
  geom_boxplot(aes(fill=SiteStatus), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text", cex=3, hjust=1, vjust=0.9) +
  ylab("CSCI (Percentile)") + xlab("Site Status")+
  theme_bw()

# Join with Flow POR ------------------------------------------------------

bmi_csci_flow_por <- left_join(bmi_csci, flow_por_wide, by="ID")

# filter to sites that have data in the flow time range? # doesn't matter for POR?
bmi_csci_flow_por_overlap <- bmi_csci_flow_por %>%
  filter(sampleyear > minYr, sampleyear< maxYr)

length(unique(bmi_csci_flow_por_overlap$StationCode)) # 76 stations
length(unique(bmi_csci_flow_por_overlap$ID)) # 36 gages

# Join with Flow by Years that Match/lag BMI -------------------------------

bmi_csci_flow_yrs <- left_join(bmi_csci, flow_by_years_bmi_wide, by=c("ID")) %>% 
  # filter to same year as BMI + 2 yr lag
  filter(sampleyear == year | sampleyear == year+1 | sampleyear==year+2)

# double check
# bmi_csci_flow_yrs %>% select(StationCode, sampleid, sampleyear, year) %>% View()

# filter to sites that have data in the flow time range?
bmi_csci_flow_yrs %>% as.data.frame() %>% distinct(StationCode) %>% tally()
bmi_csci_flow_yrs %>% as.data.frame() %>% distinct(sampleid, year) %>% tally()
bmi_csci_flow_yrs %>% as.data.frame() %>% distinct(ID) %>% tally()

# Export Cleaned Data -----------------------------------------------------

save(bmi_coms, file="data_output/05_selected_bmi_stations_w_comids.rda")

save(bmi_csci_flow_por, file="data_output/05_selected_bmi_stations_w_csci_flow_por.rda")
save(bmi_csci_flow_yrs, file="data_output/05_selected_bmi_stations_w_csci_flow_years.rda")
save(bmi_final_dat, file="data_output/05_selected_bmi_cleaned_w_data.rda")
save(flow_por_wide, flow_por, file="data_output/05_selected_usgs_flow_metrics_POR.rda")
save(mainstems, file="data_output/05_mainstems_us_ds_selected_gages.rda")

