# 05 Merge BMI Data with Flow Data by year
## R. Peek
## Link the BMI data by flow data with a lag, annual, and POR

## DATA OUT:
### bmi_coms, file="data_output/05_selected_bmi_stations_w_comids.rda" (the comids for selected BMI stations n=224)
### bmi_csci_flow_por, file="data_output/05_selected_bmi_stations_w_csci_flow_por.rda" (period of record flow associated with available CSCI data, n=194)
### bmi_csci_flow_yrs, file="data_output/05_selected_bmi_stations_w_csci_flow_years.rda" (flow data with avail csci scores, 1,2yr lags n=432)
### bmi_dat, file="data_output/05_selected_bmi_cleaned_w_data.rda" (cleaned data w site status, n=27567)
### flow_por_wide, flow_por, file="data_output/05_selected_usgs_flow_metrics_POR.rda" (ref gage data for period of record, n=223)
### mainstems, file="data_output/05_mainstems_us_ds_selected_gages.rda" (mainstem NHD flowlines, us and ds, n=1581)

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(tidyverse)

# Load Data ---------------------------------------------------------------

load("data_output/03_selected_bmi_and_gages.rda")
load("data_output/03_selected_h12_contain_bmi_gage.rda")
load("data_output/03_selected_nhd_flowlines_mainstems.rda")
load("data_output/00_bmi_cleaned_all.rda") # all data
load("data_output/01_bmi_cleaned_stations_w_site_status.rda")

# load mapview bases
# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Get BMI comids ----------------------------------------------------------

# all stations us of gage:
bmi_us_coms <- sel_bmi_gages %>% filter(comid %in% mainstems_us$nhdplus_comid)

# all stations 15km downstream on mainstem
bmi_ds_coms <- sel_bmi_gages %>% filter(comid %in% mainstems_ds$nhdplus_comid)

# combine US and DS
bmi_coms <- rbind(bmi_ds_coms, bmi_us_coms)

# rm old layer:
rm(bmi_ds_coms, bmi_us_coms)

# join with status:
bmi_coms <- bmi_coms %>% left_join(bmi_clean_stations_ss[, c(1:2)], by="StationCode")

# Combine Mainstem NHDlines -----------------------------------------------

mainstems_ds <- mainstems_ds %>% 
  mutate(main_dir = "DS")
mainstems_us <- mainstems_us %>% 
  mutate(main_dir = "US")

mainstems <- rbind(mainstems_ds, mainstems_us)

# remove old layers
rm(mainstems_ds, mainstems_us)

# Join BMI Sites with BMI Data --------------------------------------------

# pull BMI sites and get list of data, first join with orig full dataset:
bmi_dat <- inner_join(bmi_coms, bmi_clean, by="StationCode") %>% 
  select(-c(benthiccollectioncomments, percentsamplecounted:gridsvolumeanalyzed, discardedorganismcount,
            benthiclabeffortcomments, resqualcode:personnelcode_labeffort, samplecomments, effortqacode))
  # inner join drops NAs (72 sites: is.na(bmi_coms_dat$SampleID)

# now look at how many unique samples are avail: n=266 unique samples
bmi_dat %>% as.data.frame() %>% group_by(SampleID) %>% distinct(SampleID) %>% tally

# now look at how many unique stations: n=142 stations
bmi_dat %>% as.data.frame() %>% group_by(StationCode) %>% distinct(StationCode) %>% tally

# Merge with Flow Dat -----------------------------------------------------

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
bmi_yrs <- bmi_dat %>% group_by(SampleID) %>% pull(YYYY) %>% unique()
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
save(bmi_dat, file="data_output/05_selected_bmi_cleaned_w_data.rda")
save(flow_por_wide, flow_por, file="data_output/05_selected_usgs_flow_metrics_POR.rda")
save(mainstems, file="data_output/05_mainstems_us_ds_selected_gages.rda")

