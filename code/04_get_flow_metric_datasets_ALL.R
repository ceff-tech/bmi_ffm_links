# 05 Merge BMI Data with Flow Data by year
## R. Peek
## Link the BMI data by flow data with a lag, annual, and POR

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
load("data_output/03_selected_bmi_and_gages_same_h12_all_gages.rda") # sel_bmi_gages, sel_gages_bmi
load("data_output/03_selected_nhd_flowlines_mainstems_all_gages.rda") # mainstems_us, mainstems_ds
sel_h12 <- read_rds("data_output/03_selected_h12_all_gages.rds") # all h12s w bmi and gage: sel_h12_bmi

load("data_output/03_final_bmi_stations_dat_all_gages.rda") # bmi_coms_dat (all data for selected), bmi_coms_final (just coms and id)
bmi_coms <- read_rds("data_output/02_bmi_all_stations_comids.rds") # just bmi_coms, comids for all BMI sites

# re order cols
bmi_coms_final <- bmi_coms_final %>% 
  select(StationCode, longitude, latitude, 
         HUC_12, h12_area_sqkm, ID:comid, geometry) %>% 
# add site status
  left_join(bmi_clean_stations_ss[, c(1:2)], by="StationCode") %>% 
  distinct(StationCode, ID, .keep_all = T) # 1627 total

# make a mainstems all file
mainstems_all <- rbind(mainstems_us, mainstems_ds) %>% 
  rename(from_gage=to_gage)

# rm old layers
rm(mainstems_ds, mainstems_us)

# make a new layer of "unselected" bmi sites
bmi_not_selected <- sel_bmi_gages %>% 
  filter(!as.character(comid) %in% mainstems_all$nhdplus_comid) # should be 561 = (2188 total -  1627 selected)

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Filter to Selected Data Set --------------------------------------------

# use selected dataset and drop unnecessary cols
bmi_final_dat <- bmi_coms_dat %>% 
  select(-c(benthiccollectioncomments, percentsamplecounted:gridsvolumeanalyzed, 
            discardedorganismcount,
            benthiclabeffortcomments, resqualcode:personnelcode_labeffort, 
            samplecomments, effortqacode)) %>% 
  st_drop_geometry()

# now look at how many unique samples are avail: n=1507 unique samples
bmi_final_dat %>% 
  distinct(SampleID) %>% tally

# now look at how many unique stations: n=792 stations
bmi_final_dat %>% distinct(StationCode) %>% tally

# Merge with Flow Dat -----------------------------------------------------

# get a list of gages from the bmi_coms_final (n=517)
usgs_list <- bmi_coms_final %>% st_drop_geometry %>% 
  distinct(ID)

# get metadata with dataRetrieval
usgs_list <- dataRetrieval::whatNWISdata(siteNumber=usgs_list$ID, service='dv', parameterCd = '00060', statCd='00003') %>% 
    select(site_no, station_nm, dec_lat_va, dec_long_va, huc_cd, 
           data_type_cd, begin_date:count_nu) %>% 
    rename(interval=data_type_cd, huc8=huc_cd, site_id=site_no,
           date_begin=begin_date, date_end=end_date) %>% 
    mutate(yr_begin = year(date_begin),
           yr_end = year(date_end),
           yr_total = yr_end-yr_begin) %>% 
    filter(yr_total > 9) 
  # 441 left with > 9 yrs of data

usgs_list$site_id <- as.integer(usgs_list$site_id)

# RUN THE `get_altered_gage_ffc_data.R` or `get_reference_gage_ffc_data.R` here


# Load FFC Alteration Data and Join ---------------------------------------

# altered
load("data_output/usgs_altered_ffc_alteration.rda")

# reference
load("data_output/usgs_ref_ffc_alteration.rda")

# bind together:
flow_alt_df <- bind_rows(g_alt_alt, g_ref_alt)

# cross with the actual gage_list

flow_alt_df_filt <- flow_alt_df %>% filter(gage_id %in% usgs_list$site_id)

# check unique gages
flow_alt_df_filt %>% distinct(gage_id) %>% tally()

# Get Flow Record only in Same Year/lag as BMI Sites ----------------------
## NEED TO WAIT ON THIS DON"T HAVE CURRENTLY

# # need to pull 2 years prior, 1 year prior, and same year as BMI site data
# 
# # make vector of years and BMI_ids
# bmi_yrs <- bmi_final_dat %>% group_by(SampleID) %>% pull(YYYY) %>% unique()
# (bmi_yrs_2 <- bmi_yrs - 2) # set lag 2
# (bmi_yrs_1 <- bmi_yrs - 1) # set lag 1
# 
# # now combine and order:
# bmi_years <- combine(bmi_yrs, bmi_yrs_1, bmi_yrs_2) %>% sort() %>% unique() # 25 total years to match with flow record
# #bmi_years # 1993:2017
# 
# # rm old files
# rm(bmi_yrs, bmi_yrs_1, bmi_yrs_2)
# 
# # now match with flow data (only goes through 2016)
# flow_by_years_bmi <- flow_long %>% 
#   filter(year %in% bmi_years) # 1993:2017
# 
# # make wide
# flow_by_years_bmi_wide <- flow_by_years_bmi %>% 
#   pivot_wider(names_from=stat, values_from=data)
# 
# flow_by_years_bmi_wide %>% distinct(ID) %>% dim # should be 106 gages match same years
# 
# # save flow data out for annual match
# save(flow_by_years_bmi, flow_by_years_bmi_wide, file="data_output/05_selected_flow_by_years_of_bmi.rda")

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

bmi_csci_flow_por <- inner_join(bmi_csci, flow_alt_df_filt, by="comid")

#bmi_csci_flow_por <- left_join(bmi_csci, flow_por_wide, by="ID")

length(unique(bmi_csci_flow_por$StationCode)) # 101 stations
length(unique(bmi_csci_flow_por$gage_id)) # 74 gages

# Join with Flow by Years that Match/lag BMI -------------------------------

# bmi_csci_flow_yrs <- left_join(bmi_csci, flow_by_years_bmi_wide, by=c("ID")) %>% 
#   # filter to same year as BMI + 2 yr lag
#   filter(sampleyear == year | sampleyear == year+1 | sampleyear==year+2)
# 
# # double check
# # bmi_csci_flow_yrs %>% select(StationCode, sampleid, sampleyear, year) %>% View()
# 
# # filter to sites that have data in the flow time range?
# bmi_csci_flow_yrs %>% as.data.frame() %>% distinct(StationCode) %>% tally()
# bmi_csci_flow_yrs %>% as.data.frame() %>% distinct(sampleid, year) %>% tally()
# bmi_csci_flow_yrs %>% as.data.frame() %>% distinct(ID) %>% tally()

# Export Cleaned Data -----------------------------------------------------

save(bmi_coms, file="data_output/05b_selected_bmi_stations_w_comids.rda")

save(bmi_csci_flow_por, file="data_output/05b_selected_bmi_stations_w_csci_flow_por.rda")
save(bmi_csci_flow_yrs, file="data_output/05b_selected_bmi_stations_w_csci_flow_years.rda")
save(bmi_final_dat, file="data_output/05b_selected_bmi_cleaned_w_data.rda")
save(flow_por_wide, flow_por, file="data_output/05b_selected_usgs_flow_metrics_POR.rda")
save(mainstems, file="data_output/05b_mainstems_us_ds_selected_gages.rda")

