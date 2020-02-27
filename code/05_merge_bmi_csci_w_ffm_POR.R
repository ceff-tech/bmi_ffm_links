# 05 Merge BMI CSCI Data with Flow Data for Period of Record
## R. Peek

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(tidyverse)
library(tidylog)

# Load Data ---------------------------------------------------------------

# bmi data:
### bmi_coms_dat (all data for selected), 
### bmi_coms_final (just coms and id)
load("data_output/02_selected_final_bmi_stations_dat_all_gages.rda") 
# CSCI data selected?
bmi_csci <- read_rds("data_output/04_selected_bmi_stations_w_csci.rds")

# CSCI all 
load("data_output/04_all_csci_data.rda")

# bmi w site status
load("data_output/01_bmi_stations_distinct_status.rda")

# the spatially joined points
sel_bmi_gages <- readRDS("data_output/02_selected_bmi_h12_all_gages.rds")
sel_gages_bmi <- readRDS("data_output/02_selected_usgs_h12_all_gages.rds")
sel_h12 <- read_rds("data_output/02_selected_h12_all_gages.rds")

# nhd streamlines
load("data_output/02_selected_nhd_mainstems_all_gages.rda") # mainstems_all

# flow alteration status:
load("data_output/04_usgs_all_ffc_alteration.rda")
load("data_output/04_usgs_all_ffc_metrics.rda")
g_all_ffc %>% select(-c(gage_id, Year, `__summer_durations_flush`, `__summer_no_flow_counts`, contains("Julian"))) %>% names()
usgs_ffstat <- g_all_alt; rm(g_all_alt)

# re order cols
bmi_coms_final <- bmi_coms_final %>% 
  select(StationCode, longitude, latitude, 
         HUC_12, h12_area_sqkm, ID:comid, geometry) %>% 
# add site status
  left_join(bmi_stations_distinct_status[, c(1:2)], by="StationCode") %>% 
  distinct(StationCode, ID, .keep_all = T) # 1597 total

# make a new layer of "unselected" bmi sites, dropped bc off mainstem
bmi_not_selected <- sel_bmi_gages %>% 
  filter(!as.character(comid) %in% mainstems_all$nhdplus_comid) # should be 591 = (2188 total -  1597 selected)

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Get CSCI Data -----------------------------------------------------------

# see what data exist against CSCI scores currently avail (from Raffi)
csci1 <- read_csv("data/csci/csci_core.csv") %>% 
  mutate(sampledate=as.Date(sampledate)) %>% 
  select(sampleid, stationcode, sampledate, collectionmethodcode, fieldreplicate, count, csci, csci_percentile)
csci2 <- read_csv("data/csci/csci_core_v2.csv") %>% 
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
bmi_csci <- inner_join(bmi_coms_final, csci, 
                       by=c("StationCode"="stationcode")) %>% # n=2081
  distinct(sampleid, ID, .keep_all = T) # 2049 distinct combos of sampleid/ID

bmi_csci_miss <- anti_join(bmi_coms_final, csci, by=c("StationCode"="stationcode")) %>% distinct(StationCode, comid, .keep_all=T) %>% st_drop_geometry() %>% 
  select(-elev_m, -h12_area_sqkm, -date_begin, -date_end, -end_yr) %>% 
  rename(gageID=ID)

# write_csv(bmi_csci_miss, path = "data_output/04_bmi_sites_missing_csci_data.csv")

# how many unique matches?
length(unique(bmi_coms_final$StationCode)) 
# 771 stations (but some w mult gage matches)
length(unique(bmi_csci$StationCode)) # only 575 matches

# view Site Status
bmi_csci %>% st_drop_geometry() %>% 
  group_by(SiteStatus) %>% tally()

# Make BMI POR FF Dataset -----------------------------------------------

# RUN THE `get_altered_gage_ffc_data.R` or `get_reference_gage_ffc_data.R` here

# join together csci data with ffm alteration status data
bmi_csci_por <- bmi_csci %>% 
  inner_join(., usgs_ffstat, by=c("comid", "ID"="gage_id")) %>% 
  distinct(sampleid, metric, gage_id, comid, .keep_all=TRUE)

# so based on Gages (n=147)
bmi_csci_por %>% st_drop_geometry() %>% distinct(ID) %>% tally()
# so based on BMI Stations (n=196)
bmi_csci_por %>% st_drop_geometry() %>% distinct(StationCode) %>% tally()

# Visualize ---------------------------------------------------------------

# function to get data
stat_box_data <- function(y, upper_limit = max(bmi_csci_por$csci, na.rm = TRUE)) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}

# plot CSCI no NAs
ggplot(data=filter(bmi_csci_por, !is.na(SiteStatus)), aes(x=SiteStatus, y=csci)) + 
  geom_boxplot(aes(fill=SiteStatus), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text",cex=3, hjust=1, vjust=0.9) +
  ylab("CSCI") + xlab("Site Status")+
  theme_bw()

# plot alteration status
ggplot(data=bmi_csci_por, aes(x=status, y=csci)) + 
  geom_boxplot(aes(fill=status), show.legend = F) +
  ylab("CSCI") + xlab("Alteration Status")+
  theme_bw()

# plot CSCI percentile w/ NAs
ggplot(data=bmi_csci, aes(x=SiteStatus, y=csci_percentile)) + 
  geom_boxplot(aes(fill=SiteStatus), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text", cex=3, hjust=1, vjust=0.9) +
  ylab("CSCI") + xlab("Site Status")+
  theme_bw()


length(unique(bmi_csci_por$StationCode)) # 196 stations
length(unique(bmi_csci_por$ID)) # 147 gages


# Export Cleaned Data -----------------------------------------------------

# save the bmi_csci_por
write_rds(bmi_csci_por, path = "data_output/05_selected_bmi_stations_w_csci_ffm_alt_por.rds")
write_rds(bmi_csci, path = "data_output/05_selected_bmi_stations_w_csci.rds")
save(csci, file="data_output/05_all_csci_data.rda")

