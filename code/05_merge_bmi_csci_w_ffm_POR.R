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
### bmi_coms_dat (all data for selected site pairs), 
### bmi_coms_final (just coms and id)
### bmi_coms_dat_trim (all data for selected site pairs btwn Jun-Sep)
load("data_output/03_selected_final_bmi_stations_dat_all_gages.rda") 

# CSCI data selected?
bmi_csci <- read_rds("data_output/04_selected_bmi_stations_w_csci.rds")

# CSCI all 
load("data_output/04_all_csci_data.rda")

# bmi w site status
load("data_output/01_bmi_stations_distinct_status.rda")

# the spatially joined points
sel_bmi_gages<-readRDS("data_output/03_selected_bmi_h12_all_gages.rds")
sel_gages_bmi<-readRDS("data_output/03_selected_usgs_h12_all_gages.rds")
sel_h12_bmi<-readRDS("data_output/03_selected_h12_all_gages.rds")
# nhd streamlines
load("data_output/03_selected_nhd_mainstems_gages.rda") # mainstems_all


# Get Functional Flow Data ------------------------------------------------

# pulled in 02 code

load("data_output/02_usgs_ref_ffc_alteration.rda") # alteration status: g_alt_ref
load("data_output/02_usgs_altered_ffc_alteration.rda") # alteration status: g_alt_alt
load("data_output/02_usgs_altered_ffc_metrics.rda") # ffc altered: g_alt_ffc
load("data_output/02_usgs_ref_ffc_metrics.rda") # ffc reference: g_ref_ffc

# need to trim out cols we don't need:
g_alt_ffc <- g_alt_ffc %>% select(names(g_ref_ffc)) 

# then merge
g_all_ffc <- bind_rows(g_alt_ffc, g_ref_ffc)

# rm old
rm(g_alt_ffc, g_ref_ffc)

# alteration status metrics (for POR)
# fix weird numeric vs. character
g_alt_alt <- g_alt_alt %>% mutate(gage_id = as.character(gage_id))
g_all_alt <- bind_rows(g_alt_alt, g_alt_ref)
rm(g_alt_alt, g_alt_ref)


# Tidy BMI/GAGE Data -----------------------------------------------------------

# make a new layer of "unselected" bmi sites, dropped bc off mainstem
bmi_not_selected <- sel_bmi_gages %>% 
  filter(!as.character(comid) %in% mainstems_all$nhdplus_comid) # should be 352

# get all gages selected # n=212
gages_selected <- sel_gages_bmi %>% 
  filter(gage_id %in% bmi_coms_final$gage_id)

# get the gages not selected # n=54
gages_not_selected <- sel_gages_bmi %>% 
  filter(!gage_id %in% bmi_coms_final$gage_id)

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

# fix sampleID to be YMD
library(lubridate)
csci <- csci %>% 
  mutate(MM = stringi::stri_pad_left(month(sampledate), 2, pad="0"),
         YYYY = year(sampledate),
         DD = stringi::stri_pad_left(day(sampledate), 2, pad="0"),
         SampleID2 = paste0(stationcode,"_", YYYY, MM, DD, "_", collectionmethodcode, "_", fieldreplicate)) %>% 
  select(-MM, -YYYY, -DD, -fieldreplicate, -collectionmethodcode)

# number of unique Stations: 
st_drop_geometry(bmi_coms_dat) %>% distinct(StationCode) # n=489
st_drop_geometry(bmi_coms_dat_trim) %>% distinct(StationCode) # n=291

# first trim to unique sampleIDs only (to match with CSCI) 
bmi_csci <- st_drop_geometry(bmi_coms_dat_trim) %>% distinct(SampleID, .keep_all=TRUE) %>% #n=457 
# match CSCI scores against selected sites
  inner_join(., csci, by=c("SampleID"="SampleID2")) %>% 
  select(StationCode:SampleID, count:sampleyear) # n=259 total (so 291-259 = 32 missing)

table(bmi_csci$CEFF_type)
# ALT = 159, REF = 100

# Look at Missing CSCI ----------------------------------------------------

bmi_csci_miss <- anti_join(bmi_coms_final, csci, by=c("StationCode"="stationcode")) %>% 
  distinct(StationCode, comid, .keep_all=T) %>% st_drop_geometry() 

#write_csv(bmi_csci_miss, path = "data_output/04_bmi_sites_missing_csci_data.csv")

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

