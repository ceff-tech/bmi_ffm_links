# 04 Generate Final Selected Sites/Data
## R. Peek
## Look at final output

## DATA OUT:
### - sel_h12_bmi (all huc12s with gage/bmi sites inside them, n=53)
### "data_output/03_selected_h12_contain_bmi_gage.rda"


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)

# Load Data ---------------------------------------------------------------

load("data_output/01_bmi_cleaned_stations_w_site_status.rda")
load("data_output/03_selected_bmi_and_gages.rda")
load("data_output/03_selected_nhd_flowlines_mainstems.rda")
load("data_output/03_selected_h12_contain_bmi_gage.rda")
load("data_output/00_bmi_cleaned_all.rda")
load("data_output/03_bmi_all_stations_comids.rda")

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

# bmi_us_coms %>% st_drop_geometry() %>% inner_join(., bmi_ds_coms, by="StationCode") %>% tally()

# Make Map of Selected Gages and BMI Stations --------------------------

# this map of all sites selected U/S and D/S
m3 <- mapview(bmi_ds_coms, cex=6, col.regions="orange", layer.name="Selected BMI D/S") +  
  mapview(mainstems_ds, color="darkblue", cex=3, layer.name="NHD D/S Flowline 15km", legend=F)+
  mapview(bmi_us_coms, cex=6, col.regions="yellow", layer.name="Selected BMI U/S") +  
  mapview(mainstems_us, color="slateblue", cex=3, layer.name="NHD U/S Flowline", legend=F)+
  mapview(sel_gages_bmi, col.regions="cyan", cex=7, layer.name="Selected USGS Gages") + 
  mapview(sel_bmi_gages, col.regions="gray", cex=3, alpha=0.5, layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# Make Map of Selected Stations by Site Status  --------------------------

# first add site status
bmi_ds_coms <- left_join(bmi_ds_coms, bmi_clean_stations_ss, by="StationCode")
bmi_us_coms <- left_join(bmi_us_coms, bmi_clean_stations_ss, by="StationCode")


m4 <- mapview(bmi_ds_coms, cex=6, zcol="SiteStatus", layer.name="Selected BMI D/S") +  
  mapview(mainstems_ds, color="darkblue", cex=3, layer.name="NHD D/S Flowline 15km", legend=F)+
  mapview(bmi_us_coms, cex=6, zcol="SiteStatus", layer.name="Selected BMI U/S") +  
  mapview(mainstems_us, color="slateblue", cex=3, layer.name="NHD U/S Flowline", legend=F)+
  mapview(sel_gages_bmi, zcol="stream_class",  cex=7, layer.name="Selected USGS Gages") + #col.regions="cyan",
  mapview(sel_bmi_gages, col.regions="gray", cex=3, alpha=0.5, layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m4@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# Combine BMI US and DS ---------------------------------------------------

# combine:
bmi_coms <- do.call(what = sf:::rbind.sf,
                    args = list(bmi_ds_coms, bmi_us_coms))
class(bmi_coms)

#library(DT)

# list of BMI sites
# bmi_coms %>% 
#   DT::datatable()

# pull BMI sites and get list of data, first join with orig full dataset:
bmi_coms_dat <- left_join(bmi_coms, bmi_clean, by="StationCode") %>% 
  # drop NAs (72 sites: is.na(bmi_coms_dat$SampleID)
  filter(!is.na(SampleID))

# now look at how many unique samples are avail: n=266 unique samples
bmi_coms_dat %>% as.data.frame() %>% group_by(SampleID) %>% distinct(SampleID) %>% tally

# now look at how many unique stations: n=142 stations
bmi_coms_dat %>% as.data.frame() %>% group_by(StationCode) %>% distinct(StationCode) %>% tally


# Get BMI comids ----------------------------------------------------------

# all stations us of gage:
bmi_us_coms <- sel_bmi_gages %>% filter(comid %in% mainstems_us$nhdplus_comid)

# all stations 15km downstream on mainstem
bmi_ds_coms <- sel_bmi_gages %>% filter(comid %in% mainstems_ds$nhdplus_comid)

# combine US and DS
bmi_coms <- rbind(bmi_ds_coms, bmi_us_coms)

# distinct stations:
bmi_coms %>% st_drop_geometry() %>% distinct(StationCode, ID) %>% tally()
bmi_coms %>% st_drop_geometry() %>% distinct(comid) %>% tally()

# potential sites:
#bmi_coms %>% View()

# rm old layer:
rm(bmi_ds_coms, bmi_us_coms)


# Check against CSCI Scores -----------------------------------------------

# see what data exist against CSCI scores currently avail (from Raffi)
csci <- read_csv("data/csci_core.csv")

# match against existing sites:
bmi_csci <- inner_join(bmi_coms, csci, by=c("StationCode"="stationcode"))

bmi_csci <- left_join(bmi_csci, bmi_clean_stations_ss[,c(1:2)], by="StationCode")

# how many unique matches?
length(unique(bmi_csci$StationCode))
table(bmi_csci$SiteStatus)

# look at CSCI
hist(bmi_csci$csci_percentile)

# look at sampling timing
hist(bmi_csci$samplemonth)
table(bmi_csci$samplemonth)

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

# plot CSCI percentile
ggplot(data=filter(bmi_csci, !is.na(SiteStatus)), aes(x=SiteStatus, y=csci_percentile)) + 
  geom_boxplot(aes(fill=SiteStatus), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text", hjust=1, vjust=0.9) +
  theme_bw()

ggplot(data=bmi_csci, aes(x=SiteStatus, y=csci_percentile)) + 
  geom_boxplot(aes(fill=SiteStatus), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text", hjust=1, vjust=0.9) +
  theme_bw()

