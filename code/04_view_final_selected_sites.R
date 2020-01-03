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

# Load Data: REFERENCE GAGE SET -------------------------------------------

load("data_output/00_bmi_cleaned_all.rda") # bmi_clean
load("data_output/01_bmi_cleaned_stations_w_site_status.rda") # bmi_clean_stations_ss (site status)
load("data_output/02_selected_bmi_and_gages_same_h12.rda") # sel_bmi_gages, sel_gages_bmi
load("data_output/02_selected_nhd_flowlines_mainstems.rda") # mainstems_us, mainstems_ds
load("data_output/02_selected_h12_contain_bmi_gage.rda") # all h12s w bmi and gage: sel_h12_bmi
load("data_output/02_final_bmi_stations_dat_reference.rda") # bmi_coms_dat (all data for selected), bmi_coms_final (just coms and id)
bmi_coms <- read_rds("data_output/02_bmi_all_stations_comids.rds") # just bmi_coms, comids for all BMI sites

# re order cols
bmi_coms_final <- bmi_coms_final %>% select(StationCode, longitude, latitude, HUC_12, h12_area_sqkm, ID:to_gage, geometry)

# make a mainstems all file
mainstems_all <- rbind(mainstems_us, mainstems_ds)

# make a new layer of "unselected" bmi sites
bmi_not_selected <- sel_bmi_gages %>% filter(!as.character(comid) %in% mainstems_all$nhdplus_comid) # 71 distinct stations = (210 total unique stations selected - 139 unique selected)

# Set up Mapview Basemap --------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Make Map of Selected Gages and BMI Stations --------------------------

# this map of all sites selected U/S and D/S
m3 <- mapview(bmi_coms_final, zcol="to_gage", cex=6, col.regions=c("orange","maroon"), layer.name="Final BMI Sites") +  
  mapview(mainstems_ds, color="darkblue", cex=3, lwd=4, layer.name="NHD D/S Flowline 15km", legend=F)+
  mapview(mainstems_us, color="slateblue", cex=3, layer.name="NHD U/S Flowline", legend=F)+
  mapview(sel_gages_bmi, col.regions="cyan", cex=7, layer.name="Reference USGS Gages") + 
  mapview(bmi_not_selected, col.regions="gray", cex=3.2, alpha=0.5, layer.name="Other BMI Sites in H12") +
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# Make Map of Selected Stations by Site Status  --------------------------

# first add site status
bmi_coms_final <- left_join(bmi_coms_final, bmi_clean_stations_ss, by="StationCode")

m4 <- mapview(bmi_coms_final, zcol="SiteStatus", cex=6, layer.name="Final BMI Sites") +  
  mapview(mainstems_ds, color="darkblue", cex=3, layer.name="NHD D/S Flowline 15km", legend=F)+
  mapview(mainstems_us, color="slateblue", cex=3, layer.name="NHD U/S Flowline", legend=F)+
  mapview(sel_gages_bmi, zcol="stream_class",  cex=7, layer.name="Selected USGS Gages") + #col.regions="cyan",
  mapview(bmi_not_selected, col.regions="gray", cex=3, alpha=0.5, layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m4@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# View Final Tally --------------------------------------------------------

# any NA's?
bmi_coms_dat %>% st_drop_geometry %>% filter(is.na(StationCode)) # nope

# now look at how many unique samples are avail: n=267 unique samples
bmi_coms_dat %>% st_drop_geometry %>% distinct(SampleID) %>% tally

# now look at how many unique stations: n=139 stations
bmi_coms_dat %>% st_drop_geometry %>% distinct(StationCode) %>% tally

# distinct stations:
bmi_coms %>% distinct(StationCode) %>% tally()

# Check against CSCI Scores -----------------------------------------------

# see what data exist against CSCI scores currently avail (from Raffi)
csci <- read_csv("data/csci_core.csv")

# match against existing sites:
bmi_csci <- inner_join(bmi_coms_final, csci, by=c("StationCode"="stationcode"))

bmi_csci <- left_join(bmi_csci, bmi_clean_stations_ss[,c(1:2)], by="StationCode")

# how many unique matches?
length(unique(bmi_csci$StationCode))
table(bmi_csci$SiteStatus)

# look at CSCI histogram of all scores
bmi_csci %>% ggplot() + geom_histogram(aes(csci), bins = 40) +
  labs(subtitle = "Raw CSCI score for reference gages") +
  theme_bw()

# look at CSCI boxplot single box plot
bmi_csci %>% ggplot() + geom_boxplot(aes(y=csci), show.legend = F, fill="mediumpurple3") +
  labs(subtitle = "Raw CSCI score for reference gages") +
  theme_bw()

# look at CSCI boxplot by year
bmi_csci %>% ggplot() + geom_boxplot(aes(y=csci, x=sampleyear, group=as.factor(sampleyear)), show.legend = F, fill="mediumpurple3") +
  labs(subtitle = "Raw CSCI score for reference gages") +
  theme_bw()

# look at sampling timing
hist(bmi_csci$samplemonth) # majority of months sampled May:Aug
table(bmi_csci$samplemonth)

# look at CSCI percentile by Site Status (not avail for all sites)
bmi_csci %>% filter(!is.na(SiteStatus)) %>% 
  ggplot() + geom_boxplot(aes(x=SiteStatus, y=csci)) + 
  theme_bw()

# function to get data
stat_box_data <- function(y, upper_limit = max(bmi_csci$csci)) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 2), '\n')
    )
  )
}

# plot CSCI
ggplot(data=filter(bmi_csci, !is.na(SiteStatus)), aes(x=SiteStatus, y=csci)) + 
  geom_boxplot(aes(fill=SiteStatus), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text", hjust=1, vjust=0.9) +
  ggdark::dark_theme_bw(base_family = "Roboto Condensed") +
  labs(x="Site Status", y="Raw CSCI Score") + 
  scale_fill_viridis_d()

# function for percentile
stat_box_data <- function(y, upper_limit = max(bmi_csci$csci_percentile)) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 2), '\n')
    )
  )
}

# plot CSCI percentile
ggplot(data=filter(bmi_csci, !is.na(SiteStatus)), aes(x=SiteStatus, y=csci_percentile)) + 
  geom_boxplot(aes(fill=SiteStatus), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text", hjust=1, vjust=0.9) +
  ylim(c(0,1))+
  ggdark::dark_theme_bw(base_family = "Roboto Condensed") +
  labs(x="Site Status", y="CSCI Percentile") + 
  scale_fill_viridis_d()

