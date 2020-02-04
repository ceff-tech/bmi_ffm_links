# 03 Generate Final Selected Sites/Data
## R. Peek
## Look at final output


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
library(lubridate)

# Load Data: REFERENCE GAGE SET -------------------------------------------

load("data_output/00_bmi_cleaned_all.rda") # bmi_clean
load("data_output/01_bmi_stations_distinct_status.rda") # bmi_clean_stations_ss (site status)
sel_bmi_gages <- readRDS("data_output/02_selected_bmi_h12_all_gages.rds") # sel_bmi_gages
sel_gages_bmi <- readRDS("data_output/02_selected_usgs_h12_all_gages.rds") # sel_gages_bmi
load("data_output/02_selected_nhd_mainstems_all_gages.rda") # mainstems_all
sel_h12 <- read_rds("data_output/02_selected_h12_all_gages.rds") # all h12s w bmi and gage: sel_h12_bmi

load("data_output/02_selected_final_bmi_stations_dat_all_gages.rda") # bmi_coms_dat (all data for selected), bmi_coms_final (just coms and id)

bmi_coms <- read_rds("data_output/02_bmi_all_stations_comids.rds") # just bmi_coms, comids for all BMI sites

# re order cols
bmi_coms_final <- bmi_coms_final %>% select(StationCode, longitude, latitude, HUC_12, h12_area_sqkm, ID:comid2, geometry)

# make a new layer of "unselected" bmi sites
bmi_not_selected <- sel_bmi_gages %>% filter(!as.character(comid) %in% mainstems_all$nhdplus_comid) # should be 591 = (2188 total -  1597 selected)

# first add site status
bmi_coms_final <- left_join(bmi_coms_final, bmi_stations_distinct_status[,c(1:2)], by="StationCode") %>% 
  # filter for distinct
  distinct(StationCode, ID, .keep_all = TRUE) %>% 
  select(StationCode:comid2,SiteStatus,geometry)

# how many missing ss? 1000 don't have site status
bmi_coms_final %>% st_drop_geometry %>% group_by(SiteStatus) %>% tally

#write_csv(missing_site_status, path = "data_output/bmi_missing_site_status.csv")

# Set up Mapview Basemap --------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)


# Mapdeck Map -------------------------------------------------------------

# mapview breaks but mapdeck WORKS
library(mapdeck)
set_token(Sys.getenv("MAPBOX_TOKEN"))

mapdeck(
  style=mapdeck_style("dark")
) %>% 
  add_path(data = mainstems_all, stroke_colour = "gageID", tooltip="nhdplus_comid", auto_highlight = TRUE) %>% 
  add_sf(data = st_transform(bmi_coms_final, 4326), 
         fill_colour="#EE7600", tooltip="StationCode", 
         layer_id="BMI Comids", radius=500) %>% 
  add_sf(data = st_transform(sel_gages_bmi, 4326), fill_colour="#00EEEE", radius=300, tooltip="site_id",
         layer_id="USGS Gages")


# Make Mapview of Selected Gages and BMI Stations ----------------------

# this map of all sites selected U/S and D/S
m3 <- mapview(bmi_coms_final, zcol="SiteStatus", cex=6, col.regions=c("orange","maroon"), layer.name="Final BMI Sites") +  
  mapview(mainstems_all, color="darkblue", cex=3, lwd=4, layer.name="NHD Flowline 10km", legend=F)+
  mapview(sel_gages_bmi, col.regions="cyan", cex=7, layer.name="Reference USGS Gages") + 
  mapview(bmi_not_selected, col.regions="gray", cex=3.2, alpha=0.5, layer.name="Other BMI Sites in H12") +
  mapview(sel_h12, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

m3

# View Final Tally --------------------------------------------------------

# any NA's?
bmi_coms_dat %>% st_drop_geometry %>% filter(is.na(StationCode)) # nope

# now look at how many unique samples are avail: n=1464 unique samples
bmi_coms_dat %>% st_drop_geometry %>% distinct(SampleID) %>% tally

# now look at how many unique stations: n=771 stations
bmi_coms_dat %>% st_drop_geometry %>% distinct(StationCode) %>% tally

# how many unique USGS gages? n=512
bmi_coms_dat %>% st_drop_geometry %>% distinct(ID) %>% tally

# total distinct stations 2931
bmi_coms %>% distinct(StationCode) %>% tally()

# Check against CSCI Scores -----------------------------------------------

## LEFT OFF HERE
##------ Tue Feb  4 10:44:38 2020 ------##

# see what data exist against CSCI scores currently avail (from Raffi)
csci <- read_csv("data/csci_core.csv")

# match against existing sites:
bmi_csci <- inner_join(bmi_coms_final, csci, by=c("StationCode"="stationcode"))

bmi_csci <- left_join(bmi_csci, bmi_clean_stations_ss[,c(1:2)], by="StationCode")


# map
mapview(bmi_coms_final, color="orange", col.regions="gray", alpha.regions=0.1,
        layer.name="Selected BMI Sites") +
mapview(bmi_csci, col.regions="mediumpurple2", cex=1, layer.name="Selected BMI w CSCI") + 
  mapview(sel_gages_bmi, col.regions="dodgerblue", cex=2.5, alpha.regions=0.7, 
          layer.name="USGS gages")

# how many unique matches?
length(unique(bmi_csci$StationCode))
table(bmi_csci$SiteStatus)

# look at CSCI histogram of all scores
(bmi_csci %>% ggplot() + geom_histogram(aes(csci), bins = 40) +
  labs(subtitle = "Raw CSCI score for reference gages") +
  theme_bw() -> gg_csci_hist)

# look at CSCI boxplot single box plot
bmi_csci %>% ggplot() + geom_boxplot(aes(y=csci), show.legend = F, fill="mediumpurple3") +
  labs(subtitle = "Raw CSCI score for reference gages") +
  theme_bw()

# look at CSCI boxplot by year
(bmi_csci %>% ggplot() + geom_boxplot(aes(y=csci, x=sampleyear, group=as.factor(sampleyear)), show.legend = F, fill="mediumpurple3", color="gray30") +
  labs(subtitle = "Raw CSCI score for reference gages") +
  theme_bw() -> gg_bmi_box_yr)

# look at sampling timing
hist(bmi_csci$samplemonth) # majority of months sampled May:Aug
table(bmi_csci$samplemonth)

# look at CSCI percentile by Site Status (not avail for all sites)

# function to get data
stat_box_data <- function(y, upper_limit = max(bmi_csci$csci, na.rm = T)) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y, na.rm = T), 2), '\n')
    )
  )
}

# plot CSCI
(gg_csci_ss <- ggplot(data=filter(bmi_csci, !is.na(SiteStatus)), aes(x=SiteStatus, y=csci)) + 
  geom_violin(aes(fill=SiteStatus), color="transparent", alpha=0.8, show.legend = F)+
  geom_boxplot(fill="gray40", show.legend = F, outlier.alpha=0, width=.1) +
  stat_summary(fun.data=stat_box_data, geom="text", hjust=1, vjust=0.9) +
  ggdark::dark_theme_bw(base_family = "Roboto Condensed") +
  labs(x="Site Status", y="Raw CSCI Score") + 
  scale_fill_viridis_d())

# function for percentile
stat_box_data <- function(y, upper_limit = max(bmi_csci$csci_percentile, na.rm=T)) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y, na.rm=T), 2), '\n')
    )
  )
}

# plot CSCI percentile
(gg_csci_ss_prcnt <- ggplot(data=filter(bmi_csci, !is.na(SiteStatus)), 
                        aes(x=SiteStatus, y=csci_percentile)) + 
  geom_violin(aes(fill=SiteStatus), color="transparent", alpha=0.8, show.legend = F)+
  geom_boxplot(fill="gray40", show.legend = F, outlier.alpha=0, coef=0, width=.1) +
  stat_summary(fun.data=stat_box_data, geom="text", hjust=1, vjust=0.9) +
  ylim(c(0,1))+
  ggdark::dark_theme_bw(base_family = "Roboto Condensed") +
  labs(x="Site Status", y="CSCI Percentile") + 
  scale_fill_viridis_d())

library(patchwork)

# horiz
gg_csci_ss + gg_csci_ss_prcnt

# vert
gg_csci_ss + gg_csci_ss_prcnt + plot_layout(ncol=1)

# nested layout
(gg_csci_ss / gg_csci_ss_prcnt) - (gg_csci_hist / gg_bmi_box_yr)

ggsave(filename = "figs/04b_bmi_ref_sites_csci_summaries_all.png", width = 11, height = 8.5, dpi=300, units = "in")
