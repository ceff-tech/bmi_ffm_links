

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(tidyverse)

# Load Data ---------------------------------------------------------------

load("data_output/sel_bmi_and_gages.rda")
load("data_output/gages_nhd_flowlines_mainstems.rda")
load("data_output/selected_h12_contain_bmi_gage.rda")
load("data_output/bmi_cleaned_all.rda") # all data

# load mapview bases
# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)


# Make Map ----------------------------------------------------------------


# all stations us of gage:
bmi_us_coms <- sel_bmi_gages %>% filter(comid %in% mainstems_us$nhdplus_comid)

# all stations 15km downstream on mainstem
bmi_ds_coms <- sel_bmi_gages %>% filter(comid %in% mainstems_ds$nhdplus_comid)


m3 <- mapview(bmi_ds_coms, cex=6, col.regions="orange", layer.name="Selected BMI D/S") +  
  mapview(mainstems_ds, color="darkblue", cex=3, layer.name="NHD D/S Flowline 15km", legend=F)+
  mapview(bmi_us_coms, cex=6, col.regions="yellow", layer.name="Selected BMI U/S") +  
  mapview(mainstems_us, color="slateblue", cex=3, layer.name="NHD U/S Flowline", legend=F)+
  mapview(sel_gages_bmi, col.regions="cyan", cex=7, layer.name="Selected USGS Gages") + 
  mapview(sel_bmi_gages, col.regions="gray", cex=3, alpha=0.5, layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


m4 <- mapview(bmi_ds_coms, cex=6, zcol="SiteStatus", layer.name="Selected BMI D/S") +  
  mapview(mainstems_ds, color="darkblue", cex=3, layer.name="NHD D/S Flowline 15km", legend=F)+
  mapview(bmi_us_coms, cex=6, zcol="SiteStatus", layer.name="Selected BMI U/S") +  
  mapview(mainstems_us, color="slateblue", cex=3, layer.name="NHD U/S Flowline", legend=F)+
  mapview(sel_gages_bmi, col.regions="cyan", cex=7, layer.name="Selected USGS Gages") + 
  mapview(sel_bmi_gages, col.regions="gray", cex=3, alpha=0.5, layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m4@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# Get BMI Sites -----------------------------------------------------------

library(DT)

# list of BMI sites
bmi_ds_coms %>% 
  DT::datatable()

# list of Gage sites
bmi_us_coms %>% 
  DT::datatable()

# combine:
bmi_coms <- do.call(what = sf:::rbind.sf,
                    args = list(bmi_ds_coms, bmi_us_coms))
class(bmi_coms)

# Check against CSCI Scores -----------------------------------------------

csci <- read_csv("data/csci_core.csv")

# match against existing sites:
bmi_csci <- left_join(bmi_coms, csci, by=c("StationCode"="stationcode")) %>% 
  filter(!is.na(csci))

# how many unique matches?
length(unique(bmi_csci$StationCode))

table(bmi_csci$SiteStatus)

ggplot() + geom_boxplot(data=filter(bmi_csci, !is.na(SiteStatus)), aes(x=SiteStatus, y=csci_percentile))


# function to get data
stat_box_data <- function(y, upper_limit = max(bmi_csci$csci_percentile) * 1.15) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}


ggplot(data=filter(bmi_csci, !is.na(SiteStatus)), aes(x=SiteStatus, y=csci)) + 
  geom_boxplot() +
  stat_summary(fun.data=stat_box_data, geom="text", hjust=0.5, vjust=0.9) +
  theme_bw()
