

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


# Get BMI comids ----------------------------------------------------------


# all stations us of gage:
bmi_us_coms <- sel_bmi_gages %>% filter(comid %in% mainstems_us$nhdplus_comid)

# all stations 15km downstream on mainstem
bmi_ds_coms <- sel_bmi_gages %>% filter(comid %in% mainstems_ds$nhdplus_comid)


# Make Map ----------------------------------------------------------------

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
  mapview(sel_gages_bmi, zcol="stream_class",  cex=7, layer.name="Selected USGS Gages") + #col.regions="cyan",
  mapview(sel_bmi_gages, col.regions="gray", cex=3, alpha=0.5, layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m4@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# Combine BMI US and DS ---------------------------------------------------

# combine:
bmi_coms <- do.call(what = sf:::rbind.sf,
                    args = list(bmi_ds_coms, bmi_us_coms))
class(bmi_coms)

library(DT)

# list of BMI sites
# bmi_coms %>% 
#   DT::datatable()

# pull BMI sites and get list of data, first join with orig full dataset:
bmi_coms_dat <- left_join(bmi_coms, bmi_clean, by="StationCode") %>% 
  # drop NAs (72 sites: is.na(bmi_coms_dat$SampleID)
  filter(!is.na(SampleID))

# now look at how many unique samples are avail: n=261 unique samples
bmi_coms_dat %>% as.data.frame() %>% group_by(SampleID) %>% distinct(SampleID) %>% tally

# now look at how many unique stations: n=139 stations
bmi_coms_dat %>% as.data.frame() %>% group_by(StationCode) %>% distinct(StationCode) %>% tally


# Check against CSCI Scores -----------------------------------------------

# see what data exist against CSCI scores currently avail (from Raffi)
csci <- read_csv("data/csci_core.csv")

# match against existing sites:
bmi_csci <- inner_join(bmi_coms, csci, by=c("StationCode"="stationcode"))

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


# Get Regions -------------------------------------------------------------



# read in regions from fish bits
dsn_path <- "data/clusters3.gpkg"
st_layers(dsn = dsn_path)
regions <- st_read(dsn=dsn_path, layer = "contiguous_7to10")

# merge by huc_region_group?
region_cv <- regions[regions$huc_region_group == "central_valley", ] %>% 
  st_buffer(0.5) %>% # make a buffer of half a meter around all parts (to avoid slivers)
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(huc_region_group="central_valley") # add data back

region_gb <- regions[regions$huc_region_group == "great_basin", ] %>% 
  st_buffer(0.5) %>% # make a buffer of half a meter around all parts (to avoid slivers)
  st_union() %>% # unite to a geometry object
  st_sf() %>%  # make the geometry a data frame object
  mutate(huc_region_group="great_basin") # add data back

region_nc <- regions[regions$huc_region_group == "north_coast", ] %>% 
  st_buffer(0.5) %>% # make a buffer of half a meter around all parts (to avoid slivers)
  st_union() %>% # unite to a geometry object
  st_sf() %>%  # make the geometry a data frame object
  mutate(huc_region_group="north_coast") # add data back

region_sc <- regions[regions$huc_region_group == "south_coast", ] %>% 
  st_buffer(0.5) %>% # make a buffer of half a meter around all parts (to avoid slivers)
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(huc_region_group="south_coast") # add data back

# bind together
regions_diss <- rbind(region_cv, region_gb, region_nc, region_sc)

save(regions_diss, file = "data_output/regions_ca_fish_groupings.rda")

# double check it worked: 
mapview(regions_diss, zcol="huc_region_group", legend=F) + 
  mapview(bmi_csci, zcol="csci_percentile") + 
  mapview(sel_gages_bmi, col.regions="gray", cex=4, legend=F)

# join flow w csci
bmi_csci_flow <- bmi_csci %>% st_drop_geometry() %>% 
  inner_join(., flow_wide, by="ID")

# how many unique gages do we have flow for?
bmi_csci_flow %>% distinct(ID) %>% tally

# make spatial:
bmi_csci_flow_sf <- bmi_csci_flow %>% st_as_sf(coords=c("lon","lat"), crs=4326)

# select vars
bmi_csci_pcs <- bmi_csci_flow %>% 
  select(StationCode, comid, sampleid, ID, lat, lon, HUC_12, sampledate:fieldreplicate, count, pcnt_ambiguous_individuals:csci_percentile, stream_class, year, YrRange, Avg:Peak_Mag_20) %>% 
  drop_na()

summary(bmi_csci_pcs)


# simple PCA
csci_pr <- prcomp(bmi_csci_pcs[c(14:24,26:61)], center = TRUE, scale = TRUE)
summary(csci_pr)

# Since an eigenvalues <1 would mean that the component actually explains less than a single explanatory variable, discard those

screeplot(csci_pr, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(csci_pr$sdev^2 / sum(csci_pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.579, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)


plot(csci_pr$x[,1],csci_pr$x[,2], xlab="PC1 (23.2%)", ylab = "PC2 (13%)", main = "PC1 / PC2 - plot")


library("factoextra")
fviz_pca_ind(csci_pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = bmi_csci_pcs$csci_percentile, 
             col.ind = "black", 
             #palette = viridis::viridis(36), 
             addEllipses = F,
             label = "var",
             col.var = "black",
             repel = TRUE, show.legend=FALSE,
             legend.title = "CSCI") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))


fviz_pca_ind(csci_pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = bmi_csci_pcs$stream_class, 
             col.ind = "black", 
             palette = viridis::viridis(9), 
             addEllipses = T,
             label = "var",
             col.var = "black",
             repel = TRUE, show.legend=FALSE,
             legend.title = "CSCI") +
  ggtitle("PCA-plot by Stream Class") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")
