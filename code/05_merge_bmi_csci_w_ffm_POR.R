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

# FISH REGIONS
ca_sp_regions <- read_sf("data/spatial/umbrella_sp_regions.shp", as_tibble = T)

# nhd streamlines
load("data_output/03_selected_nhd_mainstems_gages.rda") # mainstems_all

# get all functional flow metric data (percentiles, alt status, ffmetrics)
load("data_output/02_usgs_all_ffm_data.rda")

# Set Basemaps ------------------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Make BMI POR FF Dataset -----------------------------------------------

# make gage_id as character for join:
sel_bmi_coms_final_v2 <- sel_bmi_coms_final_v2 %>% 
  mutate(gage_id_c = gsub("^T", "", ID))

sel_bmi_coms_final_trimmed <- sel_bmi_coms_final_v2 %>% 
  mutate(gage_id_c = gsub("^T", "", ID)) %>% 
  separate(SampleID, into=c("site", "sampledate"), sep = "_", remove = FALSE) %>% 
  mutate(sampledate = lubridate::mdy(sampledate)) %>% 
  mutate(sampledate = if_else(is.na(sampledate), lubridate::mdy("06282009"), sampledate)) %>% 
  select(-site) %>% 
  filter(lubridate::month(sampledate)>4, lubridate::month(sampledate)<10)

# check stations match for trimmed data (n=300):
bmi_coms_dat_trim %>% st_drop_geometry() %>% distinct(SampleID, ID) %>% dim()
bmi_coms_dat %>% st_drop_geometry() %>% distinct(SampleID, ID) %>% dim() # this should be 349

# join together selected csci data with ffm alteration status data (all data not trimmed)
bmi_csci_por <-  inner_join(sel_bmi_coms_final_v2, g_all_alt,
                            #by=c("comid")) #%>% # n=2688
                            #by=c("comid", "gage_id_c"="gage_id")) # %>% # n=1550
                            # since only want observed data at USGS gage:
                            by=c("gage_id_c"="gage_id")) %>%   # n=7719
  distinct(SampleID, metric, gage_id, .keep_all=TRUE) %>% 
  rename(comid_bmi = comid.x, comid_ffc = comid.y) # n=7337


# join together selected csci data with ffm alteration status data (this is the "untrimmed" dataset)
bmi_csci_por <-  inner_join(sel_bmi_coms_final_v2, g_all_alt,
                            #by=c("comid")) #%>% # n=2688
                            #by=c("comid", "gage_id_c"="gage_id")) # %>% # n=1550
                            # since only want observed data at USGS gage:
                            by=c("gage_id_c"="gage_id")) %>%   # n=7719
  distinct(SampleID, metric, gage_id, .keep_all=TRUE) %>% 
  rename(comid_bmi = comid.x, comid_ffc = comid.y) # n=7337

# now trimmed data
bmi_csci_por_trim <-  inner_join(sel_bmi_coms_final_trimmed, g_all_alt,
                                 by=c("gage_id_c"="gage_id")) %>%   # n=7719
  distinct(SampleID, metric, gage_id, .keep_all=TRUE) %>% 
  rename(comid_bmi = comid.x, comid_ffc = comid.y) # n=6242

# see how many distinct sites
length(unique(bmi_csci_por_trim$gage_id_c)) #Gages (n=154), trimmed = 137
length(unique(bmi_csci_por_trim$StationCode)) # BMI Stations (n=267), trimmed = 225

# how many of each gage type
bmi_csci_por_trim %>% st_drop_geometry() %>% 
  dplyr::distinct(ID, .keep_all=TRUE) %>% 
  group_by(CEFF_type) %>%  tally() # ALT = 100, REF = 37

bmi_csci_por %>% st_drop_geometry() %>% 
  dplyr::distinct(ID, .keep_all=TRUE) %>% 
  group_by(CEFF_type) %>%  tally() # ALT = 116, REF = 38

# and originally? : so we lost 6 ref sites :(
sel_bmi_coms_final_v2 %>% st_drop_geometry() %>% 
  dplyr::distinct(ID, .keep_all=TRUE) %>% 
  group_by(CEFF_type) %>%  tally() # ALT = 116, REF = 44

# Visualize ---------------------------------------------------------------

library(ggthemes)

hist(month(bmi_csci_por_trim$sampledate))

# function to get data
stat_box_data <- function(y, upper_limit = max(bmi_csci_por_trim$csci, na.rm = TRUE)) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}


# plot CSCI w/ NAs
ggplot(data=bmi_csci_por_trim %>% filter(status!="not_enough_data"), aes(x=CEFF_type, y=csci)) + 
  geom_boxplot(aes(fill=status), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text", cex=3, hjust=1, vjust=0.9) +
  labs(y="CSCI", x="CEFF Gage Type", subtitle="CSCI Score by FFC Alteration Status")+
  theme_bw(base_family = "Roboto Condensed") + facet_grid(.~status) +
  scale_fill_colorblind()
#ggsave(filename = "figs/05_csci_scores_by_alteration_status_ceff_type.png", height = 8, width = 11, units = "in",dpi=300)

# Add HUC Regions --------------------------------------------------

# check crs:
st_crs(bmi_csci_por_trim)
st_crs(bmi_csci_por)
st_crs(ca_sp_regions)

ca_sp_regions <- ca_sp_regions %>% st_transform(4326)

# join with regions and add huc_region, make sure both df are in 4326
bmi_csci_por_trim <- st_join(bmi_csci_por_trim, left = TRUE, ca_sp_regions["huc_region"])
bmi_csci_por <- st_join(bmi_csci_por, left = TRUE, ca_sp_regions["huc_region"])

# make a simpler layer for just editing:
bmi_csci_sites <- bmi_csci_por %>%
  dplyr::distinct(StationCode, ID, .keep_all = TRUE)
length(unique(bmi_csci_sites$StationCode))

bmi_csci_sites_trim <- bmi_csci_por_trim %>%
  dplyr::distinct(StationCode, ID, .keep_all = TRUE)
length(unique(bmi_csci_sites_trim$StationCode))

# view and update w mapedit
mapview(bmi_csci_sites, col.regions="orange") + 
  mapview(bmi_csci_sites_trim, col.regions="green", cex=2.5) +
  mapview(ca_sp_regions)

library(mapedit)
library(leafpm)
library(leaflet)

## use this to select features (returns a list of stationcodes)
# selectMap(
#   leaflet() %>%
#     addTiles() %>%
#     addPolygons(data=ca_sp_regions, layerId = ~huc_region, color = "orange") %>%
#     addCircleMarkers(data = bmi_csci_sites, layerId = ~StationCode)
#   )

## sites to add to central valley
cvalley_add <- c("514FC1278", "514RCR001", "534DCC167")

## sites to add to great_basin
gbasin_add <- c("603MAM004", "630PS0005")

## sites to add to southcoast
scoast_add <- c("628PS1307","628PS1179","719MISSCK","719TRMDSS","719FCA001")

# Amargosa site is "609PS0053" = mojave?

# so 294 NA's in each
summary(as.factor(bmi_csci_por$huc_region))
summary(as.factor(bmi_csci_por_trim$huc_region))

# use case_when to replace
bmi_csci_por_trim <- bmi_csci_por_trim %>%
  mutate(huc_region = case_when(
    StationCode %in% cvalley_add ~ "central_valley",
    StationCode %in% gbasin_add ~ "great_basin",
    StationCode %in% scoast_add ~ "south_coast",
    TRUE ~ huc_region))

bmi_csci_por <- bmi_csci_por %>%
  mutate(huc_region = case_when(
    StationCode %in% cvalley_add ~ "central_valley",
    StationCode %in% gbasin_add ~ "great_basin",
    StationCode %in% scoast_add ~ "south_coast",
    TRUE ~ huc_region))

# only NAs are now for AMARGOSA site
summary(as.factor(bmi_csci_por_trim$huc_region))
table(bmi_csci_por_trim$huc_region)

## map and double check:
 mapview(bmi_csci_por_trim, zcol="huc_region", layer.name="Selected Sites", viewer.suppress=FALSE) +
   mapview(ca_sp_regions, zcol="huc_region", layer.name="HUC Regions", alpha.regions=0.1)

# Make GAGE/BMI geoms -----------------------------------------------------
 
# make SF geometry fields for BMI
bmi_csci_por_bmi <- bmi_csci_por %>% 
 rename("geom_bmi"=geometry) 
 
# make a USGS geom field
 bmi_csci_por_usgs <- bmi_csci_por %>% st_drop_geometry() %>% 
   st_as_sf(., coords=c("LONGITUDE","LATITUDE"), crs = 4326, remove=FALSE) %>% 
   # rename the geometry col
   rename("geom_usgs"=geometry)
 
 # quick view
 mapview(bmi_csci_por_bmi, cex=7, col.regions="orange", 
         layer.name="Selected BMI comids") +
   mapview(bmi_csci_por_usgs, col.regions="skyblue", cex=4, color="blue2", layer.name="Selected USGS Gages") +
   mapview(mainstems_all, color="steelblue", cex=3, 
           layer.name="NHD Flowlines")

# Export Cleaned Data -----------------------------------------------------

# save the bmi_csci_por
write_rds(bmi_csci_por, path = "data_output/05_selected_bmi_stations_w_csci_ffm_alt_por.rds")
write_rds(bmi_csci_por_trim, path = "data_output/05_selected_bmi_stations_w_csci_ffm_alt_por_trim.rds")

# sf specific files for usgs & bmi
save(bmi_csci_por_bmi, bmi_csci_por_usgs, file="data_output/05_selected_bmi_csci_por_sf.rda")

save(bmi_csci_por_trim, file = "data_output/05_selected_bmi_csci_por_trim_w_huc_region.rda")
save(bmi_csci_por, file = "data_output/05_selected_bmi_csci_por_w_huc_region.rda")
