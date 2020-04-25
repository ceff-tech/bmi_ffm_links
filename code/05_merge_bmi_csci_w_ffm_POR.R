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

# join together selected csci data with ffm alteration status data
bmi_csci_por <-  inner_join(sel_bmi_coms_final_v2, g_all_alt,
                            #by=c("comid")) #%>% # n=2688
                            #by=c("comid", "gage_id_c"="gage_id")) # %>% # n=1550
                            # since only want observed data at USGS gage:
                            by=c("gage_id_c"="gage_id")) %>%   # n=7719
  distinct(SampleID, metric, gage_id, .keep_all=TRUE) %>% 
  rename(comid_bmi = comid.x, comid_ffc = comid.y) # n=7337

# see how many distinct sites
length(unique(bmi_csci_por$gage_id_c)) #Gages (n=154)
length(unique(bmi_csci_por$StationCode)) # BMI Stations (n=267)

# how many of each gage type
bmi_csci_por %>% st_drop_geometry() %>% 
  dplyr::distinct(ID, .keep_all=TRUE) %>% 
  group_by(CEFF_type) %>%  tally() # ALT = 116, REF = 38

# and originally? : so we lost 6 ref sites :(
sel_bmi_coms_final_v2 %>% st_drop_geometry() %>% 
  dplyr::distinct(ID, .keep_all=TRUE) %>% 
  group_by(CEFF_type) %>%  tally() # ALT = 116, REF = 44

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
mapview(bmi_csci_por_sf, cex=7, col.regions="orange", 
        layer.name="Selected BMI comids") +
  mapview(bmi_csci_por_usgs, col.regions="skyblue", cex=4, color="blue2", layer.name="Selected USGS Gages") +
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines")


# Visualize ---------------------------------------------------------------

library(ggthemes)

# separate by month and drop everything before/after May-Sep
bmi_csci_por_trim <- bmi_csci_por %>% 
  tidyr::separate(., col=SampleID, into = c("Site","YMD"), sep="_", remove=FALSE) %>% 
  select(-Site) %>% 
  # fix the one site that formats improperly (SMCR8_327_06282009_BMI_RWB_1)
  mutate(YMD = case_when(
    grepl("SMCR8_327", StationCode) ~ "06282009",
    TRUE ~ YMD),
    # now format as date
    YMD=mdy(YMD),
         MM = month(YMD)) %>% 
  filter(MM>4, MM<11)

hist(bmi_csci_por_trim$MM)

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
ggsave(filename = "figs/05_csci_scores_by_alteration_status_ceff_type.png", height = 8, width = 11, units = "in",dpi=300)

# Export Cleaned Data -----------------------------------------------------

# save the bmi_csci_por
write_rds(bmi_csci_por, path = "data_output/05_selected_bmi_stations_w_csci_ffm_alt_por.rds")

write_rds(bmi_csci_por_trim, path = "data_output/05_selected_bmi_stations_w_csci_ffm_alt_por_trim.rds")

save(bmi_csci_por_trim, bmi_csci_por_bmi, bmi_csci_por_usgs, file="data_output/05_selected_bmi_csci_por_and_sf.rda")

save(g_all_alt, g_all_ffc, file = "data_output/05_all_alt_ffc.rda")

