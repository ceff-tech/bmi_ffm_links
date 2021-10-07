## Generate Final Selected Sites/Data
## R. Peek

# add stream class final map too

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(sf)
library(glue)
library(mapview)
library(lubridate)
library(tmap)
library(tmaptools)

# Load Data ---------------------------------------------------------------

# bio_ffm: revised ffm-asci-csci data (all sites)
bio_ffm<- read_rds("https://github.com/ryanpeek/flow_seasonality/blob/main/output/10_ffc_filtered_final_combined_rev.rds?raw=true")

# Selected Sites: with BMI data, this may all be dated
# load("data_output/02c_selected_final_bmi_dat_all.rda")
# bmi_final_dat (all data) (distinct 877 usgs_bmi sites)
# bmi_not_selected: all sites not selected from BMI dataset
# gage_selected_v2 and not selected

# Streamlines
load("data_output/02b_sel_gage_mainstems_all.rda")

# get distinct segs that are DS only (either DD or DS)
mainstems_distinct <- mainstems_all %>% 
  filter(from_gage %in% c("DS", "DD")) %>% 
  distinct(nhdplus_comid, .keep_all=TRUE)


# Stream Class Data -------------------------------------------------------
load("data_output/stream_class3_all_ca_w_comid.rda")
# # STREAMCLASS (FULL)
# stream_class <- st_read("~/Downloads/stream_class_shapefile_updated/Final_Classification_9CLASS_curated.shp")
# 
# # XWALK
# # crosswalk
# strmclass_xwalk <- tibble(
#   "CLASS"=as.character(c(1,2,3,4,5,6,7,8,9)), 
#   "CLASS_NAME"=c("snowmelt", # 3 class: 1=SNOWMELT
#                  "high-volume snowmelt and rain", # 3 class: 2=MIXED
#                  "low-volume snowmelt and rain", # 3 class: 2=MIXED,
#                  "winter storms", # 3 class: 3=RAIN
#                  "groundwater", # 3 class: 2=MIXED
#                  "perennial groundwater and rain", # 3 class: 3=RAIN
#                  "flashy, ephemeral rain", # 3 class: 3=RAIN
#                  "rain and seasonal groundwater", # 3 class: 3=RAIN
#                  "high elevation low precipitation"), # 3 class: 1=SNOWMELT
#   "class3_name" = c("SNOWMELT",
#                     "MIXED","MIXED","RAIN","MIXED",
#                     "RAIN","RAIN","RAIN",
#                     "SNOWMELT"),
#   "class3_id" = c(1,
#                   2,2,3,2,
#                   3,3,3,
#                   1))
# 
# stream_class_3 <- left_join(stream_class, strmclass_xwalk)
# # mapview(stream_class_3, zcol="class3_name")
# # save it out
# save(stream_class_3, file = "data_output/stream_class3_all_ca_w_comid.rda")

# ASCI --------------------------------------------------------------------

load(url("https://github.com/ksirving/asci_ffm_2019/blob/master/output_data/05_algae_asci_por_trim_ecoreg.rda?raw=true")) 
# write_rds(algae_asci_por_trim_ecoreg, file = "data_output/03_algae_asci_por_trim_ecoreg.rds")
#asci <- read_rds("data_output/03_algae_asci_por_trim_ecoreg.rds")

# algae_stations_distinct
load("data_output/01a_algae_stations_distinct.rdata") 
asci_all <- algae_stations_distinct

# Set up Mapview Basemap --------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Make Mapview of Selected Gages and BMI Stations ----------------------

# this map of all sites selected U/S and D/S
mapviewOptions(fgb = FALSE)

# m3 <- mapview(bmi_final_dat, cex=6, col.regions="orange", 
#               layer.name="Selected BMI CSCI") +  
#   mapview(mainstems_all %>% filter(from_gage=="UM"), color="forestgreen", cex=3, 
#           layer.name="NHD Flowlines US") +
#   mapview(mainstems_distinct, color="steelblue", cex=3, 
#           layer.name="NHD Flowlines DS") +
#   mapview(gages_selected_v2, col.regions="skyblue", cex=7, color="blue2",
#           layer.name="Selected USGS Gages") + 
#   # these are all bmi or gages in same H12 but not selected
#   mapview(gages_not_selected_v2, col.regions="slateblue", color="gray20",
#           cex=3.2, layer.name="Other USGS Gages") + 
#   mapview(bmi_not_selected_v2, col.regions="gold", color="gray20", cex=3.2, 
#           layer.name="Other BMI Sites in H12") + 
#   mapview(hucs_selected_v2, col.regions="orange3", alpha.region=0.1, 
#           color="orange", legend=F, layer.name="Selected HUC12") +
#   mapview(hucs_not_selected_v2, col.regions="dodgerblue", alpha.region=0.1, 
#           color="darkblue", legend=F, layer.name="Other HUC12")
# 
# 
# m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save out
#mapshot(m3, url = paste0(here::here(),"/figs/03_map_of_final_bmi_csci_sites.html"))


# TMAP StreamClass  ---------------------------------------------------------------

library(tmap)
library(USAboundaries)
ca<-us_counties(states="ca")
load("data/spatial/major_rivers_dissolved.rda")

# crop rivers to CA only and filter to rivers only
rivs_ca <- st_intersection(rivs, ca) %>% 
  filter(FEATURE_TYPE == "river")


# first make CA map with no border
(map_ca <- tm_shape(ca) + tm_polygons(border.alpha = 0.3) +
  tm_shape(rivs_ca) + tm_lines(col="darkblue", lwd = .7, alpha = 0.8) +
  tm_layout(frame=FALSE))

#  CA with revised streamclass
(map_ecoca <- tm_shape(ca) + tm_polygons(border.alpha = 0.3) +
    tm_layout(frame=FALSE) +
    tm_shape(stream_class_3) + 
    tm_lines(col = "class3_name", alpha = 0.7, palette="viridis",
             title.col = "Stream Class") +
    tm_shape(rivs_ca) + tm_lines(col="darkblue", lwd = .5, alpha = 0.4) +
    tm_layout(legend.width = 0.45, fontfamily = "Helvetica") +
    tm_compass(type = "arrow", size = 2,
               position = c(0.1,0.18)) +
    #position = c(0.15, 0.2)) +
    tm_scale_bar(breaks = c(0, 100, 200), 
                 text.size = 0.6,
                 position = c(0.12, 0.1))
)

tmap::tmap_save(tm = map_ecoca, 
                filename = "figs/03_tmap_streamclass3.png", width = 8, height = 11, units = "in", dpi = 300)

# TMAP CSCI stations ---------------------------------------------------------

# get ALL bug data (distinct stations)      
load("data_output/01_bmi_stations_distinct.rda")

# make a tmap
(map_bmi <- map_ca +
    tm_shape(bmi_stations_distinct) +
    tm_dots(col="chocolate3", shape=21, size=0.5, alpha=0.8, 
            border.col="black",  border.alpha=0.8, 
            legend.show = TRUE) +
    tm_add_legend(type = "symbol", title="Stations",
                  labels = c("CSCI"),
                  col = c("chocolate3"),
                  shape=c(21), size = 1.1) +
    tm_compass(type = "arrow", size = 2,
               position = c(0.1,0.18)) +
    tm_scale_bar(breaks = c(0, 100, 200), 
                 text.size = 0.6,
                 position = c(0.12, 0.1)) +
    tm_layout(
      #title = glue("Sampling Locations\n (CSCI: n={nrow(bmi_stations_distinct)})"), 
      #legend.show = FALSE, 
      legend.position = c(0.65,0.7),
      frame = FALSE, title.size = 0.8,
      legend.outside = FALSE, attr.outside = FALSE,
      inner.margins = 0.01, outer.margins = (0.01),
      fontfamily = "Roboto Condensed", title.position = c(0.65, 0.7)))

# tmap::tmap_save(tm = map_bmi, 
#                 filename = "figs/03_tmap_csci_sites_all.png", width = 8, height = 11, units = "in", dpi = 300)  

# TMAP ASCI stations ---------------------------------------------------------

# make algae sf
algae_distinct <- algae_stations_distinct %>% 
  filter(!is.na(Latitude)) %>% 
  st_as_sf(coords=c("Longitude","Latitude"), remove=FALSE, crs=4326)

# make a tmap
(map_asci <- map_ca +
    tm_shape(algae_distinct) +
    tm_dots(col = "cornsilk", shape = 23, size = 0.5, alpha=.9, 
            border.alpha=0.8, legend.show = TRUE) + 
    
    tm_add_legend(type = "symbol", title="Stations",
                  labels = c("ASCI"),
                  col = c("cornsilk"),
                  shape=c(23), size = 1.1) +
    
    tm_compass(type = "arrow", size = 2,
               position = c(0.1,0.18)) +
    tm_scale_bar(breaks = c(0, 100, 200), 
                 text.size = 0.6,
                 position = c(0.12, 0.1)) +
    tm_layout(
      #title = glue("Sampling Locations\n (ASCI: n={nrow(algae_stations_distinct)})"), 
      legend.position = c(0.65,0.7),
      frame = FALSE, title.size = 0.8,
      legend.outside = FALSE, attr.outside = FALSE,
      inner.margins = 0.01, outer.margins = (0.01),
      fontfamily = "Roboto Condensed", title.position = c(0.65, 0.7)))


# asci only
# tmap::tmap_save(tm = map_asci, 
#                 filename = "figs/03_tmap_asci_sites_all.png", width = 8, height = 11, units = "in", dpi = 300)  

# TMAP ASCI and CSCI distinct stations ---------------------------------------

# make a tmap
(map_bioall <- map_ca +
   # tm_shape(stream_class_3) + 
   # tm_lines(col = "class3_name", alpha = 0.7, palette="viridis",
   #          title.col = "Stream Class") +
   # CSCI
   tm_shape(bmi_stations_distinct) +
   tm_dots(col="chocolate3", shape=21, size=0.3, alpha=0.8, 
           border.col="black",  border.alpha=0.8, 
           legend.show = TRUE) +
   # ASCI
   tm_shape(algae_distinct) +
   tm_dots(col = "cornsilk", shape = 23, size = 0.15, alpha=.8, 
           border.alpha=0.8, legend.show = TRUE) + 
   tm_add_legend(type = "symbol", title="Stations",
                 labels = c("CSCI", "ASCI"),
                 col = c("chocolate3", "cornsilk"),
                 shape=c(21, 23), size = 1.1) +
   tm_compass(type = "arrow", size = 2,
              position = c(0.1,0.18)) +
   tm_scale_bar(breaks = c(0, 100, 200), 
                text.size = 0.6,
                position = c(0.12, 0.1)) +
   tm_layout(
     #title = glue("Sampling Locations\n (ASCI: n={nrow(algae_stations_distinct)}\n  CSCI: n={nrow(bmi_stations_distinct)})"), 
     frame = FALSE,  legend.outside = FALSE, 
     legend.title.size = 1.3,
     legend.text.size = 1,
     attr.outside = FALSE, inner.margins = 0.01, 
     outer.margins = (0.01), fontfamily = "Roboto Condensed", 
     legend.position = c(0.65, 0.65)))


# FIRST TRYPTYCH
tmap::tmap_save(tm = map_bioall, 
                filename = "figs/fig2_tmap_all_bio_sites.png", width = 8, height = 11, units = "in", dpi = 300)  
tmap::tmap_save(tm = map_bioall, 
                filename = "figs/fig2_tmap_all_bio_sites.jpg", width = 8, height = 11, units = "in", dpi = 300)  

# TMAP Selected ASCI-CSCI stations w gages --------------------------------------------------------

# asci (N=233)
asci_sites <- bio_ffm %>% filter(bioindicator=="ASCI") %>% 
  select(gageid:csci) %>% distinct(.keep_all=TRUE) %>% 
  left_join(., algae_stations_distinct) %>% 
  distinct(StationCode, .keep_all=TRUE) %>% 
  st_as_sf(coords=c("Longitude","Latitude"), crs=4269, remove=FALSE)

# csci (N=231)
csci_sites <- bio_ffm %>% filter(bioindicator=="CSCI") %>% 
  select(gageid:csci) %>% distinct(.keep_all=TRUE) %>% 
  left_join(., bmi_stations_distinct) %>% 
  distinct(StationCode, .keep_all=TRUE) %>% 
  st_as_sf(coords=c("longitude","latitude"), crs=4269, remove=FALSE)

# gages (N=222)
gages_sites <- bio_ffm %>% 
  select(gageid:csci) %>% distinct(gageid, .keep_all=TRUE) %>% 
  st_as_sf(coords=c("usgs_lon","usgs_lat"), crs=4269, remove=FALSE)


# make a tmap
(map_bioselect <- map_ca +
    tm_shape(csci_sites) +
    tm_dots(col="chocolate3", shape=21, size=1, alpha=0.9, 
            border.col="black",  border.alpha=0.8, legend.show = TRUE) +
    tm_shape(asci_sites) +
    tm_dots(col = "cornsilk", shape = 23, size = 0.8, alpha=.9, 
            border.alpha=0.8, legend.show = TRUE) + 

    # tm_add_legend(type = "symbol", title="Stations",
    #               labels = c("CSCI (n=231)","ASCI (n=233)"),
    #               col = c("chocolate3", "cornsilk"),
    #               shape=c(23,21), size = 1.1) +

    # legend w gages
    tm_shape(gages_sites) +
    tm_dots(col="cyan3", shape=22, size=0.3, alpha=1) +

    tm_add_legend(type = "symbol", title="Stations",
                  labels = c("CSCI (n=231)","ASCI (n=233)", "Gages (n=222)"),
                  col = c("chocolate3", "cornsilk", "cyan3"),
                  shape=c(21,23,22), size = 1.1) +
    
    # tm_compass(type = "arrow", size = 2,
    #            position = c(0.1,0.18)) +
    # tm_scale_bar(breaks = c(0, 100, 200), 
    #              text.size = 0.6,
    #              position = c(0.12, 0.1)) +
    tm_layout(frame = FALSE, 
              legend.title.size = 1.3,
              legend.text.size = 1,
              legend.position = c(0.65,0.65),
              legend.outside = FALSE, attr.outside = FALSE,
              inner.margins = 0.01, outer.margins = (0.01),
              fontfamily = "Roboto Condensed"))


# Selected
# tmap::tmap_save(tm = map_bioselect, 
#                 filename = "figs/03_tmap_selected_bio_sites_combined.png", width = 8, height = 11, units = "in", dpi = 300)  

# tmap::tmap_save(tm = map_bioselect, 
#                 filename = "figs/03_tmap_selected_bio_sites_combined_wgages.png", width = 8, height = 11, units = "in", dpi = 300)  


# BIO + STREAM CLASS MAP --------------------------------------------------

(map_strmclass_bio <- map_ca +
   tm_shape(stream_class_3) +
   tm_lines(col = "class3_name", alpha = 0.7, palette="viridis",
            title.col = "Stream Class") +
   tm_shape(csci_sites) +
   tm_dots(col="chocolate3", shape=21, size=1, alpha=0.9, 
           border.col="black",  border.alpha=0.8, legend.show = TRUE) +
   tm_shape(asci_sites) +
   tm_dots(col = "cornsilk", shape = 23, size = 0.8, alpha=.9, 
           border.alpha=0.8, legend.show = TRUE) + 
   
   tm_add_legend(type = "symbol", title="Stations",
                 labels = c("CSCI (n=231)","ASCI (n=233)"),
                 col = c("chocolate3", "cornsilk"),
                 shape=c(21,23), size = 1.1) +
   
   # legend w gages
   # tm_shape(gages_sites) +
   # tm_dots(col="cyan3", shape=22, size=0.3, alpha=1) +
   # 
   # tm_add_legend(type = "symbol", title="Stations",
   #               labels = c("CSCI (n=231)","ASCI (n=233)", "Gages (n=222)"),
   #               col = c("chocolate3", "cornsilk", "cyan3"),
   #               shape=c(23,21,22), size = 1.1) +
   
   tm_compass(type = "arrow", size = 2,
              position = c(0.1,0.18)) +
   tm_scale_bar(breaks = c(0, 100, 200), 
                text.size = 0.6,
                position = c(0.12, 0.1)) +
   tm_layout(frame = FALSE, 
             legend.title.size = 1.7,
             legend.text.size = 1.2,
             legend.position = c(0.65,0.7),
             legend.outside = FALSE, attr.outside = FALSE,
             inner.margins = 0.01, outer.margins = (0.01),
             fontfamily = "Roboto Condensed")) #title.position = c(0.65, 0.9)))


# STREAM CLASS + SITES
tmap::tmap_save(tm = map_strmclass_bio, 
                filename = "figs/fig3_tmap_selected_bio_sites_strmclass.jpg", width = 8, height = 11, units = "in", dpi = 300)  

# tiff
tmap::tmap_save(tm = map_strmclass_bio, 
                filename = "figs/fig3_tmap_selected_bio_sites_strmclass.tiff", width = 8, height = 11, units = "in", dpi = 300)  


# GAGES MAP ---------------------------------------------------------------

# get all possible FFC DV gages
ref_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_ref_gages_list.csv") %>% mutate(CEFF_type="REF", site_id=as.character(site_id)) %>% 
  select(site_id:lon, CEFF_type)
alt_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_alt_gages_list.csv") %>% mutate(CEFF_type="ALT") %>% select(site_id:lon, CEFF_type)

usgs_gages <- bind_rows(ref_gages, alt_gages) %>% 
  st_as_sf(coords=c("lon", "lat"), remove=FALSE, crs=4326)

# then add gage stations by ref type
(map_usgs <- map_ca + 
    tm_shape(usgs_gages) +
    tm_dots(col= "cyan4",  border.col = "black",
            shape=22, size=0.3, alpha=1, 
            border.alpha = 0.8) +
    tm_add_legend(type = "symbol", title="Stations",
                  labels = c("Gages"),
                  col = c( "cyan4"),
                  shape=c(22), size = 1.1) +
    
    # tm_logo(file = "figs/logo_cws_websafe.png", position = c("left","bottom"), height = 5) +
    tm_layout(
      #title = "USGS Sites \n(n=2316)", 
      fontfamily = "Roboto Condensed",
      legend.title.size = 1.3,
      legend.text.size = 1,
      legend.outside = FALSE, attr.outside = FALSE,
      inner.margins = 0.01, outer.margins = (0.01),
      legend.position = c(0.65, 0.7)))


# tmap::tmap_save(tm = map_usgs, 
#                 filename = "figs/03_tmap_usgs_sites_all.png", width = 8, height = 11, units = "in", dpi = 300)  

# Put them all together ---------------------------------------------------

final_triptych<-tmap::tmap_arrange(
  map_bioall, map_usgs, map_bioselect, ncol = 3, outer.margins = 0.001)
print(final_triptych)

tmap::tmap_save(tm = final_triptych, 
                filename = "figs/fig2_tmap_triptych_biosel_usgs.jpg", width = 20,
                height=14.5, units = "cm", dpi = 600)  
 tmap::tmap_save(tm = final_triptych, 
                filename = "figs/fig2_tmap_triptych_biosel_usgs.tiff", width = 11, height = 8, units = "in", dpi = 300)  

# TMAP PALETTE EXPLORER ---------------------------------------------------


# tmaptools::palette_explorer()
# tm_shape(bmi_coms_final) +
#   tm_symbols(shape = 21, col = "h12_area_sqkm", n=5, pal="-Greens") #reverse the palette

# View Final Tally --------------------------------------------------------

# how many unique samples are avail: 
bmi_final_dat %>% st_drop_geometry %>% distinct(SampleID) %>% tally #n=493 samples
bmi_final_dat %>% st_drop_geometry %>% distinct(StationCode) %>% tally #n=275 sites

# how many unique USGS gages? n=226 (ALT=171, REF=55)
bmi_final_dat %>% st_drop_geometry %>% distinct(site_id, .keep_all=TRUE) %>% count(CEFF_type)

