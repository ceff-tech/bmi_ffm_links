# 0 Generate Final Selected Sites/Data
## R. Peek
## Look at final output


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

# Selected Sites: with BMI data
load("data_output/02c_selected_final_bmi_dat_all.rda")

# Streamlines
load("data_output/02b_sel_gage_mainstems_all.rda")

# tidy streams
# get distinct segs that are DS only (either DD or DS)
mainstems_distinct <- mainstems_all %>% 
  filter(from_gage %in% c("DS", "DD")) %>% 
  distinct(nhdplus_comid, .keep_all=TRUE)

# get ecoregions
eco_revised <- read_rds("data/spatial/ecoregions_combined_L3.rds")

#load(url("https://github.com/ksirving/asci_ffm_2019/blob/master/output_data/05_algae_asci_por_trim_ecoreg.rda?raw=true")) 
#write_rds(algae_asci_por_trim_ecoreg, file = "data_output/03_algae_asci_por_trim_ecoreg.rds")
asci <- read_rds("data_output/03_algae_asci_por_trim_ecoreg.rds")
load("data_output/01a_algae_stations_distinct.rdata") # algae_stations_distinct
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

m3 <- mapview(bmi_final_dat, cex=6, col.regions="orange", 
              layer.name="Selected BMI CSCI") +  
  mapview(mainstems_all %>% filter(from_gage=="UM"), color="forestgreen", cex=3, 
          layer.name="NHD Flowlines US") +
  mapview(mainstems_distinct, color="steelblue", cex=3, 
          layer.name="NHD Flowlines DS") +
  mapview(gages_selected_v2, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all bmi or gages in same H12 but not selected
  mapview(gages_not_selected_v2, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(bmi_not_selected_v2, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other BMI Sites in H12") + 
  mapview(hucs_selected_v2, col.regions="orange3", alpha.region=0.1, 
          color="orange", legend=F, layer.name="Selected HUC12") +
  mapview(hucs_not_selected_v2, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="Other HUC12")


m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save out
#mapshot(m3, url = paste0(here::here(),"/figs/03_map_of_final_bmi_csci_sites.html"))


# TMAP Ecoregions  ---------------------------------------------------------------

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

#  CA with revised ecoregions
(map_ecoca <- tm_shape(ca) + tm_polygons(border.alpha = 0.3) +
    tm_layout(frame=FALSE) +
    tm_shape(eco_revised) + tm_polygons(border.alpha=0.1, col = "US_L3_mod", alpha = 0.7, palette="viridis", title="bmi_ffm_links.RprojEcoregions") +
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
                filename = "figs/03_tmap_ecoregions_revised.png", width = 8, height = 11, units = "in", dpi = 300)

# TMAP CSCI stations ---------------------------------------------------------

# get ALL bug data (distinct stations)      
load("data_output/01_bmi_stations_distinct.rda")

# make a tmap
(map_bmi <- map_ca +
    tm_shape(bmi_stations_distinct) +
    tm_dots(col = "#FDE725FF", shape = 21, size = 0.1, alpha=0.8, border.alpha=0.9) + 
    tm_compass(type = "arrow", size = 2,
               position = c(0.1,0.18)) +
    #position = c(0.15, 0.2)) +
    tm_scale_bar(breaks = c(0, 100, 200), 
                 text.size = 0.6,
                 position = c(0.12, 0.1)) +
    tm_layout(title = glue("Sampling Locations\n (CSCI: n={nrow(bmi_stations_distinct)})"), 
              legend.show = FALSE, frame = FALSE, title.size = 0.8,
              legend.outside = FALSE, attr.outside = FALSE,
              inner.margins = 0.01, outer.margins = (0.01),
              fontfamily = "Roboto Condensed", title.position = c(0.65, 0.7)))


# FIRST TRYPTYCH
tmap::tmap_save(tm = map_bmi, 
                filename = "figs/03_tmap_csci_sites_all.png", width = 8, height = 11, units = "in", dpi = 300)  

# TMAP ASCI stations ---------------------------------------------------------

# make algae sf
algae_distinct <- algae_stations_distinct %>% 
  filter(!is.na(Latitude)) %>% 
  st_as_sf(coords=c("Longitude","Latitude"), remove=FALSE, crs=4326)

# make a tmap
(map_asci <- map_ca +
    tm_shape(algae_distinct) +
    tm_dots(col = "red2", shape = 21, size = 0.1, alpha=0.8, border.alpha=0.9) + 
    tm_compass(type = "arrow", size = 2,
               position = c(0.1,0.18)) +
    #position = c(0.15, 0.2)) +
    tm_scale_bar(breaks = c(0, 100, 200), 
                 text.size = 0.6,
                 position = c(0.12, 0.1)) +
    tm_layout(title = glue("Sampling Locations\n (ASCI: n={nrow(algae_stations_distinct)})"), 
              legend.show = FALSE, frame = FALSE, title.size = 0.8,
              legend.outside = FALSE, attr.outside = FALSE,
              inner.margins = 0.01, outer.margins = (0.01),
              fontfamily = "Roboto Condensed", title.position = c(0.65, 0.7)))


# FIRST TRYPTYCH
tmap::tmap_save(tm = map_asci, 
                filename = "figs/03_tmap_asci_sites_all.png", width = 8, height = 11, units = "in", dpi = 300)  

# TMAP ASCI and CSCI distinct stations ---------------------------------------


# make a tmap
(map_bioall <- map_ca +
   tm_shape(algae_distinct) +
   tm_dots(col = "red2", shape = 21, size=0.2, alpha=0.9, border.alpha=0.3, legend.show = TRUE) + 
   tm_shape(bmi_stations_distinct) +
   tm_dots(col="#FDE725FF", shape=21, size=0.2, alpha=0.6, border.alpha=0.3, legend.show = TRUE) +
   tm_compass(type = "arrow", size = 2,
              position = c(0.1,0.18)) +
   #position = c(0.15, 0.2)) +
   tm_scale_bar(breaks = c(0, 100, 200), 
                text.size = 0.6,
                position = c(0.12, 0.1)) +
   tm_layout(title = glue("Sampling Locations\n (ASCI: n={nrow(algae_stations_distinct)}\n  CSCI: n={nrow(bmi_stations_distinct)})"), 
             legend.show = FALSE, frame = FALSE, title.size = 0.8,
             legend.outside = FALSE, attr.outside = FALSE,
             inner.margins = 0.01, outer.margins = (0.01),
             fontfamily = "Roboto Condensed", title.position = c(0.65, 0.7)))


# FIRST TRYPTYCH
tmap::tmap_save(tm = map_bioall, 
                filename = "figs/03_tmap_all_bio_sites.png", width = 8, height = 11, units = "in", dpi = 300)  


# TMAP Selected ASCI-CSCI stations --------------------------------------------------------

# make a tmap
(map_bioselect <- map_ca +
   # tm_shape(gages_selected_v2) +
   # tm_dots(col="cyan3", shape=21, size=0.4, alpha=1) +
   tm_shape(asci) +
   tm_dots(col = "red2", shape = 21, size = 0.3, alpha=1, border.alpha=0.3, legend.show = TRUE) + 
   tm_shape(bmi_final_dat) +
   tm_dots(col="#FDE725FF", shape=21, size=0.3, alpha=0.7, border.alpha=0.3, legend.show = TRUE) +
   tm_compass(type = "arrow", size = 2,
              position = c(0.1,0.18)) +
   tm_scale_bar(breaks = c(0, 100, 200), 
                text.size = 0.6,
                position = c(0.12, 0.1)) +
   tm_layout(title = glue("Stations\n (ASCI: n=243\n  CSCI: n=275)"), 
             frame = FALSE, title.size = 0.8,
             legend.outside = FALSE, attr.outside = FALSE,
             inner.margins = 0.01, outer.margins = (0.01),
             fontfamily = "Roboto Condensed", title.position = c(0.65, 0.7)))


# Selected
tmap::tmap_save(tm = map_bioselect, 
                filename = "figs/03_tmap_selected_bio_sites_combined.png", width = 8, height = 11, units = "in", dpi = 300)  


# GAGES MAP ---------------------------------------------------------------

# get all possible FFC DV gages
ref_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_ref_gages_list.csv") %>% mutate(CEFF_type="REF", site_id=as.character(site_id)) 
alt_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_alt_gages_list.csv") %>% mutate(CEFF_type="ALT")

usgs_gages <- bind_rows(ref_gages, alt_gages) %>% 
  select(-geometry) %>% 
  st_as_sf(coords=c("lon", "lat"), remove=FALSE, crs=4326)

# then add gage stations by ref type
(map_usgs <- map_ca + 
    tm_shape(usgs_gages) +
    tm_symbols(col= "steelblue",  border.col = "black",
               size=0.1, border.alpha = 0.8) +
    # tm_logo(file = "figs/logo_cws_websafe.png", position = c("left","bottom"), height = 5) +
    tm_layout(title = "USGS Sites \n(n=2316)", 
              fontfamily = "Roboto Condensed",title.size = 0.8,
              legend.outside = FALSE, attr.outside = FALSE,
              inner.margins = 0.01, outer.margins = (0.01),
              title.position = c(0.65, 0.7)))


tmap::tmap_save(tm = map_usgs, 
                filename = "figs/03_tmap_usgs_sites_all.png", width = 8, height = 11, units = "in", dpi = 300)  

# FINAL SITES (SELECTED GAGES ONLY) TMAP -------------------------------------------

# make paired sites
(map_final_sites <- map_ca +
   tm_shape(gages_selected_v2) +
   tm_dots(col="#21908CFF", shape=21, size=0.3, alpha=1) +
   tm_shape(bmi_final_dat) +
   tm_dots(col="orange", shape=21, size=0.1, alpha=1) +
   tm_layout(title = "Selected Sites:\nGages (n=226)\nBMI (n=275)",
             title.size = 0.8, frame = FALSE, 
             fontfamily = "Roboto Condensed",
             legend.outside = FALSE, attr.outside = FALSE,
             inner.margins = 0.01, outer.margins = (0.01),
             title.position = c(0.65, 0.7)))
  #tm_compass(type = "4star", position = c("right","top"))+
  #tm_scale_bar(position = c("left","bottom"))

tmap::tmap_save(tm=map_final_sites, filename = "figs/03_tmap_selected_paired_sites_w_rivers.png", width = 8, height = 11, units = "in", dpi = 300)  


# Put them all together ---------------------------------------------------

final_triptych<-tmap::tmap_arrange(map_bioall, map_usgs, map_bioselect, ncol = 3, outer.margins = 0.001)
print(final_triptych)

tmap::tmap_save(tm = final_triptych, 
                filename = "figs/03_tmap_triptych_biosel_usgs.png", width = 11, height = 7, units = "in", dpi = 300)  


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

