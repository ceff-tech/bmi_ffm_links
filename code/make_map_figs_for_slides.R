# Make figs for slides:


# Load Libraries ----------------------------------------------------------

library(mapview)
library(tidyverse)
library(sf)
library(tmap)
library(ggspatial)
library(viridis)
library(USAboundaries)

# Load Data ---------------------------------------------------------------

load("data_output/00_bmi_stations_distinct.rda")
load("data_output/01_usgs_all_gages.rda")

# BMI SITES (final V2)
load("data_output/03_selected_final_bmi_stations_dat_all_gages.rda")

# BMI
sel_bmi_gages_csci <- readRDS("data_output/03_selected_bmi_h12_all_gages_csci.rds") # sel_bmi_gages w csci scores

sel_gages_bmi <- readRDS("data_output/03_selected_usgs_h12_all_gages.rds")
sel_h12_bmi <- readRDS("data_output/03_selected_h12_all_gages.rds")
load("data_output/03_selected_nhd_mainstems_gages.rda")

# this has usgs and bmi versions of the data
load("data_output/05_selected_bmi_csci_por_and_sf.rda")

# final sites
load("data_output/07_selected_bmi_csci_por_trim_w_huc_region.rda")

# simple just sites:
bmi_csci_sites <- bmi_csci_por_trim %>% 
  dplyr::distinct(StationCode, .keep_all = TRUE)

load("data_output/major_rivers_dissolved.rda")
load("data_output/huc12_sf.rda")

# read in fish regions:
ca_sp_regions <- read_sf("data/spatial/umbrella_sp_regions.shp", as_tibble = T) %>% st_transform(4326)

# map of all bug stations statewide
ca <- USAboundaries::us_states(resolution="low", states = "ca")
ca_co <- us_counties(states = "ca")


# Tidy Rivers Data --------------------------------------------------------------

# filter major rivers to only CA
rivs_ca <- st_intersection(rivs, ca) # crop

# filter to perennial only
rivs_filt <- filter(rivs, FEATURE_TYPE=="river", FEATURE_CLASS=="perennial")
#mapview(rivs, zcol="FEATURE_TYPE")

rivs_filt_ca <- st_intersection(rivs_filt, ca)


# Get Selected/Not Selected Data ------------------------------------------

# not selected bmi
bmi_not_selected_v2 <- sel_bmi_gages_csci %>% filter(!as.character(StationCode) %in% sel_bmi_coms_final_v2$StationCode) # n=203

# get all gages selected
gages_selected_v2 <- bmi_csci_por_usgs %>%
  filter(ID %in% bmi_csci_por_trim$ID) %>% 
  distinct(ID, .keep_all = TRUE)

# get the gages not selected
gages_not_selected_v2 <- sel_gages_bmi %>% 
  filter(!ID %in% gages_selected_v2$ID)


# Quick Mapview -----------------------------------------------------------

# set background basemaps/default options:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(homebutton = FALSE, basemaps=basemapsList, viewer.suppress = FALSE)

# quick preview
mapview(gages_selected_v2, col.regions="deepskyblue4", cex=7, alpha=0.7, legend=FALSE) +
  mapview(gages_not_selected_v2, col.regions="gray", cex=7, alpha=0.7, legend=FALSE) +
  mapview(mainstems_all, color="darkblue", lwd=1.8, legend=FALSE) +
  mapview(sel_h12_bmi, col.regions="darkslategray4",
          alpha.regions=0.3, lwd=0.8, legend=FALSE) +
  # mapview(ca_sp_regions, zcol="huc_region", alpha.regions=0.3,
  #         layer.name="CA Regions") +
  mapview(sel_bmi_gages_csci, col.regions="gray", cex=4.5, alpha=.7,
          layer.name="BMI Stations in H12") +
  mapview(bmi_csci_sites, col.regions="orange", cex=5, alpha=.7,
          layer.name="Selected BMI")



# Load Rivers and Save out from Pisces ------------------------------------
# dbcon <- src_sqlite("/Users/ryanpeek/Box Sync/GIS/pisces.sqlite", create = F)
# src_tbls(dbcon) # see tables in DB
# read in
# rivs <- st_read(dsn = "/Users/ryanpeek/Box Sync/GIS/pisces.sqlite", layer='major_rivers_dissolved') %>% st_transform(4326)
# save(rivs, file = "data_output/major_rivers_dissolved.rda")

# h12s <- st_read(dsn="/Users/ryanpeek/Box Sync/GIS/pisces.sqlite", layer="HUC12FullState") %>% st_transform(4326)
# save(h12s, file = "data_output/h12s.rda")


# Make a Map of all BMI Sites ---------------------------------------------

# first make CA map with no border
map_ca <- tm_shape(ca) + tm_polygons() + 
  tm_shape(ca_co) + tm_polygons(border.col = "gray") +
  tm_compass(type = "arrow", position = c(0.25, 0.1), size = 1.5) +
  tm_scale_bar(position = c(0.2, 0.03), breaks = c(0, 100, 200), text.size = 0.4) +
  tm_layout(frame=FALSE) #+
map_ca  

# then add bug stations by collection method
(tm_ca_bmi_sites <- map_ca + tm_shape(bmi_stations_distinct) +
    tm_symbols(col="maroon", border.col = "black", size=0.3) +
    tm_logo(file = paste0(here::here(),"/figs/logo_cws_websafe.png"), position = c("left","bottom"), height = 5) +
    #tm_facets(by = "collectionmethodcode", nrow = 2,free.coords = FALSE) + 
    tm_layout(legend.show = F, legend.outside = TRUE, 
              title = "BMI Stations \n (n=2,935)", 
              fontfamily = "Roboto Condensed",title.size = 2,
              title.position = c(0.6, 0.9),
              #legend.outside.position = c(0.5, 0.2), 
              legend.outside.size = 0.4))

tmap_save(filename="figs/map_of_bmi_sites.jpg", width = 8, height = 11, units="in", dpi=300)


# Make a Map of all USGS Sites ---------------------------------------------

# first make CA map with no border
map_ca <- tm_shape(ca) + tm_polygons() + 
  tm_shape(ca_co) + tm_polygons(border.col = "gray") +
  tm_compass(type = "arrow", position = c(0.25, 0.1), size = 1.5) +
  tm_scale_bar(position = c(0.2, 0.03), breaks = c(0, 100, 200), text.size = 0.5) +
  tm_shape(rivs_ca) + tm_lines(col="mediumblue", lwd = .4, alpha = 0.85) +
  tm_layout(frame=FALSE)
map_ca  

usgs_final_all <- st_intersection(usgs_final_all, ca)

# then add gage stations by ref type
(tm_ca_usgs <- map_ca + tm_shape(usgs_final_all) +
    tm_symbols(col="CEFF_type", palette=c("darkblue", "skyblue"),  
               border.col = "black", size=0.3) +
    tm_logo(file = "figs/logo_cws_websafe.png", position = c("left","bottom"), height = 5) +
    tm_layout(legend.show = TRUE, 
              title = "USGS Sites \n(n=810)", 
              fontfamily = "Roboto Condensed",title.size = 2,
              title.position = c(0.6, 0.9),
              legend.position = c(0.6, 0.7)))

tmap_save(filename="figs/map_of_usgs_sites_w_rivers.jpg", width = 8, height = 11, units="in", dpi=300)


# Ref ONLY gages ----------------------------------------------------------

gages_sel <- st_intersection(gages_final, ca)

# first make CA map with no border
map_ca <- tm_shape(ca) + tm_polygons() + 
  tm_shape(ca_co) + tm_polygons(border.col = "gray") +
  tm_compass(type = "arrow", position = c(0.25, 0.1), size = 1.5) +
  tm_scale_bar(position = c(0.2, 0.03), breaks = c(0, 100, 200), text.size = 0.5) +
  tm_shape(rivs_ca) + tm_lines(col="mediumblue", lwd = .4, alpha = 0.85) +
  tm_layout(frame=FALSE)

# then add gage stations by ref type
(tm_ca_usgs <- map_ca + tm_shape(gages_sel) +
    tm_symbols(col="skyblue",  
               border.col = "black", size=0.5) +
    tm_logo(file = "figs/logo_cws_websafe.png", position = c("left","bottom"), height = 5) +
    tm_layout(legend.show = TRUE, 
              title = "USGS Reference Gages \n(n=250)", 
              fontfamily = "Roboto Condensed",title.size = 2,
              title.position = c(0.6, 0.9),
              legend.position = c(0.6, 0.7)))

tmap_save(filename="figs/map_of_usgs_referenceonly_w_rivers.jpg", width = 8, height = 11, units="in", dpi=300)


# Make a Map of HUC12s ---------------------------------------------
h12v <- h12[st_is_valid(h12),]
plot(h12v$geom)
h12_ca <- st_intersection(h12v, ca)
plot(h12_ca$geom)

ggplot() + geom_sf(data=ca) + 
  geom_sf(data=h12_ca, alpha=0.4,lwd=0.2, color="deepskyblue4")+
  theme_classic() + 
  ggspatial::annotation_north_arrow(style = north_arrow_nautical())
ggsave("figs/map_of_all_h12s_outlines.jpg", width = 8, height = 11, units="in", dpi=300)

# get h12 for each point
bmi_h12 <- st_join(bmi_clean_stations, left = TRUE, h12_ca[c("HUC_12")])
gages_sel_h12 <- st_join(gages_sel, left=TRUE, h12[c("HUC_12")]) %>% 
  select(ID, HUC_12, LATITUDE, LONGITUDE) %>% st_drop_geometry()

bmi_gages_join <- inner_join(bmi_h12, gages_sel_h12, by="HUC_12") %>% 
  distinct(StationCode, ID, .keep_all = T)

# now look at overlap
h12_bmi <- h12[bmi_gages_join, ]

# selected h12s
ggplot() + geom_sf(data=ca) + 
  geom_sf(data=h12_ca, alpha=0.4,lwd=0.2, color="deepskyblue4")+
  geom_sf(data=h12_bmi, alpha=1, lwd=.3, fill="orange", color="coral")+
  theme_classic() + 
  ggspatial::annotation_north_arrow(style = north_arrow_nautical())

ggsave("figs/map_of_selected_h12s_outlines.jpg", width = 8.5, height = 11, units="in", dpi=300)


# first make CA map with no border
map_ca <- tm_shape(ca) + tm_polygons() + 
  tm_shape(ca_co) + tm_polygons(border.col = "gray") +
  tm_compass(type = "arrow", position = c(0.25, 0.1), size = 1.5) +
  tm_scale_bar(position = c(0.2, 0.03), breaks = c(0, 100, 200), text.size = 0.5) + 
  tm_shape(h12_ca) + tm_borders(col="deepskyblue4", lwd=0.2, alpha = 0.9) +
  tm_shape(h12_bmi) + tm_fill(col="orange", alpha = 0.9) +
  tm_layout(frame=FALSE) #+
map_ca  

# save out
tmap_save(filename="figs/map_tmap_of_all_h12s_outlines.jpg", width = 8.5, height = 11, units="in", dpi=300)
tmap_save(filename="figs/map_tmap_of_potential_h12s_only.jpg", width = 8.5, height = 11, units="in", dpi=300)
tmap_save(filename="figs/map_tmap_of_potential_h12s_all.jpg", width = 8.5, height = 11, units="in", dpi=300)


# then add bug stations by collection method
(tm_ca_usgs <- map_ca + 
    tm_shape(sel_gages_bmi) +
    tm_symbols(col="#31688EFF", border.col = "black", size=1) +
    tm_logo(file = "figs/logo_cws_websafe.png", position = c("left","bottom"), height = 5) +
    # tm_add_legend(type = "symbol", 
    #               col = c("#31688EFF","orange"),
    #               size = 0.5, title = "Sites",
    #               labels = c("USGS Gage","BMI Station")) +
    tm_layout(legend.show = TRUE, 
              #title = "Selected Stations", 
              fontfamily = "Roboto Condensed",title.size = 1.5,
              title.position = c(0.6, 0.9),
              legend.position = c(0.6, 0.7)))

# save out
tmap_save(filename="figs/map_of_selected_gages_w_potential_h12s.jpg", width = 8.5, height = 11, units="in", dpi=300)


# Make a Map of Sites in Same HUC12 ---------------------------------------------

# first make CA map with no border
map_ca <- tm_shape(ca) + tm_polygons() + 
  tm_shape(ca_co) + tm_polygons(border.col = "gray") +
  tm_compass(type = "arrow", position = c(0.25, 0.1), size = 1.5) +
  tm_scale_bar(position = c(0.2, 0.03), breaks = c(0, 100, 200), text.size = 0.5) + 
  tm_shape(rivs_ca) + tm_lines(col="mediumblue", lwd = .4, alpha = 0.85) +
  tm_layout(frame=FALSE) #+
map_ca  

# then add bug stations by collection method
(tm_ca_usgs <- map_ca + 
    tm_shape(gages_selected_v2) +
    tm_symbols(col="#31688EFF", border.col = "black", size=1.5) +
    tm_shape(bmi_csci_sites) +
    tm_symbols(col="orange", border.col = "black", size=.55) +
    tm_logo(file = "figs/logo_cws_websafe.png", position = c("left","bottom"), height = 5) +
    tm_add_legend(type = "symbol", 
                  col = c("#31688EFF","orange"),
                  size = 0.5, title = "Sites",
                  labels = c("USGS Gage","BMI Station")) +
    tm_layout(legend.show = TRUE, 
              title = "Selected Stations", 
              fontfamily = "Roboto Condensed",title.size = 1.5,
              title.position = c(0.6, 0.9),
              legend.position = c(0.6, 0.7)))

# save out
tmap_save(filename="figs/map_of_selected_sites.jpg", width = 8, height = 11, units="in", dpi=300)

