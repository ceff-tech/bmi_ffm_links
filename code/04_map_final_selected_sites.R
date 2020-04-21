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

load("data_output/01_bmi_stations_distinct_status.rda")
sel_bmi_gages <- readRDS("data_output/03_selected_bmi_h12_all_gages.rds") # sel_bmi_gages
sel_gages_bmi <- readRDS("data_output/03_selected_usgs_h12_all_gages.rds") # sel_gages_bmi
load("data_output/03_selected_nhd_mainstems_gages.rda") # mainstems_all
sel_h12_bmi <- read_rds("data_output/03_selected_h12_all_gages.rds")

load("data_output/03_selected_final_bmi_stations_dat_all_gages.rda") # bmi_coms_dat (all data for selected), bmi_coms_final (just coms and id)

# first add site status
bmi_coms_final <- left_join(bmi_coms_final, bmi_stations_distinct_status[,c(1:2)], by="StationCode") %>%
  # filter for distinct
  distinct(StationCode, ID, .keep_all = TRUE) %>%
  select(StationCode:comid2,SiteStatus,geometry)

# how many missing ss? 371 don't have site status
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
# library(mapdeck)
# set_token(Sys.getenv("MAPBOX_TOKEN"))
# 
# mapdeck(
#   style=mapdeck_style("dark")
# ) %>% 
#   add_path(data = mainstems_all, stroke_colour = "gageID", tooltip="nhdplus_comid", auto_highlight = TRUE) %>% 
#   add_sf(data = st_transform(bmi_coms_final, 4326), 
#          fill_colour="#EE7600", tooltip="StationCode", 
#          layer_id="BMI Comids", radius=500) %>% 
#   add_sf(data = st_transform(sel_gages_bmi, 4326), fill_colour="#00EEEE", radius=300, tooltip="site_id",
#          layer_id="USGS Gages")


# Make Mapview of Selected Gages and BMI Stations ----------------------

# get all BMI not selected...check why not on map
bmi_not_selected <- sel_bmi_gages %>% filter(!as.character(comid) %in% mainstems_all$nhdplus_comid) # should be 352

# get all gages selected
gages_selected <- sel_gages_bmi %>% 
  filter(gage_id %in% bmi_coms_final$gage_id)

# get the gages not selected
gages_not_selected <- sel_gages_bmi %>% 
  filter(!gage_id %in% bmi_coms_final$gage_id)

# get mainstems distinct
mainstems_distinct <- mainstems_all %>% distinct(nhdplus_comid, .keep_all=TRUE)

table(gages_selected$CEFF_type) # ALT=156  REF=56

# this map of all sites selected U/S and D/S
m3 <- mapview(bmi_coms_final, cex=6, col.regions="orange", 
              layer.name="Selected BMI comids") +  
  mapview(mainstems_distinct, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(gages_selected, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all bmi or gages in same H12 but not selected
  mapview(gages_not_selected, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(bmi_not_selected, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other BMI Sites in H12") + 
  mapview(sel_h12_bmi, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# Tmap --------------------------------------------------------------------

library(tmap)
library(USAboundaries)
ca<-us_counties(states="ca")
load("data_output/major_rivers_dissolved.rda")

# make a tmap
tm_shape(ca) + 
  tm_polygons() +
  tm_shape(rivs) + tm_lines(col="darkblue", lwd=0.7, alpha=0.8) +
  tm_shape(bmi_coms_final) +
  tm_dots(col = "orange", shape = 21, size = 0.2, alpha=0.8) + 
  tm_layout(title = "BMI Sites\n in CA", legend.show = FALSE, frame = FALSE, fontfamily = "Roboto Condensed", title.position = c(0.7, 0.7)) +
  tm_compass(type = "4star", position = c("right","top"))+
  tm_scale_bar(position = c("left","bottom"))

tmap::tmap_save(filename = "figs/03_map_tmap_bmi_sites_w_rivers.png", width = 8, height = 11, units = "in", dpi = 300)  

# make paired sites
tm_shape(ca) + 
  tm_polygons(alpha = 0.2) +
  tm_shape(rivs) + tm_lines(col="darkblue", lwd=0.7, alpha=0.8) +
  tm_shape(bmi_coms_final) +
  tm_dots(col = "gold1", shape = 21, size = 0.5, alpha=0.8, title="BMI Sites", legend.show = TRUE, legend.is.portrait = TRUE) + 
  tm_shape(gages_selected) +
  tm_dots(col = "CEFF_type", shape=21, size=0.2, alpha=0.8, palette=c("steelblue", "#440154FF"), 
          title="USGS Gage", legend.show = TRUE) +
  tm_layout(title = "BMI Sites (orange)\n& USGS Gage Pairs",
            frame = FALSE, fontfamily = "Roboto Condensed",
            title.position = c(0.65, 0.7)) +
  tm_compass(type = "4star", position = c("right","top"))+
  tm_scale_bar(position = c("left","bottom"))

tmap::tmap_save(filename = "figs/03_map_tmap_bmi_usgs_sites_w_rivers.png", width = 8, height = 11, units = "in", dpi = 300)  

  
# tmaptools::palette_explorer()
# tm_shape(bmi_coms_final) +
#   tm_symbols(shape = 21, col = "h12_area_sqkm", n=5, pal="-Greens") #reverse the palette

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

# # Check against CSCI Scores -----------------------------------------------
# 
# 
# 
# # see what data exist against CSCI scores currently avail (from Raffi)
# csci <- read_csv("data/csci/csci_core.csv")
# csci2 <- read_csv("data/csci/csci_core_v2.csv")
# 
# # match against existing sites:
# bmi_csci <- inner_join(bmi_coms_final, csci, by=c("StationCode"="stationcode"))
# 
# # only 995 unique
# bmi_csci %>% st_drop_geometry() %>% distinct(sampleid) %>% View()
# 
# # map
# mapview(bmi_coms_final, color="orange", col.regions="gray", alpha.regions=0.1,
#         layer.name="Selected BMI Sites") +
# mapview(bmi_csci, col.regions="mediumpurple2", cex=1, layer.name="Selected BMI w CSCI") + 
#   mapview(sel_gages_bmi, col.regions="dodgerblue", cex=2.5, alpha.regions=0.7, 
#           layer.name="USGS gages")
# 
# # how many unique matches?
# length(unique(bmi_csci$StationCode))
# table(bmi_csci$SiteStatus)
# 
# # look at CSCI histogram of all scores
# (bmi_csci %>% ggplot() + geom_histogram(aes(csci), bins = 40) +
#   labs(subtitle = "Raw CSCI score for reference gages") +
#   theme_bw() -> gg_csci_hist)
# 
# # look at CSCI boxplot single box plot
# bmi_csci %>% ggplot() + geom_boxplot(aes(y=csci), show.legend = F, fill="mediumpurple3") +
#   labs(subtitle = "Raw CSCI score for reference gages") +
#   theme_bw()
# 
# # look at CSCI boxplot by year
# (bmi_csci %>% ggplot() + geom_boxplot(aes(y=csci, x=sampleyear, group=as.factor(sampleyear)), show.legend = F, fill="mediumpurple3", color="gray30") +
#   labs(subtitle = "Raw CSCI score for reference gages") +
#   theme_bw() -> gg_bmi_box_yr)
# 
# # look at sampling timing
# hist(bmi_csci$samplemonth) # majority of months sampled May:Aug
# table(bmi_csci$samplemonth)
# 
# # look at CSCI percentile by Site Status (not avail for all sites)
# 
# # function to get data
# stat_box_data <- function(y, upper_limit = max(bmi_csci$csci, na.rm = T)) {
#   return( 
#     data.frame(
#       y = 0.95 * upper_limit,
#       label = paste('count =', length(y), '\n',
#                     'mean =', round(mean(y, na.rm = T), 2), '\n')
#     )
#   )
# }
# 
# # plot CSCI
# (gg_csci_ss <- ggplot(data=filter(bmi_csci, !is.na(SiteStatus)), aes(x=SiteStatus, y=csci)) + 
#   geom_violin(aes(fill=SiteStatus), color="transparent", alpha=0.8, show.legend = F)+
#   geom_boxplot(fill="gray40", show.legend = F, outlier.alpha=0, width=.1) +
#   stat_summary(fun.data=stat_box_data, geom="text", hjust=1, vjust=0.9) +
#   ggdark::dark_theme_bw(base_family = "Roboto Condensed") +
#   labs(x="Site Status", y="Raw CSCI Score") + 
#   scale_fill_viridis_d())
# 
# # function for percentile
# stat_box_data <- function(y, upper_limit = max(bmi_csci$csci_percentile, na.rm=T)) {
#   return( 
#     data.frame(
#       y = 0.95 * upper_limit,
#       label = paste('count =', length(y), '\n',
#                     'mean =', round(mean(y, na.rm=T), 2), '\n')
#     )
#   )
# }
# 
# # plot CSCI percentile
# (gg_csci_ss_prcnt <- ggplot(data=filter(bmi_csci, !is.na(SiteStatus)), 
#                         aes(x=SiteStatus, y=csci_percentile)) + 
#   geom_violin(aes(fill=SiteStatus), color="transparent", alpha=0.8, show.legend = F)+
#   geom_boxplot(fill="gray40", show.legend = F, outlier.alpha=0, coef=0, width=.1) +
#   stat_summary(fun.data=stat_box_data, geom="text", hjust=1, vjust=0.9) +
#   ylim(c(0,1))+
#   ggdark::dark_theme_bw(base_family = "Roboto Condensed") +
#   labs(x="Site Status", y="CSCI Percentile") + 
#   scale_fill_viridis_d())
# 
# library(patchwork)
# 
# # horiz
# gg_csci_ss + gg_csci_ss_prcnt
# 
# # vert
# gg_csci_ss + gg_csci_ss_prcnt + plot_layout(ncol=1)
# 
# # nested layout
# (gg_csci_ss / gg_csci_ss_prcnt) - (gg_csci_hist / gg_bmi_box_yr)
# 
# ggsave(filename = "figs/03_bmi_ref_sites_csci_summaries_all.png", width = 11, height = 8.5, dpi=300, units = "in")
