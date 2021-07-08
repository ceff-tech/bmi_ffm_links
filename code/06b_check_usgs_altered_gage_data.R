# get additional data from USGS gages that are altered:

# Libraries ---------------------------------------------------------------
library(dataRetrieval)
library(sf)
library(tidyverse)
library(mapview)
mapviewOptions(fgb = FALSE)
library(purrr)
library(lubridate)
library(tidylog)


# Get Data ----------------------------------------------------------------

# load data
ffm_gages <- read_rds("data_output/06_ffm_gages_missing_metrics.rds")


# Update ------------------------------------------------------------------

# temps: parameterCd = "00010", 
# discharge: "00060"
# dailymean = statCd "00003"
# https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes/help?codes_help
# check what daily data is available:
usgs_daily <- whatNWISdata(siteNumber=ffm_gages$gageid, service='dv', parameterCd = '00060', statCd='00003') %>% 
   select(site_no, station_nm, dec_lat_va, dec_long_va, huc_cd,
          parm_cd, stat_cd,
          data_type_cd, begin_date:count_nu) %>% 
   rename(interval=data_type_cd, huc8=huc_cd, site_id=site_no,
          date_begin=begin_date, date_end=end_date) %>% 
   mutate(yr_begin = year(date_begin),
          yr_end = year(date_end),
          yr_total = yr_end-yr_begin)

usgs_daily %>% View()

# Visualize ---------------------------------------------------------------

# visualize the stations to see date ranges
ggplot() + 
  geom_linerange(data=usgs_daily, 
                 aes(x=forcats::fct_reorder(site_id, huc8), 
                     ymin=yr_begin, 
                     ymax=yr_end, 
                     color=as.factor(huc8)), 
                 size=1.1, show.legend = T) + 
  coord_flip() + 
  labs(x="", y="") + 
  scale_color_viridis_d("HUC8")+
  #theme_bw(base_family = "Roboto Condensed", base_size = 8) +
  ggdark::dark_theme_bw(base_family = "Roboto Condensed", base_size = 8)

# make map
usgs_daily_sf <- usgs_daily %>% 
  rename(lon = dec_long_va, lat=dec_lat_va) %>% 
  st_as_sf(coords=c("lon","lat"), remove=FALSE, crs=4326)

mapview(usgs_daily_sf, zcol="yr_total")



# Download Daily ----------------------------------------------------------

# Get daily
usgs_q_day <- dataRetrieval::readNWISdv(
  siteNumbers=usgs_daily$site_id[1:50],
  parameterCd = "00060",
  statCd = "00003")

usgs_q_day2 <- dataRetrieval::readNWISdv(
  siteNumbers=usgs_daily$site_id[51:134],
  parameterCd = "00060",
  statCd = "00003")

# add water year
usgs_q_day <- usgs_q_day %>% dataRetrieval::addWaterYear()
usgs_q_day2 <- usgs_q_day2 %>% dataRetrieval::addWaterYear()

# bind
usgs_Q_daily <- bind_rows(usgs_q_day, usgs_q_day2)

# add correct col names:
usgs_Q_daily <- dataRetrieval::renameNWISColumns(usgs_Q_daily)

# save out
save(usgs_Q_daily, file = "data/usgs_Q_daily_to_review.rda")

# Plot --------------------------------------------------------------------

# create function to plot by facet
library(ggplot2)
library(plotly)

# expects x and y to be date and temp, but specify col name in quotes
plot_gage_facet <- function(data, x, y, facetid, plotly=FALSE){
  if(plotly == TRUE){
    p1 <- ggplot() +
      geom_line(data=data,
                aes(x=.data[[x]], y=.data[[y]],
                    group=.data[[facetid]], color=.data[[facetid]]),
                show.legend = F) +
      theme_classic(base_family = "Roboto Condensed", base_size = 9) +
      scale_color_viridis_d() + labs(y="Temp (C)", x="") +
      theme(axis.text.x = element_text(angle=90, hjust = 1)) +
      facet_wrap(.~.data[[facetid]], scales= "free_x")
    cat("Plotly it is!")
    return(plotly::ggplotly(p1))
  }
  p2 <- ggplot() +
    geom_line(data=data,
              aes(x=.data[[x]], y=.data[[y]],
                  group=.data[[facetid]], color=.data[[facetid]]),
              show.legend = F) +
    theme_classic(base_family = "Roboto Condensed", base_size = 9) +
    scale_color_viridis_d() + labs(y="Temp (C)", x="") +
    theme(axis.text.x = element_text(angle=90, hjust = 1)) +
    facet_wrap(.~.data[[facetid]], scales= "free")
  cat("printing static ggplots...")
  return(print(p2))
}

# get list of gages
usgs_Q_daily %>% filter(site_no %in% ffm_gages$gageid[1:20]) %>% 
  plot_gage_facet(., x="Date", y="Flow", 
                facetid = "site_no")

ggsave(filename = "figs/hydrographs_highly_alt_gageids_1-20.png", width = 11, height = 8.5, dpi=300)

# get list of gages
usgs_Q_daily %>% filter(site_no %in% ffm_gages$gageid[21:40]) %>% 
  plot_gage_facet(., x="Date", y="Flow", 
                  facetid = "site_no")

ggsave(filename = "figs/hydrographs_highly_alt_gageids_21-40.png", width = 11, height = 8.5, dpi=300)

# get list of gages
usgs_Q_daily %>% filter(site_no %in% ffm_gages$gageid[41:60]) %>% 
  plot_gage_facet(., x="Date", y="Flow", 
                  facetid = "site_no")

ggsave(filename = "figs/hydrographs_highly_alt_gageids_41-60.png", width = 11, height = 8.5, dpi=300)

# get list of gages
usgs_Q_daily %>% filter(site_no %in% ffm_gages$gageid[61:80]) %>% 
  plot_gage_facet(., x="Date", y="Flow", 
                  facetid = "site_no")

ggsave(filename = "figs/hydrographs_highly_alt_gageids_61-80.png", width = 11, height = 8.5, dpi=300)

usgs_Q_daily %>% filter(site_no %in% ffm_gages$gageid[81:100]) %>% 
  plot_gage_facet(., x="Date", y="Flow", 
                  facetid = "site_no")

ggsave(filename = "figs/hydrographs_highly_alt_gageids_81-100.png", width = 11, height = 8.5, dpi=300)


usgs_Q_daily %>% filter(site_no %in% ffm_gages$gageid[101:134]) %>% 
  plot_gage_facet(., x="Date", y="Flow", 
                  facetid = "site_no")

ggsave(filename = "figs/hydrographs_highly_alt_gageids_101-134.png", width = 11, height = 8.5, dpi=300)

