# creates list of all CA gages with daily data (reference and non ref)


# Packages ----------------------------------------------------------------

library(dplyr)
library(dataRetrieval)

# Set up Parameters -------------------------------------------------------

paramCD <- "00060" # discharge (cfs) (temperature=00010, stage=00065)
dataInterval <- "dv" # daily interval

# Run function and clean/rename columns
ca_usgs_gages <- dataRetrieval::whatNWISdata(stateCd="California", 
                                             service=dataInterval, 
                                             parameterCd=paramCD) %>% 
  dplyr::select(site_no, station_nm, dec_lat_va, dec_long_va,
                dec_coord_datum_cd, alt_va, huc_cd, data_type_cd,
                parm_cd, stat_cd, begin_date:count_nu) %>%
  # rename cols
  dplyr::rename(interval=data_type_cd, lat = dec_lat_va, lon=dec_long_va,
                huc8=huc_cd, site_id=site_no, date_begin=begin_date,
                date_end=end_date, datum=dec_coord_datum_cd, elev_m=alt_va) %>%
  # filter missing vals
  dplyr::filter(!is.na(lon)) %>%
  # now make sure spatially distinct
  dplyr::distinct(site_id, .keep_all=TRUE) %>% 
  sf::st_as_sf(coords=c("lon","lat"), crs=4269, remove=FALSE)

# save out
save(ca_usgs_gages, file = "data/usgs/usgs_ca_all_daily_flow_gages.rda")


# view
load("data/usgs/usgs_ca_all_daily_flow_gages.rda")

library(mapview)
mapview(ca_usgs_gages)


# Mapdeck -----------------------------------------------------------------


library(sf)
library(mapdeck)
set_token(Sys.getenv("MAPBOX_TOKEN")) # from usethis::edit_r_environ() add token

# make map
mapdeck(style = 'mapbox://styles/mapbox/dark-v9', pitch = 0) %>%
  add_scatterplot(
    data = ca_usgs_gages, 
    lat = "lat", 
    lon = "lon",
    radius = 1000,
    fill_colour = viridis::viridis(4)[3],
    tooltip="station_nm",
    layer_id = "gages",
    auto_highlight = TRUE
    )


# make map
mapdeck(style = 'mapbox://styles/mapbox/dark-v9', pitch = 0) %>%
  mapdeck::add_pointcloud(
    data = ca_usgs_gages, 
    lat = "lat", 
    lon = "lon",
    elevation = "count_nu",
    fill_colour = "count_nu",
    tooltip="station_nm",
    layer_id = "gages",
    highlight_colour = viridis::viridis(2)[1],
    auto_highlight = TRUE,
    radius = 4
  )


