library(mapdeck)
library(dplyr)
#library(sfheaders)

# set api key
# library(usethis)
# usethis::edit_r_environ() # add mapbox token https://account.mapbox.com/
# check tokens
# Sys.getenv("MAPBOX_TOKEN")
set_token(Sys.getenv("MAPBOX_TOKEN"))

sf_rds <- mapdeck::roads
#df <- sfheaders::sf_to_df(sf, fill=TRUE)

mapdeck(
  style=mapdeck_style("dark")
  ) %>% 
  add_path(data = sf_rds, stroke_colour = "EZI_RDNAME", tooltip="EZI_RDNAME")



# Arcs --------------------------------------------------------------------

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv'
flights <- read.csv(url)
flights$id <- seq_len(nrow(flights))
flights <- flights %>% 
  mutate(stroke = sample(1:3, size = nrow(flights), replace = T),
         info = glue::glue("<b>{airport1} - {airport2}</b>"))

# make map
mapdeck(style = 'mapbox://styles/mapbox/dark-v9') %>%
  add_arc(
    data = flights, 
    layer_id = "arc_layer",
    origin = c("start_lon", "start_lat"),
    destination = c("end_lon", "end_lat"),
    stroke_from = "airport1",
    stroke_to = "airport2",
    stroke_width = "stroke",
    tooltip="info",
    auto_highlight = TRUE,
    legend=list(stroke_from=TRUE, stroke_to=FALSE))


# Grid --------------------------------------------------------------------


df <- read.csv(paste0(
  'https://raw.githubusercontent.com/uber-common/deck.gl-data/master/',
  'examples/3d-heatmap/heatmap-data.csv'
))

df <- df[!is.na(df$lng), ]

mapdeck(style = mapdeck_style("dark"), pitch = 45 ) %>%
  add_grid(
    data = df
    , lat = "lat"
    , lon = "lng"
    , cell_size = 5000
    , elevation_scale = 50
    , layer_id = "grid_layer"
  )



# Hex ---------------------------------------------------------------------



library(mapdeck)

df2 <- read.csv(paste0(
  'https://raw.githubusercontent.com/uber-common/deck.gl-data/master/examples/'
  , '3d-heatmap/heatmap-data.csv'
))

df2 <- df2[!is.na(df$lng), ]

mapdeck( style = mapdeck_style("dark"), pitch = 45) %>%
  add_hexagon(
    data = df2[ df2$lat > 54.5, ]
    , lat = "lat"
    , lon = "lng"
    , layer_id = "hex_layer"
    , elevation_scale = 100
    , colour_range = colourvalues::colour_values(1:6, palette = colourvalues::get_palette("viridis")[70:256,])
  )
