# drought index
# Evaporative Demand Drought Index (EDDI)
# https://www.earthdatascience.org/eddi/articles/eddi-roi-tutorial.html
# examines how anomalous the atmospheric evaporative demand (E0; also known as "the thirst of the atmosphere") is for a given location and across a time period of interest

library(eddi)

eddi_data <- get_eddi(date = "2019-11-17", timescale = "1 month")
eddi_data

# plot
color_pal <- colorRampPalette(c("blue", "lightblue", "white", "pink", "red"))
raster::plot(eddi_data, col = color_pal(255), main = "EDDI data for 2019-11-15")

# weekly
eddi_data2 <- get_eddi(date = "2019-11-10", timescale = "1 week")

raster::plot(eddi_data2, col = color_pal(255), main = "EDDI data for 2019-11-10")
