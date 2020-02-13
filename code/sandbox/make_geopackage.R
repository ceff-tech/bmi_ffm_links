# convert data to geopackage:

library(sf)
library(tidyverse)


# Read shps ---------------------------------------------------------------

gages_ucd_ref <- st_read("data/UCD_Ref_Gages.shp")
sccwrp_flowlines_hydro_class <- st_read("data/SCCWRP_flowslines_CA_hydro_classification.shp") %>% st_transform(4326)
h6 <- st_read("data/HUC6s.shp") %>% st_transform(4326)
h12 <- st_read("data/HUC12s.shp") %>% st_transform(4326)
# h8 <- read_sf(unzip("data/HUC8_named_westcoast.zip"), quiet=TRUE)
# file.remove(list.files(pattern="HUC8_named_westcoast*", recursive = F))
h8 <- st_read("data/HUC8_named_westcoast.shp")

# Read tables -------------------------------------------------------------

runoff_ca <- read_csv("data/Runoff_CA.csv")
road_stream_crossings_ca <- read_csv("data/RoadStreamCrossings_CA.csv")
road_density_ca <- read_csv("data/RoadDensity_CA.csv")
road_density_ripbuffer <- read_csv("data/RoadDensityRipBuf100_CA.csv")
pesticides97_ca <- read_csv("data/Pesticides97_CA.csv")
gages_final_ref_20180703 <- read_csv("data/Final_Reference_Gages_07032018.csv")
gages_ref_20190315 <- read_csv("data_output/ref_gages_2019_03_15.csv")
dams_ca <- read_csv("data/Dams_CA.csv")


# Write gpkg --------------------------------------------------------------


# Write shps to gpkg
st_write(gages_ucd_ref, dsn="data_output/eflows_bmi.gpkg", layer='gages_ucd_ref')
st_write(sccwrp_flowlines_hydro_class, dsn="data_output/eflows_bmi.gpkg", layer='sccwrp_flowlines_hydro_class')
st_write(h8, dsn="data_output/eflows_bmi.gpkg", layer="huc8")
st_write(h6, dsn="data_output/eflows_bmi.gpkg", layer="huc6")
st_write(h12, dsn="data_output/eflows_bmi.gpkg", layer="huc12")

# write tables to gpkg

# using dplyr (instead of library(RSQLite))
# dbcon <- dbConnect(dbDriver("SQLite"), db)
# dbListTables(dbcon) # list all tables in DB
dbcon <- src_sqlite("data_output/eflows_bmi.gpkg", create = F) 
src_tbls(dbcon) # see tables in DB

# copy tables
copy_to(dbcon, gages_final_ref_20180703, temporary = FALSE) 
copy_to(dbcon, gages_ref_20190315, temporary = FALSE) 
copy_to(dbcon, dams_ca, temporary = FALSE) 
copy_to(dbcon, runoff_ca, temporary = FALSE) 
copy_to(dbcon, road_density_ca, temporary = FALSE)
copy_to(dbcon, road_density_ripbuffer, temporary = FALSE)
copy_to(dbcon, road_stream_crossings_ca, temporary = FALSE)
copy_to(dbcon, pesticides97_ca, temporary = FALSE)
