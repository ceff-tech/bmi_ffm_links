
# Libraries ---------------------------------------------------------------

library(sf)
options(tidyverse.quiet = TRUE)
library(tidyverse) # load quietly
library(conflicted) # deals with conflicting functions
conflict_prefer("filter", "dplyr")
library(janitor)
library(mapview)
mapviewOptions(fgb = FALSE)

# Get Spatial Data --------------------------------------------------------

eco_revised <- read_rds("data/spatial/ecoregions_combined_L3.rds")
ca <- USAboundaries::us_states(states="california")

# Sites -------------------------------------------------------------------

bmi_csci_por_trim <- read_rds("data_output/04_selected_csci_ffm_por_trim.rds")
length(unique(bmi_csci_por_trim$site_id)) #Gages (n=209)
length(unique(bmi_csci_por_trim$StationCode)) # BMI Stations (n=246)

# all data: don't really need this
load("data_output/02c_selected_final_bmi_dat_all.rda")

# Join Ecoregions to BMI sites --------------------------------------------

# get unique BMI sites only
bmi_sf <- bmi_final_dat %>% distinct(StationCode, .keep_all = TRUE)
bmi_sf <- st_join(bmi_sf, left = TRUE, eco_revised["US_L3_mod"])

# summarize BMI stations by region
table(bmi_sf$US_L3_mod)

mapview(bmi_sf, zcol="US_L3_mod", layer.name="BMI Sites") + mapview(eco_revised, zcol="US_L3_mod", cex=0.2, layer.name="Ecoregions")


# Add Ecoregions to BMI FFC Status Data -----------------------------------

bmi_csci_por_trim_sf <- st_as_sf(bmi_csci_por_trim, coords=c("lon", "lat"), crs=4326, remove=F)
bmi_csci_por_trim_ecoreg <- st_join(bmi_csci_por_trim_sf, left = FALSE, eco_revised["US_L3_mod"])

table(bmi_csci_por_trim_ecoreg$US_L3_mod)

save(bmi_csci_por_trim_ecoreg, file="data_output/05_bmi_csci_por_trim_ecoreg.rda")

summary(bmi_csci_por_trim_ecoreg)

bmi_csci_por_trim_ecoreg %>% st_drop_geometry() %>% group_by(metric) %>% tally(status_code)
