

# Libraries ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(janitor)
library(rmapshaper)
library(mapview)
mapviewOptions(fgb = FALSE)


# Get State Boundary ------------------------------------------------------

ca <- USAboundaries::us_states(states="california")


# Sites -------------------------------------------------------------------

load("data_output/02c_selected_final_bmi_dat_all.rda")


# Get Ecoregions ----------------------------------------------------------

gpkg_name <- "data/healthy_watersheds/CA_PHWA_Geodatabase_170518.gdb/"
st_layers(gpkg_name)

# get CA ecoregions
ecoregs <- st_read(gpkg_name, "CA_Ecoregions") %>% st_transform(4326)
pryr::object_size(ecoregs)

# crop to CA
ecoregs_ca <- st_intersection(ecoregs, ca)

# map
#mapview(ecoregs_ca, zcol="US_L3NAME", cex=0.4)

# Join Ecoregions to BMI sites --------------------------------------------

# get unique BMI sites only
bmi_sf <- bmi_final_dat %>% distinct(StationCode, .keep_all = TRUE)

bmi_sf <- st_join(bmi_sf, left = TRUE, ecoregs_ca["US_L3NAME"])

# summarize?
table(bmi_sf$US_L3NAME)

#mapview(bmi_sf, zcol="US_L3NAME") + mapview(ecoregs_ca, zcol="US_L3NAME", cex=0.4)

# Combine Ecoregions -----------------------------------------------------

## COMBINE or drop if less than 9 sites?
# Cascades (n=2) + Sierra Nevada (n=43)?
# Sonoran Basin and Range (n=4) + Mojave Basin and Range (n=1) >>> DROP?
# Coast Range (n=15) + Klamath Mountains/California High North Coast Range (n=3)
# Southern California/Baja (n=108) + Southern California Mountains (n=19)
# Central Basin Range (n=9)

# use case_when to assign to diff Ecoregs
bmi_sf_mod <- bmi_sf %>% 
  mutate(US_L3_mod = case_when(
    # move Sierra foothills to Sierra Nevada
    StationCode %in% c("552PS0284","539PS0146","514PS0351", "514CE0171",
                       "520PS0199", "521PS0663","521BCRBRx") ~ "Sierra Nevada",
    StationCode == "609PS0053" ~ "Desert",
    US_L3NAME =="Sonoran Basin and Range" ~ "Desert",
    US_L3NAME =="Cascades" ~ "Sierra Nevada",
    US_L3NAME =="Klamath Mountains/California High North Coast Range" ~ "Coast Range",
    TRUE ~ US_L3NAME))

table(bmi_sf_mod$US_L3_mod)

#mapview(bmi_sf_mod, zcol="US_L3_mod") + mapview(ecoregs_ca, zcol="US_L3NAME", cex=0.4)

# create combined ecoregs
ecoregs_ca_comb <- ecoregs_ca %>% 
  mutate(US_L3_mod = case_when(
    US_L3CODE %in% c(4, 9) ~ "Cascades",
    US_L3CODE %in% c(14, 81) ~ "Mojave/Sonoran Desert",
    US_L3CODE %in% c(8, 85) ~ "Southern California",
    US_L3CODE %in% c(1, 78) ~ "North Coast",
    US_L3CODE %in% c(13, 80) ~ "Basin Range",
    TRUE ~ US_L3NAME))

mapview(bmi_sf_mod, col.regions="orange") + mapview(ecoregs_ca_comb, zcol="US_L3_mod", cex=0.4)

# dissolve by attribute column
ecoregs_ca_comb <- ecoregs_ca_comb %>% rmapshaper::ms_dissolve(., field = "US_L3_mod")

# simplify
ecoregs_ca_comb <- ecoregs_ca_comb %>% rmapshaper::ms_simplify(keep=0.1)

(m1 <- mapview(ecoregs_ca_comb, zcol="US_L3_mod", cex=0.4))

# split out the foothills from the Coastal foothills
library(mapedit)
library(leafpm)

ecoregs_edit <- mapedit::editFeatures(ecoregs_ca_comb[6,], editor = "leafpm")

# extract pieces
mapview(ecoregs_edit$geometry[2,])

# intersect to get just foothills
tst <- st_intersection(ecoregs_ca_comb[6,], ecoregs_edit$geometry[2,])
mapview(tst)

# merge with Sierras
mapview(ecoregs_ca_comb[5,]) + mapview(tst, col.regions="yellow")

# merge to get all Sierras
snevada_combine <- st_union(ecoregs_ca_comb[5,], tst) %>% 
  rename(geometry=Shape) %>% select(-US_L3_mod.1)
mapview(snevada_combine)

# intersect to cut coast out from tst
coastal_foothills <- ecoregs_ca_comb[6,]
coastal_foothills_v1 <- st_difference(coastal_foothills, snevada_combine) %>% rename(geometry=Shape) %>% select(-US_L3_mod.1)
mapview(coastal_foothills_v1)

# map
mapview(ecoregs_ca_comb, zcol="US_L3_mod")+ mapview(snevada_combine)

# drop the sierras and coast
eco_trim <- ecoregs_ca_comb[-c(5, 6),] %>% rename(geometry=Shape)
mapview(eco_trim)

# now merge
eco_revised <- rbind(eco_trim, coastal_foothills_v1, snevada_combine)

mapview(eco_revised)

# SAVE THIS OUT
st_write(eco_revised, "data/spatial/ecoregions_combined_L3.shp")
write_rds(eco_revised, "data/spatial/ecoregions_combined_L3.rds")

# Get HUCs and Simplify ---------------------------------------------------

h12 <- st_read(gpkg_name, "CA_HUC12")
pryr::object_size(h12)

# dissolve by attribute column
# rmapshaper::ms_dissolve(h12, field = "field")

# simplify to avoid giant file
h12s <- rmapshaper::ms_simplify(h12, keep = .1)
pryr::object_size()
