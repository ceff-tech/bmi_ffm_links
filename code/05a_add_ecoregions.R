

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
    US_L3NAME == "Central Basin and Range" ~ "Cascades/Basin Range",
    US_L3NAME == "Cascades" ~ "Cascades/Basin Range",
    US_L3NAME == "Southern California Mountains" ~ "Southern California",
    US_L3NAME == "Southern California/Northern Baja Coast" ~ "Southern California",
    US_L3NAME =="Klamath Mountains/California High North Coast Range" ~ "Coast Range",
    TRUE ~ US_L3NAME))

table(bmi_sf_mod$US_L3_mod)

mapview(bmi_sf_mod, zcol="US_L3_mod") + mapview(ecoregs_ca, zcol="US_L3NAME", cex=0.4)

# create combined ecoregs
ecoregs_ca_comb <- ecoregs_ca %>% 
  mutate(US_L3_mod = case_when(
    US_L3CODE %in% c(13, 80, 4, 9) ~ "Cascades/Basin Range",
    US_L3CODE %in% c(14, 81) ~ "Mojave/Sonoran Desert",
    US_L3CODE %in% c(8, 85) ~ "Southern California",
    US_L3CODE %in% c(1, 78) ~ "North Coast",
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

# draw line to split coast from sierras through center of valley and exiting at top and bottom of Sierra/Foothills
ecoregs_edit <- mapedit::editFeatures(ecoregs_ca_comb[5,], editor = "leafpm")

# extract pieces
mapview(ecoregs_edit$geometry[2,])

# extract
tst <- st_collection_extract(lwgeom::st_split(ecoregs_ca_comb[5,], ecoregs_edit$geometry[2,]),"POLYGON") %>% rename(geometry=Shape)
mapview(tst$geometry[4,]) + # coast
  mapview(tst$geometry[3,], col.regions="yellow") # foothills

# merge with Sierras
mapview(ecoregs_ca_comb[5,]) + mapview(tst[3,], col.regions="yellow")

# save out temp:
foothillsn_ecoregs <- tst[3,]
foothillcoast_ecoregs <- tst[4,]
save(foothillsn_ecoregs, file="data/spatial/05a_foothillsn_ecoreg_L3.rda")

# merge to get all Sierras
snevada_combine <- st_union(ecoregs_ca_comb[4,], foothillsn_ecoregs) %>% 
  rename(geometry=Shape) %>% select(-US_L3_mod.1)
mapview(snevada_combine)

# drop the sierras and coast
eco_trim <- ecoregs_ca_comb[-c(4, 5),] %>% rename(geometry=Shape)
mapview(eco_trim)

# now merge
eco_revised <- rbind(eco_trim, foothillcoast_ecoregs, snevada_combine)
mapview(eco_revised)

# SAVE THIS OUT
st_write(eco_revised, "data/spatial/ecoregions_combined_L3.shp")
write_rds(eco_revised, "data/spatial/ecoregions_combined_L3.rds")

# Merge Sierras w Cascades ------------------------------------------------

# map it!
mapview(eco_revised, layer.name="Revised Ecoregs") + mapview(ecoregs, zcol="US_L3NAME", alpha.regions=0.2, layer.name="Ecoregions L3") +
  mapview(bmi_sf_mod, col.regions="orange", layer.name="BMI Sites") 

# Get HUCs and Simplify ---------------------------------------------------

# h12 <- st_read(gpkg_name, "CA_HUC12")
# pryr::object_size(h12)
# 
# # dissolve by attribute column
# # rmapshaper::ms_dissolve(h12, field = "field")
# 
# # simplify to avoid giant file
# h12s <- rmapshaper::ms_simplify(h12, keep = .1)
# pryr::object_size()

# Get Ecoregs to BMI data -------------------------------------------------

bmi_final_dat_ecoreg <- st_join(bmi_sf, left = FALSE, eco_revised["US_L3_mod"])

table(bmi_final_dat_ecoreg$US_L3_mod)
