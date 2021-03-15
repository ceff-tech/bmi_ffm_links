# PHAB 


# Libraries ---------------------------------------------------------------

library(vroom) # fast file reading!
library(tidyverse)
library(tidylog)
library(readxl)
library(sf)
library(mapview)
mapviewOptions(fgb = FALSE)
library(janitor)
library(lubridate)

# Load selected site localities:
load("data_output/02c_selected_final_bmi_dat_all.rda")

# now filter to just unique SAMPLE ID
bmi_selected <- bmi_final_dat %>% select(StationCode:HUC_12, CEFF_type:COMID_bmi,sampledate, MM:DD, geometry) %>% 
  group_by(SampleID) %>% distinct(.keep_all=TRUE)

bmi_stations <- bmi_selected %>% ungroup() %>% distinct(StationCode)

# Read GIANT Data ---------------------------------------------------------

# unzipped the 7zip file, then zipped to simple .zip file
#phab <- vroom::vroom(file = "data/phab-rafi-15apr19.csv.zip")

# Filter to Sites ---------------------------------------------------------

# phab_sel <- phab %>% filter(stationid %in% bmi_stations$StationCode)

# write back out!
# write_csv(phab_sel, file="data/phab_bmi_selected.csv") # this is large
# save(phab_sel, file="phab_bmi_selected.rda", compress = "xz") # this is 3.6 MB

# Read back in Small File -------------------------------------------------

# this is still a large file, takes a minute to load
load("data/phab_bmi_selected.rda")

# filter out NA values in results
phab_sel_filt <- phab_sel %>% filter(!is.na(result)) %>% 
  filter(matrixname == "habitat")

# join with BMI data
phab_sel_filt <- left_join(phab_sel_filt, bmi_selected, by=c("stationid"="StationCode"))

# look at possible variables that exist
table(phab_sel_filt$analytename)
# Canopy Cover
# Embeddedness
# Bankfull Width
# Slope
# Wetted Width
# Substrate Size Class

phab_sel_filt %>% 
  filter(analytewfractionwmatrixwunit == "Substrate Size Class,None,habitat,mm") %>% 
  ggplot() + geom_histogram(aes(x=result))

phab_sel_filt %>% 
  filter(analytename == "Embeddedness") %>% 
  ggplot() + geom_histogram(aes(x=result))

phab_sel_filt %>% 
  filter(analytename == "Embeddedness") %>% # try 
  ggplot() + geom_point(aes(x=result, y=csci, fill=CEFF_type), pch=21, size=3.5, alpha=0.7) +
  scale_fill_viridis_d() +
  stat_smooth(aes(x=result, y=csci, group=CEFF_type, color=CEFF_type), method = "glm") +
  scale_color_viridis_d() +
  labs(x="Embeddedness") +
  theme_classic()

phab_sel_filt %>% 
  filter(analytename == "Bankfull Width") %>% # try 
  ggplot() + geom_point(aes(x=result, y=csci, fill=CEFF_type), pch=21, size=3.5, alpha=0.7) +
  scale_fill_viridis_d() +
  stat_smooth(aes(x=result, y=csci, group=CEFF_type, color=CEFF_type), method = "glm") +
  scale_color_viridis_d() +
  labs(x="Bankfull Width") +
  theme_classic()

phab_sel_filt %>% 
  filter(analytename == "Wetted Width") %>% # try 
  ggplot() + geom_point(aes(x=result, y=csci, fill=CEFF_type), pch=21, size=3.5, alpha=0.7) +
  scale_fill_viridis_d() +
  stat_smooth(aes(x=result, y=csci), method = "glm") +
  scale_color_viridis_d() +
  xlim(0,40)+
  labs(x="Wetted Width") +
  theme_classic()


phab_sel_filt %>% 
  filter(analytename == "Substrate Size Class") %>% # try 
  ggplot() + geom_point(aes(x=result, y=csci, fill=CEFF_type), pch=21, size=3.5, alpha=0.7) +
  scale_fill_viridis_d() +
  stat_smooth(aes(x=result, y=csci, group=CEFF_type, color=CEFF_type), method = "glm") +
  scale_color_viridis_d() +
  #xlim(0,40)+
  labs(x="Substrate Size Class") +
  theme_classic()

phab_sel_filt %>% 
  filter(analytename == "Canopy Cover") %>% # try 
  ggplot() + geom_point(aes(x=result, y=csci, fill=CEFF_type), pch=21, size=3.5, alpha=0.7) +
  scale_fill_viridis_d() +
  stat_smooth(aes(x=result, y=csci, group=CEFF_type, color=CEFF_type), method = "glm") +
  scale_color_viridis_d() +
  #xlim(0,40)+
  labs(x="Canopy Cover") +
  theme_classic()

