### Read in BMI Data and Clean
## R. Peek
## Filters data for BMI metric generation for use in eflow usgs pairing: 
## DATA OUT:
### - bmi_clean (cleaned bmi data, used to generate metrics, n=310,216)

# Libraries ---------------------------------------------------------------

library(vroom) # fast file reading!
library(tidyverse)
library(tidylog)
library(readxl)
library(sf)
library(mapview)
library(janitor)
library(lubridate)

# CSCI package
library(CSCI)
library(BMIMetrics)

# Read in COMIDs ----------------------------------------------------------

comid <- read_excel("data/bmi/bmi_comids_for_ryan.xlsx")

# Read in BMI data --------------------------------------------------------

#library(bench) # benchmark times
## using VROOM!
# workout({bugs_v <- vroom::vroom(file = "data/Taxonomy_Ryan_SCCWRP.csv.zip")}, description = "vroom")
## using read_csv
# workout({bugs <- read_csv(file = "data/Taxonomy_Ryan_SCCWRP.csv.zip")},
#         description = "readr") 

# READ IN FAST!
bugs <- vroom::vroom(file = "data/Taxonomy_Ryan_SCCWRP.csv.zip") %>% 
  dplyr::rename("StationCode" = stationcode, "SampleID" = sampleid, 
                "FinalID" = finalid, "BAResult" = baresult, "LifeStageCode" = lifestagecode) %>% 
  mutate("Distinct" = NA) %>% # add for CSCI package
  select(StationCode, stationid, SampleID, everything()) # rearrange

# Clean Data --------------------------------------------------------------

# use lubridate/tidyr to fix dates to a consistent format
bugs <- separate(bugs, col = sampledate, into=c("AA", "BB", "YYYY"), remove = T) %>% 
  mutate(AA=as.integer(AA),
         BB=as.integer(BB)) %>% 
  mutate(
    "MM"=case_when(
      AA > 12 ~ BB,
      AA < 13 ~ AA),
    "DD" = case_when(
      AA > 12 ~ AA,
      AA < 13 ~ BB),
    "YYYY" = as.integer(YYYY),
    "sampledate" = ymd(paste0(YYYY,"-", MM, "-",DD))
  ) %>% 
  dplyr::select(StationCode:YYYY, MM, DD, sampledate, everything(), -c(AA, BB))

# how many missing SampleID's?: 7991
sum(is.na(bugs$SampleID))

# how many missing Sampledates?: 0
sum(is.na(bugs$sampledate))

# fix the SampleID: recommended format is "stationcode_sampledate_collectionmethodcode_fieldreplicate"
bugs$SampleID_rev <- with(bugs, 
                          paste0(StationCode, "_", 
                                 sprintf(fmt = '%02d', month(sampledate)),
                                 sprintf(fmt = '%02d', day(sampledate)),
                                 year(sampledate), 
                                 "_", collectionmethodcode, "_", replicate))

sum(is.na(bugs$SampleID_rev)) # should be zero

# drop the old sampleID and rename revised/new one
bugs <- bugs %>% select(StationCode:stationid, SampleID_rev, everything(), -SampleID) %>% 
  dplyr::rename(SampleID=SampleID_rev)

# range of data (1994 to 2018)
summary(bugs$sampledate)

# Notes from Rafi ---------------------------------------------------------

### FROM RAFI ON 3/8/2019
# CollectionMethodCode: The method used to collect the data (rarely, we have multiple methods at same site and date)
# These are “targeted riffle” methods (riffles or other “rich” habitat types are targeted for sampling): BMI_TRC, BMI_CSBP_Trans, BMI_CSBP_Comp, BMI_SNARL

# locationcode: A few of the targeted riffle methods collect multiple samples from different riffle “locations” (indicated by locationcode). *I recommend summing up all locations within a sample.*

# collectionmethodcode: These are “reachwide” methods (where locations are sampled in more representative, systematic fashion, e.g., at 25%, 50%, or 75% of stream-width at 11 transects): BMI_RWB, BMI_RWB_MCM, BMI_RWM_MCM

# Some methods have a target count of 600 organisms (BMI_RWB_MCM, BMI_RWM_MCM). Some methods have larger targeted counts of ~900. The SNARL method does a full count. So, when combining data, you will want to do some subsampling to control for those differences. We have functions embedded within the CSCI package that can help with this."

# Clean with CSCI Package ---------------------------------------------------------

# look at different collection methods?
table(bugs$collectionmethodcode)

# use CSCI to clean data
bmi_clean<-cleanData(bugs, purge = TRUE) # only 8 ID problems, drops about 8 rows

# check problemFinalID col:
summary(bmi_clean$problemFinalID) # only 8 ID problems, dropped above
summary(bmi_clean$fixedLifeStageCode) # merges/sums the lifestage codes

hist(bmi_clean$MM, main="Sampling Across Months", xlab="Months")

# Look at Distinct Sites by SampleID --------------------------------

# look at methods
table(bmi_clean$collectionmethodcode)

# see how many distinct sites by date/stationid (n=5662)
bmi_samples_distinct <- bmi_clean %>% 
  distinct(SampleID, .keep_all = TRUE) %>% 
  select(StationCode, SampleID, latitude, longitude, YYYY:DD)

# write out and send to RAF
# write_csv(sample_distinct, path = "data_output/sample_list_for_csci.csv")

# Make a Distinct Station List  ---------------------------------------------

bmi_stations_distinct <- bmi_clean %>%  # get distinct station locations
  distinct(StationCode, longitude, latitude) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) # make spatial

bmi_stations_distinct %>% distinct(StationCode) %>% tally() # n=2935

# Save Data ---------------------------------------------------------------

# save filtered/cleaned data
save(bmi_clean, file ="data_output/00_bmi_cleaned_all.rda")

# save distinct stations by XY
save(bmi_stations_distinct, file="data_output/00_bmi_stations_distinct.rda")

# save distinct samples by SampleID(stationcode_sampledate_collectionmethodcode_fieldreplicate")
save(bmi_samples_distinct, file="data_output/00_bmi_samples_distinct.rda")

# Join with CSCI Scores -----------------------------------------------
 
 # see what data exist against CSCI scores currently avail (from Raffi)
csci1 <- read_csv("data/csci/csci_core.csv") %>% 
  select(sampleid:sampledate, collectionmethodcode:fieldreplicate, count, mmi, csci:csci_percentile) %>% 
  dplyr::rename(station_code=stationcode) %>% 
  mutate(sampledate = as_date(sampledate))
csci2 <- read_csv("data/csci/csci_core_v2.csv") %>% clean_names() %>% 
  mutate(sampledate = mdy(sampledate)) %>% 
  select(station_code:count, csci:sampleid, mmi, csci,csci_percentile) %>% 
  select(names(csci1)) 

csci <- bind_rows(csci1, csci2)

# double check and resave the SampleID
csci <- csci %>% 
  mutate(MM = stringi::stri_pad_left(month(sampledate), 2, pad="0"),
         DD = stringi::stri_pad_left(day(sampledate), 2, pad="0"),
         YYYY = year(sampledate),
         SampleID = paste0(station_code,"_", MM,DD,YYYY, "_", collectionmethodcode, "_", fieldreplicate)) %>% 
  select(SampleID, sampledate:csci_percentile) %>% 
  filter(!is.na(csci)) %>% # drop NAs
  distinct(SampleID, .keep_all=TRUE) # remove duplicates
# check
summary(csci)
length(unique(csci$SampleID)) # n=4031

# match against existing samples:
bmi_samples_distinct_csci <- left_join(bmi_samples_distinct, csci, by=c("SampleID"))

# see how many CSCI missing? # n=2737 (48% of data)
bmi_samples_distinct_csci %>% filter(is.na(csci)) %>% tally()/nrow(bmi_samples_distinct_csci)

# save out
save(bmi_samples_distinct_csci, file = "data_output/00_bmi_samples_distinct_csci.rda")

# look at distrib through months and years
# ggplot() + 
#   geom_boxplot(data=bmi_samples_distinct_csci %>% filter(MM>4, MM<10), aes(x=YYYY, y=csci, group=YYYY), color="gray70") +
#   geom_jitter(data=bmi_samples_distinct_csci %>% filter(MM>4, MM<10), aes(x=YYYY, y=csci, group=YYYY, color=MM), alpha=0.5) +
#   scale_x_continuous(labels = c(seq(1994,2018,2)), breaks = c(seq(1994,2018,2))) +
#   labs(x="Sample Year", y="CSCI")+
#   coord_flip() +
#   facet_grid(.~MM)

#  * Plot CSCI Data by Month ----------------------------------------------

library(ggtext)

# # look at distrib through years
# ggplot() + 
#   geom_jitter(data=bmi_samples_distinct_csci, #%>% 
#               #filter(MM>4, MM<10), 
#               aes(x=as.factor(YYYY), y=csci, group=as.factor(YYYY), color=as.factor(MM)), alpha=0.7, show.legend=TRUE) +
#   scale_color_viridis_d("Month", option = "D") +
#   geom_violin(data=bmi_samples_distinct_csci, #%>% 
#               #filter(MM>4, MM<10), 
#               aes(x=as.factor(YYYY), y=csci, group=as.factor(YYYY)), color="gray40", alpha=0.2,draw_quantiles = c(0.25,0.5, 0.75)) +
#   theme_bw(base_family = "Roboto Condensed") +
#   labs(x="", y="CSCI", 
#        subtitle = "Raw CSCI Score by Year of Sample",
#        caption = "Data from SCCWRP & SWAMP\n<www.waterboards.ca.gov/water_issues/programs/swamp>") +
#   theme(
#     axis.text.x = element_text(angle=60, vjust=0.05, hjust=0.2))

#ggsave(filename = "figs/00_raw_csci_score_by_year.png", width = 9, height = 6, units = "in", dpi=300)

#  * Plot CSCI Data by Month ----------------------------------------------

library(ggtext)

# look at distrib through months
ggplot() + 
  geom_jitter(data=bmi_samples_distinct_csci, #%>% 
              #filter(MM>4, MM<10), 
              aes(x=as.factor(MM), y=csci, group=as.factor(MM)), color=viridis::viridis(3)[2], alpha=0.5, show.legend=FALSE) +
  geom_violin(data=bmi_samples_distinct_csci, #%>% 
                #filter(MM>4, MM<10), 
              aes(x=as.factor(MM), y=csci, group=as.factor(MM)), color="gray40", alpha=0.2,draw_quantiles = c(0.25,0.5, 0.75)) +
  theme_classic(base_family = "Roboto Condensed") +
  labs(x="Sample Month", y="CSCI", 
       subtitle = "**Raw CSCI Score by Month**",
       caption = "Data from SCCWRP & SWAMP\n<www.waterboards.ca.gov/water_issues/programs/swamp>") +
  theme(
    plot.subtitle = element_markdown(
      size = 12,
      lineheight = 1.1
    ))
ggsave(filename = "figs/00_raw_csci_score_by_month.png", width = 9, height = 6, units = "in", dpi=300)

# Make Maps ---------------------------------------------------------------

# make it spatial
bmi_samples_distinct_csci_sf <- bmi_samples_distinct_csci %>% 
  st_as_sf(., coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

# * Map of sites with CSCI scores -----------------------------------------
mapview(bmi_samples_distinct_csci_sf %>% filter(!is.na(csci)), zcol="csci",cex=4, alpha.regions=0.8,
        layer.name="BMI Samples w CSCI")


# * Map of sites colored by Collection Methods ------------------------------

# make diff set of data that includes collection methods
bmi_stations_methods_distinct <- bmi_clean %>%  # get distinct stations and locations
  distinct(StationCode, longitude, latitude, collectionmethodcode) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) # make spatial

# mapview of collection methods
mapview(bmi_stations_methods_distinct, zcol="collectionmethodcode", 
        layer="Benthos", cex=4, alpha=0.8)

# static tmap of collection methods
library(tmap)
ca <- USAboundaries::us_states(resolution="low", states = "ca")

# first make CA map with no border
map_ca <- tm_shape(ca) + tm_polygons() + 
  tm_compass(type = "arrow", position = c("right", "top"), size = 1.5) +
  tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 100, 200), text.size = 0.4) +
  tm_layout(frame=FALSE) #+
#map_ca  

# then add bug stations by collection method
(tm_ca_bmi_sites <- map_ca + tm_shape(bmi_stations_methods_distinct) +
  tm_symbols(col="collectionmethodcode", border.col = "gray30", size=0.4) +
  tm_facets(by = "collectionmethodcode", nrow = 2,free.coords = FALSE) + 
  tm_layout(legend.show = F, legend.outside = TRUE, 
            #legend.outside.position = c(0.5, 0.2), 
            legend.outside.size = 0.4))

#tmap_save(filename = "figs/00_bmi_station_by_methods.png", width = 11, height = 8, dpi = 300, units = "in")

# leaflet map:
tm_ca_bmi_sites <- map_ca + tm_shape(bmi_stations_methods_distinct) +
  tm_symbols(col="collectionmethodcode", border.col = "gray30", size=0.4) +
  tm_layout(legend.show = F, legend.outside = TRUE, 
            legend.outside.size = 0.4)

tmap_leaflet(tm_ca_bmi_sites)

# Generate Metrics --------------------------------------------------------

# generate test dataset of subset
# bug_trc <- filter(bmi_clean, collectionmethodcode=="BMI_TRC") %>% as.data.frame()
## using CSCI package, coerce data into a “BMI” data object for generating metrics
# bugdata <- BMI(bug_trc)
## Subsample to 500 individuals and aggregate
# bugdata_samp <- sample(bugdata)
# bugdata_agg <- aggregate(bugdata_samp) 
# # Calculate metrics at SAFIT Level 1
# bug_metrics <- BMIall(bugdata_agg, effort=1)
# bmi_trc <- bug_metrics
# save(bmi_trc, file = "data_output/bmi_trc_metrics_safit1.rda")

# Generate SAFIT with purrr -----------------------------------------------

## this takes a little while

# library(purrr)
# bugs_split <- bmi_clean %>% 
#   split(.$collectionmethodcode) %>% # split by collection method
#   map(~BMI(.x)) # make into BMI object
# 
# bugs_samp <- bugs_split %>% 
#   map(~sample(.x)) # subsample to 500 individuals and aggregate
# 
# bugs_agg <- bugs_samp %>% 
#   map(~aggregate(.x))
# 
# # Calculate metrics at SAFIT Level 1
# bug_metrics <- BMIall(bugdata_agg, effort=1)
