
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)
library(mapview)
library(janitor)
library(lubridate)

# CSCI package
library(CSCI)
library(BMIMetrics)

# Read in COMIDs ----------------------------------------------------------

comid <- read_excel("data/BMI_COMIDs_for_Ryan.xlsx")

# Read in BMI data --------------------------------------------------------

bugs <- read_csv(file = "data/Taxonomy_Ryan_SCCWRP.csv.zip") %>% 
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

# how many missing Sampledates?
sum(is.na(bugs$sampledate))

# fix the SampleID: recommended format is "stationcode_sampledate_collectionmethodcode_fieldreplicate"
bugs$SampleID_rev <- with(bugs, paste0(StationCode, "_", 
                                       year(sampledate), sprintf(fmt = '%02d', month(sampledate)), 
                                       sprintf(fmt = '%02d', day(sampledate)), "_",
                                       collectionmethodcode, "_",
                                       replicate))

sum(is.na(bugs$SampleID_rev))

# drop the old sampleID and rename revised/new one
bugs <- bugs %>% select(StationCode:stationid, SampleID_rev, everything(), -SampleID) %>% 
  dplyr::rename(SampleID=SampleID_rev)

# range of data
summary(bugs$sampledate)

# Notes from Rafi ---------------------------------------------------------

### FROM RAFI ON 3/8/2019
# CollectionMethodCode: The method used to collect the data (rarely, we have multiple methods at same site and date)
# These are “targeted riffle” methods (riffles or other “rich” habitat types are targeted for sampling): BMI_TRC, BMI_CSBP_Trans, BMI_CSBP_Comp, BMI_SNARL

# locationcode: A few of the targeted riffle methods collect multiple samples from different riffle “locations” (indicated by locationcode). *I recommend summing up all locations within a sample.*

# collectionmethodcode: These are “reachwide” methods (where locations are sampled in more representative, systematic fashion, e.g., at 25%, 50%, or 75% of stream-width at 11 transects): BMI_RWB, BMI_RWB_MCM, BMI_RWM_MCM

# Some methods have a target count of 600 organisms (BMI_RWB_MCM, BMI_RWM_MCM). Some methods have larger targeted counts of ~900. The SNARL method does a full count. So, when combining data, you will want to do some subsampling to control for those differences. We have functions embedded within the CSCI package that can help with this."


# Clean with CSCI ---------------------------------------------------------

# look at different collection methods?
table(bugs$collectionmethodcode)

# use CSCI to clean data
bugs_filt<-cleanData(bugs, purge = T) # only 8 ID problems, drops about 8 rows

# check problemFinalID col:
summary(bugs_filt$problemFinalID) # only 8 ID problems
summary(bugs_filt$fixedLifeStageCode) # merges/sums the lifestage codes

# Filter to Reachwide Methods ---------------------------------------------

# filter to include "reachwide methods" only:
# bugs_filt <- bugs_filt %>% 
#   filter(collectionmethodcode %in% c("BMI_RWB", "BMI_RWB_MCM", "BMI_RWM_MCM"))
#                                      #"BMI_SNARL", "BMI_TRC"))

table(bugs_filt$collectionmethodcode)

# see how many distinct sites by date/stationid
sample_distinct <- bugs_filt %>% 
  distinct(StationCode, sampledate, collectionmethodcode, replicate) %>% 
  mutate("YYYY"=year(sampledate),
         "MM" = month(sampledate),
         "DD" = day(sampledate))

# write out and send to RAF
# write_csv(sample_distinct, path = "data_output/sample_list_for_csci.csv")
# save(sample_distinct, file = "data_output/sample_list_for_csci.rdata")

# Make a Distinct Station List  ---------------------------------------------

# make a station list for a map
bug_stations <- bugs_filt %>%  # get distinct stations and locations
  distinct(StationCode, longitude, latitude, collectionmethodcode, sampledate, county) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) # make spatial


# Make Maps ---------------------------------------------------------------

# mapview of collection methods
mapview(bug_stations, zcol="collectionmethodcode", 
        #col.regions="orange", 
        layer="Benthos", cex=4, alpha=0.8)

# static tmap of collection methods
library(tmap)
ca <- USAboundaries::us_states(resolution="low", states = "ca")

# first make CA map with no border
map_ca <- tm_shape(ca) + tm_polygons() + 
  tm_compass(type = "arrow", position = c("right", "top"), size = 1.5) +
  tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 100, 200), text.size = 0.4) +
  tm_layout(frame=FALSE) #+
map_ca  

# then add bug stations by collection method
map_ca + tm_shape(bug_stations) +
  tm_symbols(col="collectionmethodcode", border.col = "gray30", size=0.4) +
  tm_facets(by = "collectionmethodcode", nrow = 2,free.coords = FALSE) + 
  tm_layout(legend.show = F, legend.outside = TRUE, 
            #legend.outside.position = c(0.5, 0.2), 
            legend.outside.size = 0.4)



# Generate Metrics --------------------------------------------------------

# generate test dataset of subset
bug_rwm <- filter(bugs_filt, collectionmethodcode=="BMI_RWM_MCM") %>% as.data.frame()

  # using CSCI package, coerce data into a “BMI” data object for generating metrics
bugdata <- BMI(bug_rwm)

# #Subsample to 500 individuals and aggregate
bugdata_samp <- sample(bugdata)
bugdata_agg <- aggregate(bugdata_samp) 


# Calculate metrics at SAFIT Level 1
bug_metrics <- BMIall(bugdata_agg, effort=1)
