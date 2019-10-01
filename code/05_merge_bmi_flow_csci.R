# 05_merge_sites_by_year

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(tidyverse)

# Load Data ---------------------------------------------------------------

load("data_output/sel_bmi_and_gages.rda")
load("data_output/selected_h12_contain_bmi_gage.rda")
load("data_output/gages_nhd_flowlines_mainstems.rda")
load("data_output/bmi_cleaned_all.rda") # all data

# load mapview bases
# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Get BMI comids ----------------------------------------------------------

# all stations us of gage:
bmi_us_coms <- sel_bmi_gages %>% filter(comid %in% mainstems_us$nhdplus_comid)

# all stations 15km downstream on mainstem
bmi_ds_coms <- sel_bmi_gages %>% filter(comid %in% mainstems_ds$nhdplus_comid)

# combine US and DS
bmi_coms <- rbind(bmi_ds_coms, bmi_us_coms)

# potential sites:
#bmi_coms %>% View()

# rm old layer:
rm(bmi_ds_coms, bmi_us_coms)

# Combine Mainstem NHDlines -----------------------------------------------

mainstems_ds <- mainstems_ds %>% 
  mutate(main_dir = "DS")
mainstems_us <- mainstems_us %>% 
  mutate(main_dir = "US")

mainstems <- rbind(mainstems_ds, mainstems_us)

# remove old layers
rm(mainstems_ds, mainstems_us)

# Join BMI Sites with BMI Data --------------------------------------------

# pull BMI sites and get list of data, first join with orig full dataset:
bmi_dat <- inner_join(bmi_coms, bmi_clean, by="StationCode") %>% 
  select(-c(Eco_III_2010, benthiccollectioncomments, percentsamplecounted:gridsvolumeanalyzed, discardedorganismcount,
            benthiclabeffortcomments, resqualcode:personnelcode_labeffort, samplecomments, effortqacode))
  # inner join drops NAs (72 sites: is.na(bmi_coms_dat$SampleID)

# now look at how many unique samples are avail: n=261 unique samples
bmi_dat %>% as.data.frame() %>% group_by(SampleID) %>% distinct(SampleID) %>% tally

# now look at how many unique stations: n=139 stations
bmi_dat %>% as.data.frame() %>% group_by(StationCode) %>% distinct(StationCode) %>% tally


# Merge with Flow Dat -----------------------------------------------------

load("data/ref_gage_annFlow_stats_long.rda")

# need to add "T" to the gageID
flow_long <- dat_long %>% mutate(ID=paste0("T", gage)) %>% 
  # filter out NA's?
  filter(!is.na(data)) %>% 
  ungroup() %>% 
  select(ID, gage, stream_class, stat:YrRange)

# rm old dataset
rm(dat_long)


# Avg Metrics for Period of Record ----------------------------------------

# set ID vars for flow metrics
flow_idvars <- flow_long %>% group_by(ID, stat) %>% distinct(ID, stat, maxYr, minYr)


# now avg for PERIOD OF RECORD for CSCI comparison
flow_por <- flow_long %>% select(-YrRange, -gage, -year, -stream_class) %>% group_by(ID, stat) %>% 
  summarize_at(vars(data), mean, na.rm=T) %>% 
  # rejoin yr ranges
  left_join(., flow_idvars)

# unique metrics? (should be 34)
length(unique(flow_por$stat))

# make wide for join
flow_wide <- flow_por %>% 
  pivot_wider(names_from=stat, values_from=data)
#values_fill=list(data=0))
# can fill missing values with zero if preferred: values_fill=list(data=0)

# check number unique gages:
flow_wide %>% distinct(ID) %>% dim # should be 223


# Get Flow Record only in Same Year/lag as BMI Sites ----------------------

# need to pull 2 years prior, 1 year prior, and same year as BMI site data

# make vector of years and BMI_ids
bmi_yrs <- bmi_dat %>% group_by(SampleID) %>% pull(YYYY) %>% unique()
(bmi_yrs_2 <- bmi_yrs - 2)
(bmi_yrs_1 <- bmi_yrs - 1)

# now combine and order:
bmi_years <- combine(bmi_yrs, bmi_yrs_1, bmi_yrs_2) %>% sort() %>% unique() # 25 total years to match with flow record
#bmi_years # 1993:2017

# rm old files
rm(bmi_yrs, bmi_yrs_1, bmi_yrs_2)

# now match with flow data (only goes through 2016)
flow_by_years_bmi <- flow_long %>% 
  filter(year %in% bmi_years) # 1993:2017

# make wide
flow_by_years_bmi_wide <- flow_by_years_bmi %>% 
  pivot_wider(names_from=stat, values_from=data)

flow_by_years_bmi_wide %>% distinct(ID) %>% dim # should be 106 gages match same years

# save flow data out for annual match
save(flow_by_years_bmi, flow_by_years_bmi_wide, file="data_output/selected_flow_by_years_of_bmi.rda")


# Get CSCI Data -----------------------------------------------------------

# see what data exist against CSCI scores currently avail (from Raffi)
csci <- read_csv("data/csci_core.csv")

# match against existing sites:
bmi_csci <- inner_join(bmi_coms, csci, by=c("StationCode"="stationcode"))

# how many unique matches?
length(unique(bmi_csci$StationCode))
table(bmi_csci$SiteStatus)

# look at CSCI
hist(bmi_csci$csci_percentile)

# look at CSCI percentile by Site Status (not avail for all sites)
ggplot() + geom_boxplot(data=bmi_csci, aes(x=SiteStatus, y=csci_percentile))

# function to get data
stat_box_data <- function(y, upper_limit = max(bmi_csci$csci_percentile)) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}

# plot CSCI percentile no NAs
ggplot(data=filter(bmi_csci, !is.na(SiteStatus)), aes(x=SiteStatus, y=csci_percentile)) + 
  geom_boxplot(aes(fill=SiteStatus), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text",cex=3, hjust=1, vjust=0.9) +
  ylab("CSCI (Percentile)") + xlab("Site Status")+
  theme_bw()

# plot CSCI percentile w/ NAs
ggplot(data=bmi_csci, aes(x=SiteStatus, y=csci_percentile)) + 
  geom_boxplot(aes(fill=SiteStatus), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text", cex=3, hjust=1, vjust=0.9) +
  ylab("CSCI (Percentile)") + xlab("Site Status")+
  theme_bw()

# Join with Flow POR ------------------------------------------------------

bmi_csci_flow <- left_join(bmi_csci, flow_wide, by="ID")

# filter to sites that have data in the flow time range?
bmi_csci_flow_yr_overlap <- bmi_csci_flow %>% 
  filter(sampleyear > minYr, sampleyear< maxYr)

length(unique(bmi_csci_flow_yr_overlap$StationCode))
length(unique(bmi_csci_flow_yr_overlap$ID))
class(bmi_csci_flow_yr_overlap)
#mapview(bmi_csci_flow_yr_overlap)


# Join with Flow by Years that Match/lag BMI -------------------------------

bmi_csci_flow_yrs <- left_join(bmi_csci, flow_by_years_bmi_wide, by=c("ID")) %>% 
  # filter to same year as BMI + 2 yr lag
  filter(sampleyear == year | sampleyear == year-1 | sampleyear==year-2)

# filter to sites that have data in the flow time range?
bmi_csci_flow_yrs %>% as.data.frame() %>% distinct(StationCode) %>% tally()
bmi_csci_flow_yrs %>% as.data.frame() %>% distinct(ID) %>% tally()

# Export Cleaned Data -----------------------------------------------------

save(bmi_coms, file="data_output/selected_bmi_stations_w_comids.rda")
save(bmi_csci_flow, file="data_output/selected_bmi_stations_w_csci_flow_por.rda")
save(bmi_csci_flow_yrs, file="data_output/selected_bmi_stations_w_csci_flow_annual.rda")
save(bmi_dat, file="data_output/selected_bmi_stations_w_data.rda")
flow_por_wide <- flow_wide; rm(flow_wide)
save(flow_por_wide, flow_por, file="data_output/selected_usgs_flow_metrics_POR.rda")
save(mainstems, file="data_output/maintems_us_ds_selected_gages.rda")

# PCA ---------------------------------------------------------------------

# simple PCA
# select vars
bmi_csci_pcs <- bmi_csci_flow_yr_overlap %>% st_drop_geometry() %>% 
  select(StationCode, comid, sampleid, ID, lat, lon, HUC_12, sampledate:fieldreplicate, count, pcnt_ambiguous_individuals:csci_percentile, sampleyear, Avg:WSI_Tim) %>% 
  drop_na()

csci_pr <- prcomp(bmi_csci_pcs[c(5:6,9,11,14:58)], center = TRUE, scale = TRUE)
summary(csci_pr)

# Since an eigenvalues <1 would mean that the component actually explains less than a single explanatory variable, discard those

screeplot(csci_pr, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(csci_pr$sdev^2 / sum(csci_pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.82, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6, 82% of var"),
       col=c("blue"), lty=5, cex=0.6)


plot(csci_pr$x[,1],csci_pr$x[,2], xlab="PC1 (28.8%)", ylab = "PC2 (22.2%)", main = "PC1 / PC2 - plot")


library("factoextra")
fviz_pca_ind(csci_pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = bmi_csci_pcs$Peak_Tim_5, 
             col.ind = "black", 
             #palette = "Dark2", 
             addEllipses = F,
             label = "var",
             col.var = "black",
             repel = TRUE, show.legend=FALSE,
             legend.title = "CSCI") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))


fviz_pca_ind(csci_pr, geom.ind = "point", axes = 2:3,
             pointshape = 21, 
             pointsize = 2, 
             fill.ind = bmi_csci_pcs$CV, 
             col.ind = "black", 
             #palette = viridis::magma(30), 
             addEllipses = F,
             label = "var",
             col.var = "black",
             repel = TRUE, show.legend=FALSE,
             legend.title = "CSCI") +
  ggtitle("PCA-plot by Stream Class") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")


fviz_contrib(csci_pr, choice="var", axes=c(1:2), top=30)



# RF ----------------------------------------------------------------------

bmi_csci_flow_rf <- bmi_csci_flow %>% st_drop_geometry() %>% 
  select(csci_percentile, samplemonth, Avg:WSI_Tim) %>% drop_na()

library(randomForest)
library(caret)
library(doParallel)

# make training set 90%, test set 10%
gbmTrain <- bmi_csci_flow_rf[sample(nrow(bmi_csci_flow_rf), round(nrow(bmi_csci_flow_rf)*0.9),  replace = F),]

# tuning grid for hyper parameters of model
grid <- expand.grid(n.trees = c(1000,1500), interaction.depth=c(1:3), shrinkage=c(0.01,0.05,0.1), n.minobsinnode=c(20))

# set up the cv validation
ctrl <- trainControl(method = "repeatedcv",number = 5, repeats = 2, allowParallel = T)

# parallel
registerDoParallel(detectCores()-1)

# build mod
set.seed(124)
unwantedoutput <- capture.output(GBMModel <- train(csci_percentile~.,
                                                   data = gbmTrain,
                                                   method = "gbm", 
                                                   trControl = ctrl, tuneGrid = grid))

print(GBMModel)
summary(GBMModel, las=2)
plot(GBMModel)
plot(GBMModel,i="Peak_Dur_10")
