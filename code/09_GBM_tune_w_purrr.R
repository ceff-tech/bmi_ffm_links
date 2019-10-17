# Running GBM (Boosted Regression Trees) with PURRR
# Selecting best models
# R. Peek 2019
# Generates models to select "final" model based on tuning criteria
# then makes final model, saved in data_output/gbms

# Libraries ---------------------------------------------------------------

library(purrr) # for working with lists/loops
library(tidyverse) # all the things
library(viridis) # colors
library(sf) # spatial operations
library(mapview) # web maps
library(gbm) # boosted regression trees
library(rsample) # sampling
library(rlang)
library(dismo)
source("code/functions/My.gbm.step.R")

# round functioN:
#RoundUp <- function(from,to) ceiling(from/to)*to
set.seed(321) # reproducibility

# Load Data ---------------------------------------------------------------

load("data_output/06_selected_bmi_flow_metrics_w_csci_ANN.rda")
load("data_output/06_selected_bmi_flow_metrics_w_csci_POR.rda")
load("data_output/06_selected_bmi_flow_metrics_w_csci_LAG1.rda")
load("data_output/06_selected_bmi_flow_metrics_w_csci_LAG2.rda")
load("data_output/05_selected_bmi_stations_w_comids.rda")
load("data_output/05_mainstems_us_ds_selected_gages.rda")
load("data_output/03_selected_bmi_and_gages.rda")
load("data_output/07_selected_bmi_nearest_usgs_stations.rda")
# read in fish regions:
load("data/07_umbrella_sp_regions.rda")

# Link Fish BioRegions w Data --------------------------------------------

# spatial join gage sites with regions, adds ATTRIBUTES, retains ALL pts if left=TRUE,
sel_gages_bmi <- st_join(st_transform(sel_gages_bmi, 3310), left = TRUE, ca_sp_regions["huc_region"])
sel_bmi_gages <- st_join(st_transform(sel_bmi_gages, 3310), left = TRUE, ca_sp_regions["huc_region"])
# add huc regions to the data
bmi_nearest <- st_join(st_transform(bmi_nearest, 3310), left = TRUE, ca_sp_regions["huc_region"])

# Filter: Select Regions --------------------------------------------------

# if selecting by a specific region use region select
# region_sel <- sel_bmi_gages %>% filter(huc_region=="north_coast" | huc_region=="south_coast")

#region_sel <- sel_bmi_gages %>% filter(huc_region=="north_coast")

# Select BMI Response Variable for GBM ------------------------------

# get metrics
bmi.metrics<-c("Shannon_Diversity", "Simpson_Diversity", "Taxonomic_Richness", "EPT_Percent", "Tolerant_Percent", "Intolerant_Percent", "csci", "csci_percentile", "mmi", "mmi_percentile")

# actually only want Shannon's, CSCI_percentile, Intolerant, and MMI

# PICK RESPONSE VAR FOR MODEL
hydroDat <- "POR" # can be Annual, Lag1, Lag2, POR
bmiVar <- quote(Intolerant_Percent) # select response var from list above

# ANNUAL DATA -------------------------------------------------------------

data_ann <- dplyr::select(bmi_flow_metrics_ann_csci, 1, 106:107, 151:152, 91:93, 96, one_of(bmi.metrics), 115:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

# make sure data is randomized:
random_index <- sample(1:nrow(data_ann), nrow(data_ann))
data_ann <- data_ann[random_index, ]

## Split data and specify train vs. test using rsample
data_ann_split <- initial_split(data_ann, prop = .9)
data_ann_tr <- training(data_ann_split) %>% 
  dplyr::select(!!bmiVar, 18:ncol(.)) %>% 
  filter(!is.na(!!bmiVar)) %>% as.data.frame()
data_ann_te  <- testing(data_ann_split) %>% 
  dplyr::select(!!bmiVar, 18:ncol(.)) %>% 
  filter(!is.na(!!bmiVar)) %>% as.data.frame()

# LAG1 DATA ---------------------------------------------------------------

data_lag1 <- dplyr::select(bmi_flow_metrics_lag1_csci, 1, 106:107, 151:152, 91:93, 96, one_of(bmi.metrics), 115:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

# make sure data is randomized:
random_index <- sample(1:nrow(data_lag1), nrow(data_lag1))
data_lag1 <- data_lag1[random_index, ]

## Split data and specify train vs. test using rsample
data_lag1_split <- initial_split(data_lag1, prop = .9)
data_lag1_tr <- training(data_lag1_split) %>% 
  dplyr::select(!!bmiVar, 18:ncol(.)) %>% 
  filter(!is.na(!!bmiVar)) %>% as.data.frame()
data_lag1_te  <- testing(data_lag1_split) %>% 
  dplyr::select(!!bmiVar, 18:ncol(.)) %>% 
  filter(!is.na(!!bmiVar)) %>% as.data.frame()

# LAG2 DATA ---------------------------------------------------------------

data_lag2 <- dplyr::select(bmi_flow_metrics_lag2_csci, 1, 106:107, 151:152, 91:93, 96, one_of(bmi.metrics), 115:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

# make sure data is randomized:
random_index <- sample(1:nrow(data_lag2), nrow(data_lag2))
data_lag2 <- data_lag2[random_index, ]

## Split data and specify train vs. test using rsample
data_lag2_split <- initial_split(data_lag2, prop = .9)
data_lag2_tr <- training(data_lag2_split) %>% 
  dplyr::select(!!bmiVar, 18:ncol(.)) %>% 
  filter(!is.na(!!bmiVar)) %>% as.data.frame()
data_lag2_te <- testing(data_lag2_split) %>% 
  dplyr::select(!!bmiVar, 18:ncol(.)) %>% 
  filter(!is.na(!!bmiVar)) %>% as.data.frame()


# POR DATA ----------------------------------------------------------------

data_por <- dplyr::select(bmi_flow_metrics_por_csci, 1, 106:107, 151:152, 91:93, 96, one_of(bmi.metrics), 115:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

# make sure data is randomized:
random_index <- sample(1:nrow(data_por), nrow(data_por))
data_por <- data_por[random_index, ]

## Split data and specify train vs. test using rsample
data_por_split <- initial_split(data_por, prop = .9)
data_por_tr <- training(data_por_split) %>% 
  dplyr::select(!!bmiVar, 18:ncol(.)) %>% 
  filter(!is.na(!!bmiVar)) %>% as.data.frame()
data_por_te <- testing(data_por_split) %>% 
  dplyr::select(!!bmiVar, 18:ncol(.)) %>% 
  filter(!is.na(!!bmiVar)) %>% as.data.frame()


# GBM.STEP TUNING GRID  -------------------------------------------------

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(.001, .005), # c(0.001, .005, .01)  
  interaction.depth = c(2, 3, 4),
  n.minobsinnode = c(5,10),
  bag.fraction = c(0.75, 0.8)
)

## SET THIS ONCE to input correct HYDRODAT
gbm_hydrodat_tr <- data_por_tr

# GBM.STEP FUNCTION -----------------------------------------------------

# requires dismo and function loaded

gbm_fit_step <- function(
  shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_step <- My.gbm.step(
    gbm.y = 1, # response in training data
    gbm.x = 2:ncol(data), # hydro dat
    family = "gaussian",
    data = data,
    #max.trees = 8000, # can specify but don't for now
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = FALSE,
    verbose = FALSE
  )
  
  # Compute the Deviance Explained: (total dev - cv dev) / total dev
  (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) /
    m_step$self.statistics$mean.null
  # print which iteration completed
  #print("Model Iteration Completed!")
}

# TUNE GBM.STEP WITH PURRR ---------------------------------------------

# use PURRR
hyper_grid$dev_explained <- purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = gbm_hydrodat_tr # CHECK AND CHANGE!!
  )
)

# % percent exlained
#(gbm.fit.final$self.statistics$mean.null - gbm.fit.final$cv.statistics$deviance.mean) / gbm.fit.final$self.statistics$mean.null 

# look at results:
hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>%
  head(5) # best solution

(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))

write_csv(hyper_grid, path = paste0("data_output/gbms/10_gbm_hypergrid_",tolower(as_name(bmiVar)),"_", tolower(hydroDat),".csv"))

# FINAL BRT.STEP --------------------------------------------------------

# # make single hyper_best based on txt outputs
# hyper_best <- expand.grid(
#   shrinkage = c(.007),
#   interaction.depth = c(3),
#   n.minobsinnode = c(10),
#   bag.fraction = c(0.8)
# )

# based on above, run final BRT and save:
gbm_final_step <- function(
  shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_final <- My.gbm.step(
    gbm.y = 1, # response in training data
    gbm.x = 2:ncol(data), # hydro dat
    family = "gaussian",
    data = data,
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = TRUE,
    verbose = TRUE
  )
}

# run with PURR
capture.output(gbm_fin_out <- purrr::pmap(
    hyper_best,
    ~ gbm_final_step(
      shrinkage = ..1,
      interaction.depth = ..2,
      n.minobsinnode = ..3,
      bag.fraction = ..4,
      data = gbm_hydrodat_tr # CHECK AND CHANGE!!
    )
  ), file=paste0("data_output/gbms/10_gbm_final_",tolower(as_name(bmiVar)),"_", tolower(hydroDat),"_output.txt"), append=T)

#strip off a list layer
(gbm_fin_out <- gbm_fin_out[[1]])

# add hyperbest to capture output file:
cat("\nBest parameters for GBM.STEP:\n\n", 
    file = paste0("data_output/gbms/10_gbm_final_",tolower(as_name(bmiVar)),"_", tolower(hydroDat),"_output.txt"), append=TRUE)

# for testing:
#cat(format_tsv(hyper_best))
write_tsv(hyper_best, path = paste0("data_output/gbms/10_gbm_final_",
                                    tolower(as_name(bmiVar)),"_", 
                                    tolower(hydroDat),"_output.txt"),
          col_names = TRUE, append=TRUE)

# Save Final GBM ----------------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(paste0("gbm_final_", as_name(bmiVar),"_",hydroDat)), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = paste0("gbm_final_", tolower(as_name(bmiVar)))))

# save to RDS
write_rds(x = get(fileToSave), path = paste0("data_output/gbms/10_",fileToSave, ".rds"), compress = "gz")

# SAVE ALL HYDRO DATASETS for this var:
save(list = ls(pattern="data_"), file = tolower(paste0("data_output/gbms/10_gbm_final_", as_name(bmiVar), "_hydrodata.rda")))
