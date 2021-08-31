# Running GBM (Boosted Regression Trees) with PURRR
# R. Peek 2021
# Generates models to select "final" model based on tuning criteria
# then makes final model, saved in models/

# this is building off of flow_seasonality repo 
# see data here: https://github.com/ryanpeek/flow_seasonality/tree/main/output

# Libraries ---------------------------------------------------------------

library(purrr) # for working with lists/loops
library(glue)
options(tidyverse.quiet = TRUE)
library(tidyverse) # load quietly
library(viridis) # colors
library(sf) # spatial operations
library(mapview) # web maps
# set background basemaps/default options:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron")

mapviewOptions(homebutton = FALSE, 
               basemaps=basemapsList, 
               viewer.suppress = FALSE,
               fgb = FALSE)

library(gbm) # boosted regression trees
library(rsample) # sampling
library(rlang)
library(dismo)
library(tidylog)
source("code/functions/My.gbm.step.R")

set.seed(321) # reproducibility

# 01. Load Data ---------------------------------------------------------------

# load updated data:
#bio_ffm<- read_rds("data_output/06_csci_por_trim_final_dataset.rds")
bio_ffm<- read_rds("https://github.com/ryanpeek/flow_seasonality/blob/main/output/10_ffc_filtered_final_combined.rds?raw=true")

## ADD DELTA HYDRO AND PLOT ------------------------------------------------

# need to add a "delta hydro: delta_p50 = (p50_obs-p50_pred)/p50_pred)"
bio_ffm <- bio_ffm %>% 
  # add delta hydro
  mutate(delta_p50 = (p50_obs-p50_pred) / p50_pred) %>% 
  # fix zeros and NaNs
  mutate(delta_p50 = case_when(
    is.infinite(delta_p50) ~ 0, # replace "Not a Number" w zero
    is.nan(delta_p50) ~ 0, # replace "Not a Number" w zero
    delta_p50 == NaN ~ NA_real_,
    TRUE ~ delta_p50
  )) %>% 
  relocate(delta_p50, .after = "p50_pred") %>% 
  mutate(delta_p50_scale = 
           as.vector(scale(delta_p50, scale=TRUE)), 
         .after="delta_p50") %>% 
  filter(!is.na(delta_p50))

# summarize
summary(bio_ffm$delta_p50, useNA="ifany")
summary(bio_ffm$delta_p50_scale, useNA="ifany")
bio_ffm %>% select(contains("p50")) %>% summary


# # plot
# bio_ffm %>% 
#   filter(bioindicator=="ASCI") %>% 
#   ggplot() +
#   geom_point(aes(x=biovalue, y=delta_p50, color=gagetype, 
#                  shape=gagetype), alpha=0.7)+
#   geom_smooth(method = "gam",
#               aes(x=biovalue, y=delta_p50, 
#                   fill=gagetype, color=gagetype), 
#               lwd=.5, show.legend = FALSE) +
#   theme_classic() +
#   #scale_y_log10() +
#   labs(x="ASCI", y="Delta Hydrology (p50)") +
#   scale_shape("Gage Type", solid = FALSE) +
#   ggthemes::scale_fill_colorblind("Gage Type") +
#   ggthemes::scale_color_colorblind("Gage Type") +
#   facet_wrap(.~metric, scales = "free_y")
# 
# # still some very extreme values...filter to everything below 95% quantile
# (delta_p50_95 <- quantile(bio_ffm$delta_p50, na.rm=TRUE, 0.95))
# 
# bio_ffm %>% 
#   filter(delta_p50 < delta_p50_95) %>% 
#   ggplot() +
#   geom_point(aes(x=asci, y=delta_p50, fill=gagetype, 
#                  color=gagetype, shape=gagetype), alpha=0.7)+
#   geom_smooth(method = "gam",
#               aes(x=asci, y=delta_p50, color=gagetype, 
#                   fill=gagetype), 
#               lwd=.5, show.legend = FALSE) +
#   theme_classic() +
#   #scale_y_log10() +
#   labs(x="ASCI", y="Delta Hydrology (p50)") +
#   scale_shape("Gage Type", solid = TRUE) +
#   ggthemes::scale_fill_colorblind("Gage Type") +
#   ggthemes::scale_color_colorblind("Gage Type") +
#   facet_wrap(.~metric, scales = "free_y")

# 02. Select BMI Response Variable for GBM ------------------------------

# get metrics
modname <- "all_ca_seasonality"
bioindi <- "ASCI"
bioVar <- quote(asci) # select response var from list above

# 03. Setup POR Data for Model ----------------------------------------------------------------
  
# need to select and spread data: 
data_por <- bio_ffm %>% 
  filter(bioindicator==bioindi) %>% 
  filter(gagetype=="ALT") %>% 
  dplyr::select(StationCode, SampleID, sampledate,
                HUC_12, gageid, comid, 
                COMID_bio, gagetype,
                bioindicator, biovalue,
                metric, delta_p50,
                MP_metric,
                Power.avg) %>% 
  # need to spread the metrics wide
  pivot_wider(names_from = metric, values_from = delta_p50) %>% 
  mutate(gagetype = as.factor(gagetype)) %>% 
  as.data.frame()


# check how many rows/cols: KEEP ALL FOR NOW
dim(data_por) # CSCI: 420 / 29 ASCI: 255 / 29
(data_names <- names(data_por)) # save colnames out

# remove cols that have more than 70% NA
data_por <- data_por[, which(colMeans(!is.na(data_por)) > 0.7)]
dim(data_por)
 
# find the cols that have been dropped
setdiff(data_names, names(data_por))
# remove rows that have more than 70% NA
data_por <- data_por[which(rowMeans(!is.na(data_por))>0.7),]
dim(data_por) # same...no rows dropped

# 04. RANDOMIZE AND TIDY Data ----------------------------------------------------

# make sure data is randomized:
random_index <- sample(1:nrow(data_por), nrow(data_por))
data_por <- data_por[random_index, ]

## USE ALL DATA
data_por_train <- data_por %>% # use all data
  dplyr::select(biovalue, 10:ncol(.)) %>% as.data.frame() 
# summary(data_por_train)
         
# double check cols are what we want
names(data_por_train) # should be 19 (ASCI + MP_metric, Power.avg and 16 metrics)
str(data_por_train)

# 05. GBM.STEP MODEL  ------------------------------------------------------------

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(0.001, 0.003, 0.005), 
  interaction.depth = c(5), 
  n.minobsinnode = c(3, 5, 10), 
  bag.fraction = c(0.75, 0.8) 
)

# double check and view
hyper_grid

# load the GBM.step function (requires dismo and function loaded)
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
  if(!is.null(m_step)){ # this helps if there's an error above
    (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) /
      m_step$self.statistics$mean.null
  } else { 
    return(NA)
  }
}

# 06. RUN GBM.STEP WITH PURRR ---------------------------------------------

# use PURRR: this part can take awhile...get some coffee
hyper_grid$dev_explained <-purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data_por_train) # CHECK AND CHANGE!!
)

# 07. VIEW AND SAVE MODEL RESULTS -----------------------------------------

# look at results:
hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>%
  head(5) # top 5 models

# pick the best solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))

# write these all out to a file for reference later
(gbm_file <- glue("models/07_gbm_final_{tolower(as_name(bioVar))}_{tolower(modname)}_hypergrid"))

# check for file and delete?
if(fs::file_exists(path = paste0(gbm_file,".csv"))){
  fs::file_delete(path = paste0(gbm_file, ".csv"))
  print("File deleted, time for a fresh start!")
} else {
  print("No file saved yet")
}

# save out
write_csv(hyper_grid, file = glue("{gbm_file}.csv"), append = TRUE)

# read in
hyper_grid <- readr::read_csv(file = paste0(gbm_file,".csv"), col_names = c("shrinkage", "interaction.depth", "n.minobsinnode", "bag.fraction", "dev_explained"))

# get best model solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))

# 08. RUN BEST BRT MODEL ---------------------------------------------------------------

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

# set up filename for best model outputs
(gbm_best_file <- glue("models/07_gbm_final_{tolower(as_name(bioVar))}_{tolower(modname)}_model_output.txt"))

# check for file and delete?
if(fs::file_exists(path = gbm_best_file)){
  fs::file_delete(path = gbm_best_file)
  print("File deleted, time for a fresh start!")
} else {
  print("No file saved yet")
}

# run best option with PURR
capture.output(gbm_fin_out <- purrr::pmap(
    hyper_best,
    ~ gbm_final_step(
      shrinkage = ..1,
      interaction.depth = ..2,
      n.minobsinnode = ..3,
      bag.fraction = ..4,
      data = data_por_train # CHECK AND CHANGE!!
    )
  ), file=gbm_best_file, append=T)

#strip off a list layer to view data
(gbm_fin_out <- gbm_fin_out[[1]])

# add hyperbest to capture output file:
cat("\nBest parameters for GBM.STEP:\n\n", 
    file = gbm_best_file, append=TRUE)

# add the parameters used to run the model
write_tsv(hyper_best, file = gbm_best_file,
          col_names = TRUE, append=TRUE)

# % percent explained
(gbm_fin_out$self.statistics$mean.null - gbm_fin_out$cv.statistics$deviance.mean) / gbm_fin_out$self.statistics$mean.null 

# 09. SAVE FINAL GBM AND DATA ---------------------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(glue("gbm_final_{as_name(bioVar)}_{modname}")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = glue("gbm_final_{tolower(as_name(bioVar))}_{modname}")))

# save to RDS
write_rds(x = get(fileToSave), file = glue("models/07_{fileToSave}_model.rds"), compress = "gz")

# Save all the datasets used in the model:
save(list = ls(pattern="data_"), file = tolower(glue("models/07_{fileToSave}_model_data.rda")))

# 10. STRMCLASS: Response Variable for GBM ------------------------------

# get metrics
modname <- "mixed_seasonality"   # or "all_ca_ffc_only"
strmclass <- "MIXED" # MIXED, SNOWMELT, RAIN

# snowmelt= not enough samples (ASCI)

# 11. STRMCLASS: Setup POR Data for Model ----------------------------------------------------------------

# need to select and spread data: 
data_strmclass <- bio_ffm %>% 
  filter(bioindicator==bioindi, 
         class3_name == strmclass,
         gagetype == "ALT") %>% 
  dplyr::select(StationCode, SampleID, sampledate,
                HUC_12, gageid, comid, 
                COMID_bio, gagetype,
                bioindicator, biovalue,
                metric, delta_p50,
                MP_metric,
                Power.avg) %>% 
  # need to spread the metrics wide
  pivot_wider(names_from = metric, values_from = delta_p50) %>% 
  mutate(gagetype = as.factor(gagetype)) %>% 
  as.data.frame()

bio_ffm %>% filter(bioindicator=="ASCI", gagetype=="ALT") %>%
  distinct(gageid, SampleID, .keep_all=TRUE) %>%
  select(class3_name) %>% table()

# check how many rows/cols: KEEP ALL FOR NOW
dim(data_strmclass) # should match above
(data_names <- names(data_strmclass)) # save colnames out

# remove cols that have more than 70% NA
data_strmclass <- data_strmclass[, which(colMeans(!is.na(data_strmclass)) > 0.7)]
dim(data_strmclass)

# find the cols that have been dropped
setdiff(data_names, names(data_strmclass))
# remove rows that have more than 70% NA
data_strmclass <- data_strmclass[which(rowMeans(!is.na(data_strmclass))>0.7),]
dim(data_strmclass) # same...no rows dropped

# 12. RANDOMIZE AND TIDY Data ----------------------------------------------------

# make sure data is randomized:
random_index <- sample(1:nrow(data_strmclass), nrow(data_strmclass))
data_strmclass <- data_strmclass[random_index, ]

## USE ALL DATA
data_strmclass_train <- data_strmclass %>% # use all data
  dplyr::select(biovalue, 10:ncol(.)) %>% as.data.frame() 
names(data_strmclass_train)

# 13. GBM.STEP MODEL  ------------------------------------------------------------

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(0.001, 0.003, 0.005), 
  interaction.depth = c(5), 
  n.minobsinnode = c(3, 5, 10), 
  bag.fraction = c(0.75, 0.8) 
)

# double check and view
hyper_grid

# load the GBM.step function (requires dismo and function loaded)
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
  if(!is.null(m_step)){ # this helps if there's an error above
    (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) /
      m_step$self.statistics$mean.null
  } else { 
    return(NA)
  }
}

# 14. RUN GBM.STEP WITH PURRR ---------------------------------------------

# use PURRR: this part can take awhile...get some coffee
hyper_grid$dev_explained <-purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data_strmclass_train) # CHECK AND CHANGE!!
)

# 15. VIEW AND SAVE MODEL RESULTS -----------------------------------------

# look at results:
hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>%
  head(5) # top 5 models

# pick the best solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))

# write these all out to a file for reference later
(gbm_file <- glue("models/07_gbm_final_{tolower(as_name(bioVar))}_{tolower(modname)}_hypergrid"))

# check for file and delete?
if(fs::file_exists(path = paste0(gbm_file,".csv"))){
  fs::file_delete(path = paste0(gbm_file, ".csv"))
  print("File deleted, time for a fresh start!")
} else {
  print("No file saved yet")
}

# save out
write_csv(hyper_grid, file = glue("{gbm_file}.csv"), append = TRUE)

# read in
hyper_grid <- readr::read_csv(file = paste0(gbm_file,".csv"), col_names = c("shrinkage", "interaction.depth", "n.minobsinnode", "bag.fraction", "dev_explained"))

# get best model solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))

# 16. RUN BEST BRT MODEL ---------------------------------------------------------------

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

# set up filename for best model outputs
(gbm_best_file <- glue("models/07_gbm_final_{tolower(as_name(bioVar))}_{tolower(modname)}_model_output.txt"))

# check for file and delete?
if(fs::file_exists(path = gbm_best_file)){
  fs::file_delete(path = gbm_best_file)
  print("File deleted, time for a fresh start!")
} else {
  print("No file saved yet")
}

# run best option with PURR
capture.output(gbm_fin_out <- purrr::pmap(
  hyper_best,
  ~ gbm_final_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data_strmclass_train # CHECK AND CHANGE!!
  )
), file=gbm_best_file, append=T)

#strip off a list layer to view data
(gbm_fin_out <- gbm_fin_out[[1]])

# add hyperbest to capture output file:
cat("\nBest parameters for GBM.STEP:\n\n", 
    file = gbm_best_file, append=TRUE)

# add the parameters used to run the model
write_tsv(hyper_best, file = gbm_best_file,
          col_names = TRUE, append=TRUE)

# % percent explained
(gbm_fin_out$self.statistics$mean.null - gbm_fin_out$cv.statistics$deviance.mean) / gbm_fin_out$self.statistics$mean.null 

# 17. SAVE FINAL GBM AND DATA ---------------------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(glue("gbm_final_{as_name(bioVar)}_{modname}")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = glue("gbm_final_{tolower(as_name(bioVar))}_{modname}")))

# save to RDS
write_rds(x = get(fileToSave), file = glue("models/07_{fileToSave}_model.rds"), compress = "gz")

# Save all the datasets used in the model:
save(list = ls(pattern="data_strmclass"), file = glue("models/07_{fileToSave}_model_data.rda"))

