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

# Load Data ------------------------------------------------

# load updated data:
bio_ffm<- read_rds("https://github.com/ryanpeek/flow_seasonality/blob/main/output/10_ffc_filtered_final_combined_rev.rds?raw=true")

## Add Delta Hydrology for FFM ---------------------------------------

# need to add a "delta hydro: delta_p50 = (p50_obs-p50_pred)/p50_pred)"
bio_ffm <- bio_ffm %>% ungroup() %>% 
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
bio_ffm %>% select(contains("csci"), contains("bio")) %>% summary

## Check for Duplicates ------------------------------------

# check for duplicates
# bio_ffm %>% 
#   filter(bioindicator=="CSCI") %>% 
#   distinct(StationCode, gageid, biovalue, .keep_all=TRUE) %>% 
#   group_by(StationCode, gageid) %>% 
#   tally() %>% 
#   arrange(desc(n)) %>% #View()
#   #filter(n>0) %>% nrow() # CSCI n= 364 sites total
#   filter(n>1) %>%  nrow() # # CSCI n= 147 sites w dups


## Box Plots ------------------------------------------------------------

# need to make this by flow component colors 
bio_ffm %>%
  #filter(bioindicator=="ASCI") %>% 
  # some extreme values so filter to everything below 98 quantile
  filter(delta_p50 < quantile(bio_ffm$delta_p50, na.rm=TRUE, 0.98)) %>% 
  ggplot() +
  geom_jitter(aes(x=metric, y=delta_p50), col="gray",
              alpha=0.4, show.legend = FALSE) +
  geom_hline(yintercept = 0, color="maroon", alpha=0.9, lwd=0.7, lty=1) +
  geom_boxplot(aes(x=metric, y=delta_p50, fill=metric), 
               alpha=0.8, show.legend = FALSE, outlier.shape = NA) +
  cowplot::theme_cowplot(font_family = "Roboto Condensed") +
  labs(x="FF Metric", y="Delta Hydrology ([p50 obs - p50 pred]/p50 pred)") +
  scale_fill_viridis_d("FFM") + 
  coord_flip() +
  #labs(subtitle = "ASCI") +
  theme(plot.background = element_rect(fill="white")) +
  facet_wrap(.~bioindicator)

# ggsave(filename = "figs/boxplot_of_asci_csci_ffm_by_deltaH.png", 
#        width = 11, height = 8, 
#        dpi=300, units="in")

# look at trends by stream class
# bio_ffm %>%
#   filter(bioindicator=="ASCI") %>% 
#   filter(delta_p50 < quantile(delta_p50, na.rm=TRUE, 0.98)) %>%
#   ggplot() +
#   geom_point(aes(y=biovalue, x=delta_p50, color=class3_name), 
#              pch=16, alpha=0.2, show.legend = FALSE)+
#   geom_smooth(method = "gam",
#               aes(y=biovalue, x=delta_p50, color=class3_name,
#                   fill=class3_name),
#               lwd=.8, show.legend = TRUE, alpha=0.3) +
#   theme_classic() +
#   #scale_y_log10() +
#   labs(y="ASCI", x="Delta Hydrology (p50)") +
#   ggthemes::scale_fill_colorblind("Stream Class") +
#   ggthemes::scale_color_colorblind("Stream Class") +
#   facet_wrap(.~metric, scales = "free_x")
# 
# ggsave(filename = "figs/trendplot_of_asci_ffm_deltaH_by_streamclass3_gam.png", 
#        width = 11, height = 8, 
#        dpi=300, units="in")


# ALL CA: GBM ------------------------------

# get metrics
modname <- "all_ca_seasonality"
bioindi <- "CSCI"
bioVar <- quote(csci) # select response var from list above

## A. ALL CA: Setup POR Data for Model ----------------------------------------------
  
# need to select and spread data: 
data_por <- bio_ffm %>% 
  filter(bioindicator==bioindi) %>% 
  filter(delta_p50 < quantile(delta_p50, na.rm=TRUE, 0.96)) %>% 
  #filter(gagetype=="ALT") %>% all gages
  dplyr::select(StationCode, SampleID, sampledate,
                HUC_12, gageid, comid, 
                COMID_bio, gagetype,
                bioindicator, biovalue,
                metric, delta_p50,
                MP_metric) %>% # drop wavelet
  # need to spread the metrics wide
  pivot_wider(names_from = metric, values_from = delta_p50) %>% 
  mutate(gagetype = as.factor(gagetype)) %>% 
  as.data.frame()

# check how many rows/cols: KEEP ALL FOR NOW
dim(data_por) # CSCI: 613 / 27
(data_names <- names(data_por)) # save colnames out

# remove cols that have more than 70% NA
data_por <- data_por[, which(colMeans(!is.na(data_por)) > 0.7)]
dim(data_por)
 
# find the cols that have been dropped
setdiff(data_names, names(data_por))
# remove rows that have more than 70% NA
data_por <- data_por[which(rowMeans(!is.na(data_por))>0.7),]
dim(data_por) # 4 sites dropped

## B. ALL CA: Randomize and Tidy Data ----------------------------------------------------

# make sure data is randomized:
random_index <- sample(1:nrow(data_por), nrow(data_por))
data_por <- data_por[random_index, ]

## USE ALL DATA
data_por_train <- data_por %>% # use all data
  dplyr::select(biovalue, 10:ncol(.)) %>% as.data.frame() 

# double check cols are what we want
names(data_por_train) # should be 18 (bio + MP_metric & 16 FFM metrics)
str(data_por_train)

summary(data_por_train)

## C. ALL CA: Setup gbm.step model  ---------------------------------------

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

## D. ALL CA: Iterate gbm.step --------------------------------------

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

## E. ALL CA: View and save model results -------------------------------

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

## F. ALL CA: Run best BRT ----------------------------------------

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

## G. ALL CA: Save final GBM and Data ------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(glue("gbm_final_{as_name(bioVar)}_{modname}")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = glue("gbm_final_{tolower(as_name(bioVar))}_{modname}")))

# save to RDS
write_rds(x = get(fileToSave), file = glue("models/07_{fileToSave}_model.rds"), compress = "gz")

ls(pattern="data_")

# Save all the datasets used in the model:
save(list = ls(pattern="data_"), file = tolower(glue("models/07_{fileToSave}_model_data.rda")))

# STRMCLASS: MIXED GBM ------------------------------

# get metrics
modname <- "mixed_seasonality"   # or "all_ca_ffc_only"
strmclass <- "MIXED" # MIXED, RAIN, SNOWMELT

## A. MIXED: Setup POR Data for Model ----------------------------------------------------------------

# need to select and spread data: 
data_strmclass <- bio_ffm %>% 
  filter(bioindicator==bioindi, 
         class3_name == strmclass) %>% 
  dplyr::select(StationCode, SampleID, sampledate,
                HUC_12, gageid, comid, 
                COMID_bio, gagetype,
                bioindicator, biovalue,
                metric, delta_p50,
                MP_metric) %>% 
  # need to spread the metrics wide
  pivot_wider(names_from = metric, values_from = delta_p50) %>% 
  mutate(gagetype = as.factor(gagetype)) %>% 
  as.data.frame()

bio_ffm %>% filter(bioindicator=="CSCI") %>%
  distinct(gageid, SampleID, .keep_all=TRUE) %>%
  select(class3_name) %>% table()

# CSCI:
# MIXED   RAIN      SNOWMELT 
# 125      397       91 

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

## B. MIXED: Randomize and tidy ----------------------------------------------------

# make sure data is randomized:
random_index <- sample(1:nrow(data_strmclass), nrow(data_strmclass))
data_strmclass <- data_strmclass[random_index, ]

## USE ALL DATA
data_strmclass_train <- data_strmclass %>% # use all data
  dplyr::select(biovalue, 10:ncol(.)) %>% as.data.frame() 
names(data_strmclass_train)

summary(data_strmclass_train)

## C. MIXED: Setup gbm.step model  ------------------------------------------------------------

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

## D. MIXED: Iterate gbm.step ---------------------------------------------

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

## E. MIXED: View and save model results -----------------------------------------

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

## F. MIXED: Run best BRT ---------------------------------------------------------------

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

# summary(gbm_fin_out)

## G. MIXED: Save Final GBM and Data ---------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(glue("gbm_final_{as_name(bioVar)}_{modname}")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = glue("gbm_final_{tolower(as_name(bioVar))}_{modname}")))

# save to RDS
write_rds(x = get(fileToSave), file = glue("models/07_{fileToSave}_model.rds"), compress = "gz")

ls(pattern="data_strmclass")

# Save all the datasets used in the model:
save(list = ls(pattern="data_strmclass"), file = glue("models/07_{fileToSave}_model_data.rda"))

# STRMCLASS: RAIN GBM ------------------------------

# get metrics
modname <- "rain_seasonality"   # or "all_ca_ffc_only"
strmclass <- "RAIN" # MIXED, RAIN, SNOWMELT

## A. RAIN: Setup POR Data for Model ----------------------------------------------------------------

# need to select and spread data: 
data_strmclass <- bio_ffm %>% 
  filter(bioindicator==bioindi, 
         class3_name == strmclass) %>% 
  dplyr::select(StationCode, SampleID, sampledate,
                HUC_12, gageid, comid, 
                COMID_bio, gagetype,
                bioindicator, biovalue,
                metric, delta_p50,
                MP_metric) %>% 
  # need to spread the metrics wide
  pivot_wider(names_from = metric, values_from = delta_p50) %>% 
  mutate(gagetype = as.factor(gagetype)) %>% 
  as.data.frame()

bio_ffm %>% filter(bioindicator=="CSCI") %>%
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

## B. RAIN: Randomize and tidy ----------------------------------------------------

# make sure data is randomized:
random_index <- sample(1:nrow(data_strmclass), nrow(data_strmclass))
data_strmclass <- data_strmclass[random_index, ]

## USE ALL DATA
data_strmclass_train <- data_strmclass %>% # use all data
  dplyr::select(biovalue, 10:ncol(.)) %>% as.data.frame() 
names(data_strmclass_train)

## C. RAIN: Setup gbm.step model  ------------------------------------------------------------

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

## D. RAIN: Iterate gbm.step ---------------------------------------------

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

## E. RAIN: View and save model results -----------------------------------------

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

## F. RAIN: Run best BRT ---------------------------------------------------------------

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

#summary(gbm_fin_out)

## G. RAIN: Save Final GBM and Data ---------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(glue("gbm_final_{as_name(bioVar)}_{modname}")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = glue("gbm_final_{tolower(as_name(bioVar))}_{modname}")))

# save to RDS
write_rds(x = get(fileToSave), file = glue("models/07_{fileToSave}_model.rds"), compress = "gz")

ls(pattern="data_strmclass")

# Save all the datasets used in the model:
save(list = ls(pattern="data_strmclass"), file = glue("models/07_{fileToSave}_model_data.rda"))

# STRMCLASS: SNOWMELT GBM ---------------------------

# get metrics
modname <- "snow_seasonality"   # or "all_ca_ffc_only"
strmclass <- "SNOWMELT" # MIXED, RAIN, SNOWMELT

## A. SNOW: Setup POR Data for Model ----------------------------------------------------------------

# some serious outliers in these data, need to center and scale
bio_ffm %>% 
  filter(bioindicator==bioindi, 
         class3_name == strmclass) %>%
  # if using 98th drop 32 obs, if using 96, drop 60 obs
  filter(delta_p50 < quantile(delta_p50, na.rm=TRUE, 0.96)) %>% 
  dplyr::select(StationCode, SampleID, sampledate,
                HUC_12, gageid, comid, 
                COMID_bio, gagetype,
                bioindicator, biovalue,
                metric, delta_p50,
                MP_metric) %>% 
  ggplot() +
  geom_jitter(aes(x=metric, y=delta_p50), col="gray",
              alpha=0.4, show.legend = FALSE) +
  geom_hline(yintercept = 0, color="maroon", alpha=0.9, lwd=0.7, lty=1) +
  geom_boxplot(aes(x=metric, y=delta_p50, fill=metric), 
               alpha=0.8, show.legend = FALSE, outlier.shape = NA) +
  cowplot::theme_cowplot(font_family = "Roboto Condensed") +
  labs(x="FF Metric", y="Delta Hydrology ([p50 obs - p50 pred]/p50 pred)") +
  scale_fill_viridis_d("FFM") + 
  coord_flip() +
  labs(subtitle = "CSCI: Snowmelt stream class sites (outliers >95 percentile excluded)") +
  #labs(subtitle = "CSCI: Snowmelt stream class sites (all data)") +
  theme(plot.background = element_rect(fill="white"))

# save
# ggsave(filename = "figs/boxplot_SNOW_csci_ffm_by_deltaH_95.png", 
#        width = 11, height = 8, 
#        dpi=300, units="in")
# ggsave(filename = "figs/boxplot_SNOW_csci_ffm_by_deltaH_all.png", 
#        width = 11, height = 8, 
#        dpi=300, units="in")

# need to select and spread data: 
data_strmclass <- bio_ffm %>% 
  filter(bioindicator==bioindi, 
         class3_name == strmclass) %>% 
  filter(delta_p50 < quantile(delta_p50, na.rm=TRUE, 0.96)) %>% 
  dplyr::select(StationCode, SampleID, sampledate,
                HUC_12, gageid, comid, 
                COMID_bio, gagetype,
                bioindicator, biovalue,
                metric, delta_p50,
                MP_metric) %>% 
  # need to spread the metrics wide
  pivot_wider(names_from = metric, values_from = delta_p50) %>% 
  mutate(gagetype = as.factor(gagetype)) %>% 
  as.data.frame()

bio_ffm %>% filter(bioindicator=="CSCI") %>%
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
dim(data_strmclass) # 4 sites dropped

## B. SNOW: Randomize and tidy ----------------------------------------------------

# make sure data is randomized:
random_index <- sample(1:nrow(data_strmclass), nrow(data_strmclass))
data_strmclass <- data_strmclass[random_index, ]

## USE ALL DATA
data_strmclass_train <- data_strmclass %>% # use all data
  dplyr::select(biovalue, 10:ncol(.)) %>% as.data.frame() 
names(data_strmclass_train)

summary(data_strmclass_train)

# try scaling?
data_strmclass_train_s <- scale(data_strmclass_train) %>% as.data.frame()

summary(data_strmclass_train_s)

## C. SNOW: Setup gbm.step model  ------------------------------------------------------------

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(0.001),
  interaction.depth = c(5),
  n.minobsinnode = c(3),
  bag.fraction = c(0.75)
)

# double check and view
hyper_grid

# load the GBM.step function (requires dismo and function loaded)
source("code/functions/My.gbm.step.snow.R")
gbm_fit_step <- function(
  shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_step <- My.gbm.step.snow(
    gbm.y = 1, # response in training data
    gbm.x = 2:ncol(data), # hydro dat
    family = "gaussian",
    data = data,
    #max.trees = 3000, # can specify but don't for now
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

## D. SNOW: Iterate gbm.step ---------------------------------------------

# use PURRR: this part can take awhile...get some coffee
hyper_grid$dev_explained <-purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step_snow(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data_strmclass_train) # CHECK AND CHANGE!!
)

## E. SNOW: View and save model results -----------------------------------------

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

## F. SNOW: Run best BRT ---------------------------------------------------------------

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

## G. SNOW: Save Final GBM and Data ---------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(glue("gbm_final_{as_name(bioVar)}_{modname}")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = glue("gbm_final_{tolower(as_name(bioVar))}_{modname}")))

# save to RDS
write_rds(x = get(fileToSave), file = glue("models/07_{fileToSave}_model.rds"), compress = "gz")

# Save all the datasets used in the model:
save(list = ls(pattern="data_strmclass"), file = glue("models/07_{fileToSave}_model_data.rda"))

