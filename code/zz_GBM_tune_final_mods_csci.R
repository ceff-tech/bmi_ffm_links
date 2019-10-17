# Tune BRTs (Boosted Regression Trees)
## R. Peek
## BRT Models of the BMI Metrics vs. Flw Metrics
## Use 4 different flow datasets:  Annual, lag1, lag2, and POR
## This is a more robust approach for model tuning

# Libraries ---------------------------------------------------------------

library(purrr) # for working with lists/loops
library(tidyverse) # all the things
library(viridis) # colors
library(sf) # spatial operations
library(mapview) # web maps
library(gbm) # boosted regression trees
library(rsample) # sampling

set.seed(321) # reproducibility

# round functioN:
RoundUp <- function(from,to) ceiling(from/to)*to


# Data --------------------------------------------------------------------

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

# Link Regions ------------------------------------------------------------

# spatial join gage sites with regions, 
## adds ATTRIBUTES, retains ALL pts if left=TRUE,
sel_gages_bmi <- st_join(st_transform(sel_gages_bmi, 3310), left = TRUE, ca_sp_regions["huc_region"])

sel_bmi_gages <- st_join(st_transform(sel_bmi_gages, 3310), left = TRUE, ca_sp_regions["huc_region"])

# add huc regions to the data
bmi_nearest <- st_join(st_transform(bmi_nearest, 3310), left = TRUE, ca_sp_regions["huc_region"])

# a map
# mapview(sel_gages_bmi, col.regions="deepskyblue4", cex=7, alpha=0.7) + 
#   mapview(mainstems, color="darkblue", lwd=0.5) +
#   mapview(ca_sp_regions, zcol="huc_region", alpha.regions=0.3) + 
#   mapview(bmi_coms, col.regions="orange", cex=5, alpha=.7)

# Set up Model Response Vars ----------------------------------------------

bmi.metrics<-c("Shannon_Diversity", "Simpson_Diversity", "Taxonomic_Richness", "EPT_Percent", "Tolerant_Percent", "Intolerant_Percent", "csci", "csci_percentile", "mmi", "mmi_percentile")


# Filter Datasets ---------------------------------------------------------

# if selecting by a specific region use region select
# region_sel <- sel_bmi_gages %>% filter(huc_region=="north_coast" | huc_region=="south_coast")

#region_sel <- sel_bmi_gages %>% filter(huc_region=="north_coast")

# Annual Data -------------------------------------------------------------

## select data and arrange
data_ann <- dplyr::select(bmi_flow_metrics_ann_csci, 1, 106:107, 151:152, 91:93, 96, one_of(bmi.metrics), 115:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

# make sure data is randomized:
random_index <- sample(1:nrow(data_ann), nrow(data_ann))
data_ann <- data_ann[random_index, ]

## Split data and specify train vs. test using rsample
data_ann_split <- initial_split(data_ann, prop = .9)
data_ann_tr <- training(data_ann_split)
data_ann_test  <- testing(data_ann_split)

# Lag 1 Data --------------------------------------------------------------

data_lag1 <- dplyr::select(bmi_flow_metrics_lag1_csci, 1, 106:107, 151:152, 91:93, 96, one_of(bmi.metrics), 115:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

# make sure data is randomized:
random_index <- sample(1:nrow(data_lag1), nrow(data_lag1))
data_lag1 <- data_lag1[random_index, ]

## Split data and specify train vs. test using rsample
data_lag1_split <- initial_split(data_lag1, prop = .9)
data_lag1_tr <- training(data_lag1_split)
data_lag1_test  <- testing(data_lag1_split)

# Lag 2 Data --------------------------------------------------------------

# lag2
data_lag2 <- dplyr::select(bmi_flow_metrics_lag2_csci, 1, 106:107, 151:152, 91:93, 96, one_of(bmi.metrics), 115:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

# make sure data is randomized:
random_index <- sample(1:nrow(data_lag2), nrow(data_lag2))
data_lag2 <- data_lag2[random_index, ]

## Split data and specify train vs. test using rsample
data_lag2_split <- initial_split(data_lag2, prop = .9)
data_lag2_tr <- training(data_lag2_split)
data_lag2_test  <- testing(data_lag2_split)

# Period of Record Data ---------------------------------------------------

data_por <- dplyr::select(bmi_flow_metrics_por_csci, 1, 106:107, 151:152, 91:93, 96, one_of(bmi.metrics), 115:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

# make sure data is randomized:
random_index <- sample(1:nrow(data_por), nrow(data_por))
data_por <- data_por[random_index, ]

## Split data and specify train vs. test using rsample
data_por_split <- initial_split(data_por, prop = .9)
data_por_tr <- training(data_por_split)
data_por_test  <- testing(data_por_split)

# SET UP GRID AND DATA FOR GBM TUNING -------------------------------------

library(rlang)

# SET UP VARS FOR MODEL
hydroDat <- "Annual"
bmiVar <- quote(Shannon_Diversity)


gbm_out <- select(data_ann_tr, !!bmiVar, 18:ncol(data_ann_tr)) %>% 
  filter(!is.na(!!bmiVar)) %>% as.data.frame()

gbm_out_t <- select(data_ann_test, !!bmiVar, 18:ncol(data_ann_test)) %>% 
  filter(!is.na(!!bmiVar)) %>% as.data.frame()

# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.005, .01, .1), # 
  interaction.depth = c(2, 3, 4),
  n.minobsinnode = c(3, 5, 7),
  bag.fraction = c(.65, .75, .85), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)


# see here: http://uc-r.github.io/gbm_regression

# Grid Search GBM ---------------------------------------------------------

# see here: http://www.storybench.org/tidytuesday-bike-rentals-part-2-modeling-with-gradient-boosting-machine/

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = csci_percentile ~ ., # need to manually change!!
    distribution = "gaussian",
    data = gbm_out,
    cv.folds=5, # takes awhile!!
    n.trees = 8000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .8,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$cv.error) # if using CV folds
  #hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$cv.error)) # if using CV folds
}

# look at options:
hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(5)

# best solution based on RMSE
# so on avg our model CSCI_percentile is off by the min RMSE from actual CSCI_percentile
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(min_RMSE) %>% #
    head(n=1))

# purrr GRID GBM ---------------------------------------------------------

# https://bradleyboehmke.github.io/HOML/gbm.html

# create model fit function
gbm_fit <- function(n.trees, shrinkage, interaction.depth, n.minobsinnode, bag.fraction) {
  set.seed(123)
  m <- gbm(
    formula = Shannon_Diversity ~ .,
    data = gbm_out,
    distribution = "gaussian",
    n.trees = n.trees,
    shrinkage = shrinkage,
    interaction.depth = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    cv.folds = 5,
    train.fraction=0.8,
    verbose=FALSE
  )
  # compute RMSE and min trees
  sqrt(min(m$cv.error))
  which.min(m$cv.error) # if using CV folds
}

# simple grid
hyper_grid <- expand.grid(
  n.trees = 8000,
  shrinkage = c(.05, .1), # 
  interaction.depth = c(2, 4),
  n.minobsinnode = c(5, 7),
  bag.fraction = c(.75, .85)
)

# use PURRR
hyper_grid$rmse <- purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit(
    n.trees = ..1,
    shrinkage = ..2,
    interaction.depth = ..3,
    n.minobsinnode = ..4,
    bag.fraction = ..5
  )
)

# look at options:
hyper_grid %>% 
  dplyr::arrange(rmse) %>%
  head(5)

# best solution based on RMSE
# so on avg our model CSCI_percentile is off by the min RMSE from actual CSCI_percentile
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(rmse) %>% #
    head(n=1))

# Grid Search GBM.STEP ---------------------------------------------------------

# simple grid
hyper_grid <- expand.grid(
  shrinkage = c(.05, .1), # 
  interaction.depth = c(2, 4),
  n.minobsinnode = c(5, 7),
  bag.fraction = c(.75, .85), 
  null_dev = 0,               # a place to dump results
  est_remain_dev = 0                     # a place to dump results
)


library(dismo)
source("code/functions/My.gbm.step.R")

gbm_fit_step <- function(shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
    set.seed(123)
  m_step <- My.gbm.step(
    gbm.y = 1, 
    gbm.x = 2:ncol(data), 
    family = "gaussian",
    data = data,
    #max.trees = 8000,
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    train.fraction = .8,
    plot.main = FALSE,
    verbose = FALSE
  )
  
  # compute RMSE and min trees
  #sqrt(min(m_step$cv.error))
  (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) / m_step$self.statistics$mean.null

}

# simple grid
hyper_grid <- expand.grid(
  shrinkage = c(.05, .1),  
  interaction.depth = c(2),
  n.minobsinnode = c(5, 7),
  bag.fraction = c(.75)
)

# use PURRR
hyper_grid$dev_explained <- purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = gbm_out
  )
)

# % percent exlained
#(gbm.fit.final$self.statistics$mean.null - gbm.fit.final$cv.statistics$deviance.mean) / gbm.fit.final$self.statistics$mean.null 

# look at options:
hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>%
  head(5) # best solution based on RMSE

# % explained
#(mean(gbm.tune$valid.error) - mean(gbm.tune$cv.error)) / mean(gbm.tune$valid.error) 

# RMSE sqrt(min(cv.error))
# so on avg our model CSCI_percentile is off by the min RMSE from actual CSCI_percentile
(hyper_best <- hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>% #
  head(n=1))


# R2
# cor(gbm.tune$fit, gbm_out$csci_percentile)^2

# RMSE of prediction: 
# Metrics::rmse(gbm_out$csci_percentile, predict.gbm(object=gbm.tune, newdata=gbm_out_t, RoundUp(hyper_best$optimal_trees, 100)))


# NOW TRAIN BEST GBM MODEL ------------------------------------------------

source("code/functions/My.gbm.step.R")

# pick best model and train:
# for reproducibility
set.seed(123)

# TRAIN BEST GBM MODEL: GBM
gbm.fit.final <- gbm(
  formula = csci_percentile ~ ., ## manually change
  distribution = "gaussian",
  data = gbm_out,
  #cv.folds=5,
  n.trees = RoundUp(hyper_best$optimal_trees, 100), # set to larger number?
  interaction.depth = hyper_best$interaction.depth,
  shrinkage = hyper_best$shrinkage,
  n.minobsinnode = hyper_best$n.minobsinnode,
  bag.fraction = hyper_best$bag.fraction,
  train.fraction = 1, # use all the training data
  n.cores = NULL, # will use all cores by default
  verbose = F
)

# NOW TRAIN BEST GBM.STEP MODEL -------------------------------------------

# TRAIN BEST GBM MODEL: GBM STEP
gbm.fit.final <- My.gbm.step(
  gbm.y = 1, gbm.x = 2:ncol(gbm_out), 
  family = "gaussian",
  data = gbm_out,
  #learning.rate = hyper_best$shrinkage,# use default of 0.01
  tree.complexity = hyper_best$interaction.depth,
  n.minobsinnode = hyper_best$n.minobsinnode,
  bag.fraction = hyper_best$bag.fraction, 
  #n.trees = RoundUp(hyper_best$optimal_trees, 50), # default
  n.folds = 5,
  verbose=TRUE,
  plot.main = FALSE,
  n.cores = NULL # will use all cores by default
)

## % variation explained (only calculable from gbm.step)
## D2 = (mean total deviance - cross validated residual deviance)/total deviance 
# see (Leathwick et al., 2006)
(gbm.fit.final$self.statistics$mean.null - gbm.fit.final$cv.statistics$deviance.mean) / gbm.fit.final$self.statistics$mean.null 

# RMSE sqrt(min(cv.error))

# PLOT MSE RELATIVE INFLUENCE ----------------------------------------------

## RI: improvement by MSE (split criterion), vars with largest avg decrease in MSE are important

gbm_fin_RI<-as.data.frame(summary(gbm.fit.final, plotit = F, method=relative.influence)) %>% 
  mutate("Ymetric"= as_name(bmiVar),
         "flowdat" = hydroDat,
         "method" = "mse")

rownames(gbm_fin_RI) <- NULL

# get top vars >=5 RI
gbm_fin_topn <- sum((summary(gbm.fit.final, plotit=FALSE)$rel.inf)>=5)

# get var names
(gbm_fin_topvar <- as.character(summary(gbm.fit.final, plotit=FALSE)$var[1:gbm_fin_topn]))

# make df:
gbm_fin_ri_top <- tibble(RI=summary(gbm.fit.final, plotit=FALSE)$rel.inf[1:gbm_fin_topn], varnames=gbm_fin_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(fin_ri_ann <- ggplot() + 
    geom_col(data=gbm_fin_ri_top, aes(x=varnames, y=RI), 
             fill="skyblue") + coord_flip() + 
    labs(title=paste0(hydroDat, ": Top ", gbm_fin_topn," vars: ", as_label(bmiVar)), 
         y="Relative Influence (%)", x="",
         subtitle = "MSE Criterion") +
    ylim(c(0,30)) +
    ggdark::dark_theme_classic(base_family = "Roboto Condensed")+
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2)) 

#figs/Annual_brt_csci_percentile_top_RI_rmse.png
ggsave(filename=paste0("figs/", hydroDat,"_brt_", as_name(bmiVar), "_top_RI_mse", ".png"), width = 8, height = 7, units = "in", dpi = 300)

# PLOT PERMUTATION TEST RI ------------------------------------------------

## PT: permutation test, decrease in accuracy is averaged and vars with largest avg decrease in accuracy are import

gbm_fin_PT<-as.data.frame(summary(gbm.fit.final, plotit = F, method=permutation.test.gbm)) %>% 
  mutate("Ymetric"= as_name(bmiVar),
         "flowdat" = hydroDat,
         "method" = "permtest")
rownames(gbm_fin_PT) <- NULL

# get top vars
gbm_fin_topn_pt <- sum((summary(gbm.fit.final, method=permutation.test.gbm, plotit=FALSE)$rel.inf)>=5)
(gbm_fin_topvar_pt <- as.character(summary(gbm.fit.final, 
                                           method=permutation.test.gbm, plotit=FALSE)$var[1:gbm_fin_topn_pt]))

# make df:
gbm_fin_pt_top <- tibble(PT=summary(gbm.fit.final, method=permutation.test.gbm, plotit=FALSE)$rel.inf[1:gbm_fin_topn_pt], 
                         varnames=gbm_fin_topvar_pt) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), PT))

# barplot by most ACCURATE
(fin_pt_ann <- ggplot() + 
    geom_col(data=gbm_fin_pt_top, aes(x=varnames, y=PT), 
             fill="skyblue") + coord_flip() + 
    labs(title=paste0(hydroDat, ": Top ", gbm_fin_topn," vars: ", as_label(bmiVar)), 
         y="Relative Influence (%) (perm test)", x="",
         subtitle = "Permutation Test"
         ) +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    ggdark::dark_theme_classic(base_family = "Roboto Condensed"))

# save
ggsave(filename=paste0("figs/", hydroDat,"_brt_", as_name(bmiVar), "_top_RI_pt", ".png"), width = 8, height = 7, units = "in", dpi = 300)

# Partial Dependence Plots ------------------------------------------------

library(pdp)

## displays avg change in predicted csci as we vary VARIABLE while holding everything else constant

## THESE ONLY WORK WITH STANDARD GBM model, not GBMSTEP

# get top var
(bestHydro_pt <- gbm_fin_pt_top %>% top_n(n = 3, PT))
(bestHydro_ri <- gbm_fin_ri_top %>% top_n(n = 3, RI))

# set top var Number
varNo <- 1

## PT Top
gbm.fit.final %>%
  partial(pred.var = bestHydro_pt$varnames[varNo], 
          n.trees = gbm.fit.final$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = gbm_out) +
  ggdark::dark_theme_classic(base_family = "Roboto Condensed") + 
  labs(subtitle = paste0("Partial Dependence Plot: ",bestHydro_pt$varnames[varNo]), 
       x=paste0(bestHydro_pt$varnames[varNo]),
       y=paste0("Predicted ", as_name(bmiVar)))

# Save 
ggsave(filename=paste0("figs/pdp_pt_",as_name(bmiVar),  "_top_var_", as.character(bestHydro_pt$varnames[varNo]), ".png"), width = 11, height = 7, units = "in", dpi=300)

## RI Top
gbm.fit.final %>%
  partial(pred.var = bestHydro_ri$varnames[varNo], 
          n.trees = gbm.fit.final$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = gbm_out) +
  ggdark::dark_theme_classic(base_family = "Roboto Condensed") + 
  labs(subtitle = paste0("Partial Dependence Plot: ",bestHydro_ri$varnames[varNo]), 
       x=paste0(bestHydro_ri$varnames[varNo]),
       y=paste0("Predicted ", as_name(bmiVar)))

# Save 
ggsave(filename=paste0("figs/pdp_ri_",as_name(bmiVar),  "_top_var_", as.character(bestHydro_ri$varnames[varNo]), ".png"), width = 11, height = 7, units = "in", dpi=300)


# Save Out RIs ------------------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(paste0(as_name(bmiVar),"_ri_",hydroDat,"_gbm")), value=bind_rows(gbm_fin_PT, gbm_fin_RI))
ls(pattern = as_name(bmiVar))
save(list = ls(pattern = as_name(bmiVar)), file = paste0("data_output/09_final_gbm_",as_name(bmiVar),"_ri_", tolower(hydroDat), ".rda"))

# save out hyper grid results:
assign(x = tolower(paste0(as_name(bmiVar),"_",hydroDat,"_gbm_hypergrid")), value=hyper_grid)
ls(pattern = "gbm_hypergrid")
save(list = ls(pattern = "gbm_hypergrid"), file = paste0("data_output/09_final_gbm_",as_name(bmiVar),"_hypergrid_", tolower(hydroDat), ".rda"))

# ICE PLOTS ---------------------------------------------------------------

# ICE (Individual conditonal expectation) plots: rather than plot the average marginal effect on the response variable, we plot the change in the predicted response variable for each observation as we vary each predictor variable.

## The equivalent to a PDP for individual data instances is called individual conditional expectation (ICE) plot (Goldstein et al. 2017). An ICE plot visualizes the dependence of the prediction on a feature for each instance separately, resulting in one line per instance, compared to one line overall in partial dependence plots.

# When the curves have a wide range of intercepts and are consequently “stacked” on each other, heterogeneity in the response variable values due to marginal changes in the predictor variable of interest can be difficult to discern, thus centering can help

# PT
# ice1_pt <- gbm.fit.final %>%
#   partial(
#     pred.var = as.character(bestHydro_pt$varnames[varNo]), 
#     n.trees = gbm.fit.final$n.trees, 
#     grid.resolution = 100,
#     ice = TRUE) %>%
#   autoplot(rug = TRUE, train = gbm_out, alpha = .1) +
#   labs(subtitle = paste0("ICE (PT): ", bestHydro_pt$varnames[varNo]),
#        y=paste0("Predicted ", as_name(bmiVar))) +
#   ggdark::dark_theme_classic(base_family = "Roboto Condensed")
# ice1_pt

ice2_pt <- gbm.fit.final %>%
  partial(
    pred.var = as.character(bestHydro_pt$varnames[varNo]), 
    n.trees = gbm.fit.final$n.trees, 
    grid.resolution = 100,
    ice = TRUE) %>%
  autoplot(rug = TRUE, train = gbm_out, alpha = .1, center=TRUE) +
  labs(subtitle = paste0("ICE Centered (PT): ", bestHydro_pt$varnames[varNo]),
       y=paste0("Predicted ", as_name(bmiVar))) +
  ggdark::dark_theme_classic(base_family = "Roboto Condensed")
ice2_pt

# RI
# ice1_ri <- gbm.fit.final %>%
#   partial(
#     pred.var = as.character(bestHydro_ri$varnames[varNo]), 
#     n.trees = gbm.fit.final$n.trees, 
#     grid.resolution = 100,
#     ice = TRUE) %>%
#   autoplot(rug = TRUE, train = gbm_out, alpha = .1) +
#   labs(subtitle = paste0("ICE (RI): ", bestHydro_ri$varnames[varNo]),
#        y=paste0("Predicted ", as_name(bmiVar))) +
#   ggdark::dark_theme_classic(base_family = "Roboto Condensed")
# ice1_ri

ice2_ri <- gbm.fit.final %>%
  partial(
    pred.var = as.character(bestHydro_ri$varnames[varNo]), 
    n.trees = gbm.fit.final$n.trees, 
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = gbm_out, alpha = .1, center = TRUE) +
  labs(subtitle = paste0("ICE Centered (RI): ", bestHydro_ri$varnames[varNo])) +
  ggdark::dark_theme_classic(base_family = "Roboto Condensed")
ice2_ri

# plot the non-centered
# gridExtra::grid.arrange(ice1_pt, ice1_ri, nrow = 1)
# plot centered
# gridExtra::grid.arrange(ice2_pt, ice2_ri, nrow = 1)
cowplot::plot_grid(ice2_pt, ice2_ri, nrow = 1)

# permutation test:
ggsave(filename=paste0("figs/pdp_ice_",as_name(bmiVar),  
                       "_top_var_", 
                       as.character(bestHydro_pt$varnames[varNo]),"_",
                       as.character(bestHydro_ri$varnames[varNo]),
                       ".png"), width = 11, height = 7, units = "in", dpi=300)


# Predict -----------------------------------------------------------------

# predict values for test data
pred <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, gbm_out_t)

# results
caret::RMSE(pred, gbm_out$csci_percentile)

# Single GBM --------------------------------------------------------------

# see here:
# http://uc-r.github.io/gbm_regression#gbm

# select cols of interest
data_csci <- data_ann_tr[,c(5,18:ncol(data_ann_tr))] %>% 
  filter(!is.na(csci_percentile)) %>% as.data.frame()

# train GBM model
gbm.fit_1 <- gbm(
  formula = csci_percentile ~ .,
  distribution = "gaussian",
  data = data_csci,
  n.trees = 5000, # trees
  interaction.depth = 3, # tree complexity
  shrinkage = 0.005, # learning rate
  cv.folds = 5, # cv folds
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# find index for n trees with minimum CV error
(min_MSE <- which.min(gbm.fit_1$cv.error))

# get MSE
gbm.fit_1$cv.error[min_MSE]

# compute RMSE
sqrt(gbm.fit_1$cv.error[min_MSE])

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit_1, method = "cv")



