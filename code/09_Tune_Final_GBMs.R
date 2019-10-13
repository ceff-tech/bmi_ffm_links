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
#region_sel <- bmi_nearest %>% filter(huc_region=="north_coast" | huc_region=="south_coast")

# Annual Data -------------------------------------------------------------

## select data and arrange
data_ann <- dplyr::select(bmi_flow_metrics_ann_csci, 1, 106:107, 151:152, 91:93, 96, one_of(bmi.metrics), 115:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

# make sure data is randomized:
random_index <- sample(1:nrow(data_ann), nrow(data_ann))
data_ann <- data_ann[random_index, ]

## Split data and specify train vs. test using rsample
data_ann_split <- initial_split(data_ann, prop = .7)
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
data_lag1_split <- initial_split(data_lag1, prop = .7)
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
data_lag2_split <- initial_split(data_lag2, prop = .7)
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
data_por_split <- initial_split(data_por, prop = .7)
data_por_tr <- training(data_por_split)
data_por_test  <- testing(data_por_split)


# Single GBM --------------------------------------------------------------

# see here:
# http://uc-r.github.io/gbm_regression#gbm

# select cols of interest
data_csci <- data_ann_tr[,c(5,18:ncol(data_ann_tr))] %>% 
  filter(!is.na(csci_percentile)) %>% as.data.frame()

# train GBM model
gbm.fit2 <- gbm(
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
(min_MSE <- which.min(gbm.fit2$cv.error))

# get MSE
gbm.fit2$cv.error[min_MSE]

# compute RMSE
sqrt(gbm.fit2$cv.error[min_MSE])

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit2, method = "cv")




# SET UP GRID AND DATA FOR GBM TUNING -------------------------------------

library(rlang)

# SET UP VARS FOR MODEL
hydroDat <- "Annual"
#bmiVar <- "csci_percentile"
bmiVar <- quote(csci_percentile)

gbm_ann <- select(data_ann_tr, !!bmiVar, 18:ncol(data_ann_tr)) %>% 
  filter(!is.na(!!bmiVar)) %>% as.data.frame()

gbm_ann_t <- select(data_ann_test, !!bmiVar, 18:ncol(data_ann_test)) %>% 
  filter(!is.na(!!bmiVar)) %>% as.data.frame()

# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.005, .01, .1),
  interaction.depth = c(2, 3, 4),
  n.minobsinnode = c(3, 5, 7),
  bag.fraction = c(.65, .75, .85), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)


# Grid Search GBM ---------------------------------------------------------

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = csci_percentile ~ ., # need to manually change!!
    distribution = "gaussian",
    data = gbm_ann,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

# look at options:
hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(5)

hyper_best <- hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(n=1)


# NOW TRAIN BEST GBM MODEL ------------------------------------------------

# pick best model and train:
# for reproducibility
set.seed(123)

# TRAIN BEST GBM MODEL
gbm.fit.final <- gbm(
  formula = csci_percentile ~ ., ## manually change
  distribution = "gaussian",
  data = gbm_ann,
  n.trees = hyper_best$optimal_trees,
  interaction.depth = hyper_best$interaction.depth,
  shrinkage = hyper_best$shrinkage,
  n.minobsinnode = hyper_best$n.minobsinnode,
  bag.fraction = hyper_best$bag.fraction, 
  train.fraction = 1, # use all the training data
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  



# PLOT RELATIVE INFLUENCE -------------------------------------------------

## RI: improvement by MSE (split criterion), vars with largest avg decrease in MSE are important

gbm_fin_RI<-as.data.frame(summary(gbm.fit.final, plotit = F, method=relative.influence)) %>% 
  mutate("Ymetric"= as_name(bmiVar),
         "flowdat" = hydroDat)

rownames(gbm_fin_RI) <- NULL

# get top vars
gbm_fin_topn <- sum((summary(gbm.fit.final, plotit=FALSE)$rel.inf)>=5)
(gbm_fin_topvar <- as.character(summary(gbm.fit.final, plotit=FALSE)$var[1:gbm_fin_topn]))

# make df:
gbm_fin_ri_top <- tibble(RI=summary(gbm.fit.final, plotit=FALSE)$rel.inf[1:gbm_fin_topn], varnames=gbm_fin_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(fin_ri_ann <- ggplot() + 
    geom_col(data=gbm_fin_ri_top, aes(x=varnames, y=RI), 
             fill="skyblue") + coord_flip() + 
    labs(title=paste0(hydroDat, ": Top ", gbm_fin_topn," vars: ", as_label(bmiVar)), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    ggdark::dark_theme_classic(base_family = "Roboto Condensed")+
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2)) 



# PLOT PERMUTATION TEST RI ------------------------------------------------

## PT: permutation test, decrease in accuracy is averaged and vars with largest avg decrease in accuracy are import

gbm_fin_PT<-as.data.frame(summary(gbm.fit.final, plotit = F, method=permutation.test.gbm)) %>% 
  mutate("Ymetric"= as_name(bmiVar),
         "flowdat" = hydroDat)
rownames(gbm_fin_PT) <- NULL

# get top vars
gbm_fin_topn_pt <- sum((summary(gbm.fit.final, method=permutation.test.gbm, plotit=FALSE)$rel.inf)>=5)
(gbm_fin_topvar_pt <- as.character(summary(gbm.fit.final, 
                                           method=permutation.test.gbm, plotit=FALSE)$var[1:gbm_fin_topn_pt]))

# make df:
gbm_fin_pt_top <- tibble(PT=summary(gbm.fit.final, method=permutation.test.gbm, plotit=FALSE)$rel.inf[1:gbm_fin_topn_pt], varnames=gbm_fin_topvar_pt) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), PT))

# barplot by most ACCURATE
(fin_pt_ann <- ggplot() + 
    geom_col(data=gbm_fin_pt_top, aes(x=varnames, y=PT), 
             fill="skyblue") + coord_flip() + 
    labs(title=paste0(hydroDat, ": Top ", gbm_fin_topn," vars: ", as_label(bmiVar)), y="Relative Influence (%) (perm test)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    ggdark::dark_theme_classic(base_family = "Roboto Condensed"))


#figs/ann_brt_csci_top_RI_barplot.png)
ggsave(filename=paste0("figs/", hydroDat,"_brt_", as_name(bmiVar), "_top_RI", ".png"), width = 8, height = 7, units = "in", dpi = 300)


# Partial Dependence Plots ------------------------------------------------

# for partial depend plots
library(pdp)

# get top var
bestHydro <- gbm_fin_pt_top %>% top_n(n = 1, PT)

# displays avg change in predicted csci as we vary Wet_Tim while holding everything else constant
gbm.fit.final %>%
  partial(pred.var = bestHydro$varnames, 
          n.trees = gbm.fit.final$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = gbm_ann) +
  ggdark::dark_theme_classic(base_family = "Roboto Condensed") + 
  labs(subtitle = paste0("Partial Dependence Plot: ",bestHydro$varnames), y=paste0("Predicted ", as_name(bmiVar)))

ggsave(filename=paste0("figs/pdp_pt_",as_name(bmiVar),  "_top_var_", as.character(bestHydro$varnames), ".png"), width = 11, height = 7, units = "in", dpi=300)

# ICE PLOTS ---------------------------------------------------------------

# ICE plots: rather than plot the average marginal effect on the response variable, we plot the change in the predicted response variable for each observation as we vary each predictor variable
# When the curves have a wide range of intercepts and are consequently “stacked” on each other, heterogeneity in the response variable values due to marginal changes in the predictor variable of interest can be difficult to discern. 

ice1 <- gbm.fit.final %>%
  partial(
    pred.var = as.character(bestHydro$varnames), 
    n.trees = gbm.fit.final$n.trees, 
    grid.resolution = 100,
    ice = TRUE) %>%
  autoplot(rug = TRUE, train = gbm_ann, alpha = .1) +
  labs(subtitle = paste0("ICE Non-centered: ", bestHydro$varnames),
       y=paste0("Predicted ", as_name(bmiVar))) +
  ggdark::dark_theme_classic(base_family = "Roboto Condensed")
ice1

ice2 <- gbm.fit.final %>%
  partial(
    pred.var = as.character(bestHydro$varnames), 
    n.trees = gbm.fit.final$n.trees, 
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = gbm_ann, alpha = .1, center = TRUE) +
  labs(subtitle = paste0("ICE Centered: ", bestHydro$varnames)) +
  ggdark::dark_theme_classic(base_family = "Roboto Condensed")

#gridExtra::grid.arrange(ice1, ice2, nrow = 1)
cowplot::plot_grid(ice1, ice2, nrow = 1)

# permutation test:
ggsave(filename=paste0("figs/pdp_ice_pt_",as_name(bmiVar),  "_top_var_", as.character(bestHydro$varnames), ".png"), width = 11, height = 7, units = "in", dpi=300)

# Predict -----------------------------------------------------------------

# predict values for test data
pred <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, gbm_ann_t)

# results
caret::RMSE(pred, gbm_ann$csci_percentile)
