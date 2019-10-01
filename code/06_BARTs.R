# BARTS (Bayesian Additive Regression Trees)


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(CSCI)
library(BMIMetrics)
library(lubridate)
#library(BART)
#library(bartMachine)
#library(parallel)
#detectCores()

# Data --------------------------------------------------------------------

load("data_output/selected_bmi_flow_metrics_w_csci_ANN.rda")
load("data_output/selected_bmi_flow_metrics_w_csci_POR.rda")
load("data_output/selected_bmi_stations_w_comids.rda")
load("data_output/maintems_us_ds_selected_gages.rda")
#load("data_output/selected_bmi_stations_w_csci_flow_por.rda")
#load("data_output/selected_bmi_stations_w_csci_flow_annual.rda")
#load("data_output/selected_bmi_stations_w_data.rda")
#load("data_output/selected_usgs_flow_metrics_POR.rda")
#load("data_output/selected_flow_by_years_of_bmi.rda")
#load("selected_bmi_metrics_at_gage_sites.rda")
# make non-sf: 
#bmi_csci_flow_yrs <- bmi_csci_flow_yrs %>% st_drop_geometry() %>% as.data.frame

# Calc Bug Metrics --------------------------------------------------------

# # first filter to years of interest
# 
# bmi_filt <- bmi_dat %>% 
#   filter(YYYY %in% c(1993:2016))
# 
# library(purrr)
# 
# bugs_split <- bmi_filt %>% 
#   split(.$SampleID) %>% 
#   map(~BMI(.x)) # make into BMI object
# 
# bugs_samp <- bugs_split %>% 
#   map(~sample(.x)) # subsample to 500 individuals and aggregate
# 
# bugs_agg <- bugs_samp %>% 
#   map(~aggregate(.x))
# 
# # Calculate metrics at SAFIT Level 1
# bug_metrics <- bugs_agg %>% 
#   map(~BMIall(.x, effort=1))
# 
# # make clean station set:
# bmi_sampleids <- bmi_filt %>% st_drop_geometry() %>% distinct(SampleID, .keep_all = T) %>% 
#   select(1:4, 8:24)
# 
# # flatten and rejoin with station data:
# bmi_metrics_df <- bug_metrics %>%
#   do.call(what = rbind) %>% 
#   remove_rownames() %>% 
#   inner_join(., bmi_sampleids, by="SampleID")
# 
# # clean workspace, rm old bits
# rm(bugs_agg, bugs_samp, bugs_split, bug_metrics, bmi_filt)
# 
# # SAVE IT
# save(bmi_metrics_df, file="selected_bmi_metrics_at_gage_sites.rda")


# Join Bug Metrics with Ann Flow Data -------------------------------------

# get flow POR and add to annual
flow_por_wide <- flow_por_wide %>% mutate(year=as.factor("POR"))

# join but make year a "factor"
flow_by_years_bmi_wide <- flow_by_years_bmi_wide %>% 
  mutate(year=as.factor(year)) %>% 
  bind_rows(., flow_por_wide) %>% 
  mutate(year=as.factor(year))

# join with flow data by gage ID, then filter to years BMI data collected
bmi_flow_metrics_all <- left_join(bmi_metrics_df, flow_by_years_bmi_wide, by="ID") %>% 
  # fix years so Period of record shows up as 1900 and can be filtered out for annual vs. non
  mutate(years = as.integer(ifelse(year=="POR", "1900", as.character(year))))

# make an annual dataset where flow data matched to bug data: 
bmi_flow_metrics_annual <- bmi_flow_metrics_all %>% 
  dplyr::filter(YYYY == years | YYYY == years-1 | YYYY==years-2)

# make POR dataset
bmi_flow_metrics_por <- bmi_flow_metrics_all %>% 
  dplyr::filter(years==1900)

# Now add CSCI: but need to regen sampleID for CSCI data (station_YMD_samplemethod_replicate)
bmi_csci_flow <- bmi_csci_flow %>% 
  mutate(SampleID=paste0(StationCode, "_", year(sampledate), sprintf("%02d", month(sampledate)), sprintf("%02d", day(sampledate)), "_", collectionmethodcode, "_", fieldreplicate)) %>% st_drop_geometry()

# make a distinct SampleID list of csci
csci_only <- bmi_csci_flow %>% select(SampleID, csci, csci_percentile) %>% 
  distinct(SampleID, .keep_all = T)

# now join
bmi_flow_metrics_por_csci <- left_join(bmi_flow_metrics_por, csci_only)
bmi_flow_metrics_ann_csci <- left_join(bmi_flow_metrics_annual, csci_only)

save(bmi_flow_metrics_por_csci, file="data_output/selected_bmi_flow_metrics_w_csci_POR.rda")
save(bmi_flow_metrics_ann_csci, file="data_output/selected_bmi_flow_metrics_w_csci_ANN.rda")


# Set up Model Vars -------------------------------------------------------


## BMI Metrics (Definitely)  
bmi.metrics<-c("Shannon", "totRich", "totDens_sqm") 
## BMI Metrics (Potentially)
bmi.metrics.extra<-c("EPTRich", "Sens_N", "Toler_N", "ChironDens_sqm")

## select data and arrange
data<-select(dat, one_of(bmi.metrics), 3, 47:67) # not including River here

## Scale and center data (subtract mean and divide by SD)
df.scaled<-sapply(data, function(x) scale(x, center=TRUE, scale = TRUE))
df.scaled<-as.data.frame(df.scaled) # make as.dataframe to remove attributes from scale()

# primary bmi response variables
y1 <- df.scaled$Shannon # shannon diversity
y2 <- df.scaled$totRich # richness
y3 <- df.scaled$totDens_sqm # totDens_sqm

# secondary bmi response vars
y4 <- df.scaled$EPTRich # EPT Richness
y5 <- df.scaled$Sens_N # Sensitive Proportion
y6 <- df.scaled$Toler_N # Tolerant Proportion
y7 <- df.scaled$ChironDens_sqm # Biotic Index

# covariates
names(df.scaled)
X <- df.scaled[,c(4:25)]

# the default BART
b1 <- bartMachine(X, y1)
b1 # rmse in sample 0.6, PseuodR2=.607

# estimate out of sample error with K-Fold Cross Validation
k_fold_cv(X, y1, k_folds = 10) # cv w 10 folds, out of sample rmse=0.58, and PseuodR2=0.29

# Assess out-of-sample RMSE of a BART model for varying numbers of trees in the sum-of-trees model
# rmse_by_num_trees(b1, tree_list = c(5, seq(10, 100, 10), 150, 200), holdout_pctg = 0.1, num_replicates = 20)

# build better model
#b1_cv <- bartMachineCV(X, y1, num_tree_cvs = c(25, 50, 200))

## SCALED Winner: bartMachine CV win: k: 2 nu, q: 3, 0.9 m: 25
k_fold_cv(X, y1, k_folds = 10, k = 2, nu=3, q=0.9, num_trees= 25, verbose=T)

# check normality and heteroskedasticity
# png(filename = "figs/normality_shannon.png",width = 6, height = 4, units = "in",res = 100)
# check_bart_error_assumptions(b1_cv)
# dev.off()

# check convergence of Gibbs 
# plot_convergence_diagnostics(b1_cv)

# Plots
print(plot_y_vs_yhat(b1, credible_intervals = TRUE))

print(plot_y_vs_yhat(b1, prediction_intervals = TRUE))

# Variable Importance by Trees
print(investigate_var_importance(b1, type = "trees", num_replicates_for_avg = 20)) # plot vars
# Var importance by Splits
print(investigate_var_importance(b1, type = "splits", num_replicates_for_avg = 20))

# looks at covariance
# cov_importance_test(b1_cv, covariates = c("YrRecess_AvgDailyRate"))
# cov_importance_test(b1_cv, covariates = c("YrRecess_LengthDay"))
# cov_importance_test(b1_cv, covariates = c("temp.7.avg"))
cov_importance_test(b1, covariates = c("YrRecess_AvgDailyRate","temp.7.avg", "YrRecess_LengthDay","AvgRecessHist_DailyRate","MaxMinRatio_YrRecess", "YrRecess_DevStartWyday"))

# cov_importance_test(b1) # test all vars together

print(pd_plot(b1, j = "temp.7.avg"))
print(pd_plot(b1, j = "YrRecess_AvgDailyRate"))
print(pd_plot(b1, j = "YrRecess_LengthDay"))

# Select Variables
# vs <- var_selection_by_permute(b1,bottom_margin = 6, num_permute_samples = 10)
# print(vs)

## look at important variables using different methods
# vs$important_vars_local_names
# vs$important_vars_global_max_names
# vs$important_vars_global_se_names

# var_selection_by_permute_cv(b1)
