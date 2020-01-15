
# Get Altered Gage FFM ----------------------------------------------------


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(tidyverse)
library(tidylog)

#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library(ffcAPIClient)
#options(scipen = 999) # turn of scientific notation
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", "")) 

# Get Gages ---------------------------------------------------------------

usgs_list <- read_csv("data/FinalAlteredList.csv")

# clean up: drop the "T" from the ID, drop cols
usgs_list <- usgs_list %>% 
  mutate(gage_id=gsub("^T",replacement = "", ID)) %>% 
  select(gage_id, ID:LONGITUDE, FLOW_YRS_POST_1950:ROADS_KM_SQ_KM) 

# look at reference?
table(usgs_list$FINAL_REFERENCE) # why are 30=Y here? were they included in reference data, but pre-regulation?


# Run FFC: Get FFC Results as DF ------------------------------------------

# use flow calculator to pull FFM for each gage, this works as raw option
usgs_ffc_dat <- usgs_list %>% 
  slice(1:6) %>%
  mutate(ffc_data = map(gage_id, ~get_ffc_results_for_usgs_gage(.x))) %>% 
  mutate(ffc_df = map(ffc_data, ~ffcAPIClient::get_results_as_df(.x))) %>% 
  select(-ffc_data) %>% 
  mutate(ffc_percentiles = map(ffc_df, ~get_percentiles(.x))) %>%
  # add column designating these percentiles as ffc_obs (observed)
  mutate(ffc_percentiles = map_depth(ffc_percentiles, 1, ~mutate(.x, data_source="ffc_obs"))) %>% 
  mutate(ffc_pred = map(NHDV2_COMID, ~get_predicted_flow_metrics(.x))) %>% 
  # add column designating these percentiles as ffc_pred (predicted)
  mutate(ffc_pred = map_depth(ffc_pred, 1, ~mutate(.x, data_source="ffc_pred")))
  # convert to dataframe (not listcol)
  # unnest(cols=c(ffc_percentiles))

# gages with no data or no ffc:
# 10264675


# using get preds
usgs_ffc_pred <- usgs_list %>% 
  slice(1:6) %>%
  mutate(ffc_pred = map(NHDV2_COMID, ~get_predicted_flow_metrics(.x))) %>% 
  mutate(ffc_df = map(ffc_pred, ~ffcAPIClient::get_results_as_df(.x))) %>% 
  select(-ffc_data) %>% 
  # convert to dataframe (not listcol)
  unnest(cols=c(ffc_df))

# Run FFC: Evaluate Gage Alteration ---------------------------------------

# try using FFC with evaluate_gage_alteration
tst_eval <- ffcAPIClient::evaluate_gage_alteration(gage_id = "10259540", token = Sys.getenv("EFLOWS_TOKEN", ""))
tst_pred <- ffcAPIClient::get_predicted_flow_metrics(com_id = 22592737)
tst_percentiles <- ffcAPIClient::get_percentiles(usgs_ffc_dat[[28]][[1]])


# follow up with ones that broke?
# tst <- ffcAPIClient::evaluate_gage_alteration(gage_id = 10257549, token = Sys.getenv("EFLOWS_TOKEN", ""))

