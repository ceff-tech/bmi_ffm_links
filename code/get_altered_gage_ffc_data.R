
# Get Altered Gage FFM ----------------------------------------------------


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tictoc)
#library(tidylog)
#library(sf)
#library(mapview)
#library(lubridate)

#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library(ffcAPIClient)
#options(scipen = 999) # turn of scientific notation
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", "")) 

# Get Gages ---------------------------------------------------------------

usgs_list <- read_csv("data/FinalAlteredList.csv")

# clean up: drop the "T" from the ID, drop cols
usgs_list <- usgs_list %>% 
  mutate(gage_id=as.integer(gsub("^T",replacement = "", ID))) %>% 
  select(gage_id, ID:LONGITUDE, FLOW_YRS_POST_1950:REF_END_YEAR) 

# look at reference?
table(usgs_list$FINAL_REFERENCE) # 30=Y here, included in reference data but pre-regulation?

# summary(usgs_list)

# Setup Error Functions for ffcAPI ----------------------------------------

get_ffc <- possibly(get_ffc_results_for_usgs_gage, otherwise = NA_character_)
# tst <- get_ffc(10253080) # should give NA
get_ffc_df <- possibly(get_results_as_df, otherwise = NA_character_)
get_ffc_percentiles <- possibly(get_percentiles, otherwise = NA_character_)

# Run FFC: Get FFC Results as DF ------------------------------------------

# use flow calculator to pull FFM for each gage, this works as raw option
tic()
# this unnests by percentiles for each metric for each gage
usgs_ffc_dat <- usgs_list %>% 
  #slice(1:100) %>% # pick a few rows to test
  mutate(ffc_df = furrr::future_imap(gage_id, ~get_ffc(.x))) %>%
  mutate(ffc_df = furrr::future_imap(ffc_df, ~get_ffc_df(.x))) %>% 
  mutate(ffc_percentiles = furrr::future_imap(ffc_df, ~get_ffc_percentiles(.x))) %>%
  unnest(cols = c(ffc_percentiles)) %>% # pull out percentiles
  select(-ffc_percentiles)

toc()

# for first 100:
# 157.437 sec elapsed with future_imap

# NOTES:
## if unnesting by ffc_df, will get ffc metrics as cols for each year of gage (years as rows)
# To add column inside of list-column dataframe:
## mutate(ffc_percentiles = map_depth(ffc_percentiles, 1, ~mutate(.x, data_source="ffc_obs")))
## convert to dataframe (not listcol)
## unnest(cols=c(ffc_percentiles))

# SAVE SAVE SAVE
save(usgs_ffc_dat, file = "data_output/usgs_altered_ffc_observed.rda")


# Get Predicted Metrics ---------------------------------------------------

# All predicted metrics
tic(msg="Getting predicted percentiles")

usgs_ffc_pred <- usgs_list %>% 
  # run in parallel with furrr
  mutate(ffc_pred = furrr::future_imap(NHDV2_COMID, ~get_predicted_flow_metrics(.x))) %>% 
  unnest_longer(col=ffc_pred)
beepr::beep(2)
toc()  
# Getting predicted percentiles: 1067.47 sec elapsed

# SAVE SAVE SAVE
save(usgs_ffc_pred, file = "data_output/usgs_altered_ffc_predicted.rda")

# Run FFC: Evaluate Gage Alteration ---------------------------------------

# try using FFC with evaluate_gage_alteration
tst_eval <- ffcAPIClient::evaluate_gage_alteration(gage_id = "10259540", token = Sys.getenv("EFLOWS_TOKEN", ""))


