# Get Altered Gage FFM 


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tictoc)
#library(tidylog)

#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library(ffcAPIClient)
#options(scipen = 999) # turn of scientific notation
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", "")) 

# Get Gages ---------------------------------------------------------------

# read in altered gage list
usgs_list <- read_csv("data/FinalAlteredList.csv")

# clean up: drop the "T" from the ID, drop cols
usgs_list <- usgs_list %>% 
  mutate(gage_id=as.integer(gsub("^T",replacement = "", ID))) %>% 
  select(gage_id, ID:LONGITUDE, FLOW_YRS_POST_1950:REF_END_YEAR) 

# look at reference?
table(usgs_list$FINAL_REFERENCE) # 30=Y here, included in reference data but pre-regulation?

# Setup Error Functions for ffcAPI ----------------------------------------

# this was a workaround to return NAs if there were errors
# get_ffc <- possibly(get_ffc_results_for_usgs_gage, otherwise = NA_real_)
# tst <- get_ffc(10253080) # should give NA
# get_ffc_df <- possibly(get_results_as_df, otherwise = NA_real_)
# get_ffc_percentiles <- possibly(get_percentiles, otherwise = NA_real_)
get_ffc_eval <- possibly(evaluate_gage_alteration, otherwise=NA_real_)

# Gage Tests -------------------------------------------------------

# this works for a single gage and shows plots
g1 <- usgs_list %>% 
  slice(5) %>% pull(gage_id) %>% 
  evaluate_gage_alteration(., ffctoken, plot_results = TRUE)

# this works for a list of gages and adds list cols, no plots
g2 <- usgs_list %>% 
  slice(1:5) %>% 
  select(gage_id, NHDV2_COMID) %>% 
  mutate(
    alt_eval = furrr::future_imap(
      gage_id, ~evaluate_gage_alteration(.x, ffctoken, 
                                         plot_results = FALSE),
      .progress = TRUE)) 


# ALL ---------------------------------------------------------------------

tic()
# get ffc and percentiles (observed)
g500 <- usgs_list %>% 
  slice(301:500) %>% 
  select(gage_id, NHDV1_COMID) %>% split(.$gage_id) %>% 
  furrr::future_imap(., 
      ~get_ffc_eval(.x$gage_id, ffctoken, plot_results = FALSE),
      .progress = TRUE)
beepr::beep(2)
toc()

# bind and save
usgs_ffc_data <- append(g200, g300, g500)

# save out
save(usgs_ffc_data, file = "data_output/usgs_altered_ffc_observed.rda")


# get preds
gPreds <- usgs_list %>% 
  slice(1:10) %>% 
  select(NHDV2_COMID, gage_id) %>% split(.$NHDV2_COMID) %>% 
  map(., ~get_predicted_flow_metrics(.x$NHDV2_COMID))


# pwalk to get the alteration
gPercentiles <- map_df(gAll, ~.x["percentiles"])

#
#pwalk(list(gAll[[.x]][[2]], gAll[[..2]], ~assess_alteration(..1, ..2, ..3, ..4)))

# this works
#assess_alteration(percentiles = gAll[[4]][[2]], ffc_values = gAll[[4]][[1]], predictions = gPreds[[4]], comid = names(gPreds)[4])

# Load FFC Data -----------------------------------------------------------

# old version
#load("data_output/usgs_altered_ffc_observed.rda")

# Run FFC: Get FFC Results as DF ------------------------------------------

# use flow calculator to pull FFM for each gage, this works as raw option
tic()
usgs_ffc_dat_100 <- usgs_list %>% 
  slice(1:7) %>% # pick a few rows to test
  #mutate(alt_eval = furrr::future_imap(gage_id, ~get_ffc_eval(.x, ffctoken))) %>% 
  mutate(ffc_df = furrr::future_imap(gage_id, ~get_ffc(.x))) %>%
  mutate(ffc_df = furrr::future_imap(ffc_df, ~get_ffc_df(.x))) #%>% 
  #mutate(ffc_percentiles = furrr::future_imap(ffc_df, ~get_ffc_percentiles(.x))) %>%
  #unnest(cols = c(ffc_percentiles)) # pull out percentiles
beepr::beep(2)
toc()

# bind and save
usgs_ffc_data <- bind_rows(usgs_ffc_dat_100, usgs_ffc_dat_200, usgs_ffc_dat_300, usgs_ffc_dat_400, usgs_ffc_dat_500, usgs_ffc_dat_600, usgs_ffc_dat_700, usgs_ffc_dat_800)

# save out
save(usgs_ffc_data, file = "data_output/usgs_altered_ffc_observed.rda")

# check no of gages
usgs_ffc_data %>% distinct(gage_id) %>% tally()
usgs_ffc_data %>% group_by(Metric) %>% tally() %>% View()# so over half are NA

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

# SAVE SAVE SAVE
save(usgs_ffc_pred, file = "data_output/usgs_altered_ffc_predicted.rda")

# Run FFC: Evaluate Gage Alteration ---------------------------------------

# try using FFC with evaluate_gage_alteration
eval_alt <- function(x) {possibly(evaluate_gage_alteration(gage_id = x, token = Sys.getenv("EFLOWS_TOKEN", "")), NA_real_)}

tst_eval <- eval_alt(10253080)
#tst_eval <- ffcAPIClient::evaluate_gage_alteration(gage_id = 10253080, token = Sys.getenv("EFLOWS_TOKEN", ""))


usgs_ffc_eval <- usgs_list %>% 
  slice(1:2) %>% purrr::transpose(.names = usgs_list$gage_id)
  map(., ~evaluate_gage_alteration(.x, token = Sys.getenv("EFLOWS_TOKEN", "")))


# add col inside dataframe
# map2_df(dat2, stocks, ~update_list(.x, stock = .y))
