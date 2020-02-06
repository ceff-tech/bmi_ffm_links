# Get ref Gage FFM 

# Libraries ---------------------------------------------------------------

# MAIN
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library(ffcAPIClient)

# set/get the token for using the FFC
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", "")) 

# SUPPPORTING
library(tidyverse) # yes
library(tictoc) # timing stuff
library(furrr) # parallel processing for mapping functions/loops (purrr)
#library(tidylog) # good for logging what happens

# Get Gages ---------------------------------------------------------------

# read in ref gage list
load("data_output/02_selected_gages_for_ref_bmi.rda")

usgs_list <- sel_gages_bmi

# clean up: drop the "T" from the ID, drop cols
usgs_list <- usgs_list %>% 
  mutate(gage_id=as.integer(gsub("^T",replacement = "", ID))) %>% 
  select(gage_id, ID:LONGITUDE, FLOW_YRS_POST_1950:REF_END_YEAR) 

# look at reference?
table(usgs_list$FINAL_REFERENCE) # 30=Y here, included in reference data but pre-regulation, so some stretch of years is a mix of ref/ref

# Test with a Single Gage -------------------------------------------------

# this works for a single gage and doesn't show plots
g1 <- usgs_list %>% 
  slice(5) %>% select(gage_id, NHDV2_COMID) %>% 
  evaluate_gage_alteration(gage_id = .$gage_id,  
                           comid = .$NHDV2_COMID,
                           token = ffctoken, 
                           plot_results = FALSE)  

# here we get a list of 5 df
names(g1)

# look at just one
glimpse(g1$alteration)

# view
view(g1$alteration)

# 01. DOWNLOAD Multiple Gages IN LIST ------------------------------

# Error wrapper to return NA if the gage data doesn't exist/not enough data
get_ffc_eval <- possibly(evaluate_gage_alteration, otherwise=NA_real_)

# this works for a list of gages and adds list cols, no plots
tic(msg = "Finished Getting Data") # time start
gAll <- usgs_list %>% 
  #slice(1:4) %>%  # pick only a subset from gage list
  split(.$gage_id) %>% # make into named list
  map(., ~{pluck(.x$gage_id)}) %>% # pull gage ID out
  furrr::future_imap(., 
                     ~get_ffc_eval(gage_id = .x, 
                                   token = ffctoken,
                                   force_comid_lookup = TRUE,
                                   plot_results = FALSE), 
                     .progress = TRUE) 
#beepr::beep(2) # something fun to let you know it's done
toc()

# Make sure the list is named
#names(g50)

# 02. CLEAN NAs ----------------------------------------------------

# because some gages return NA, we have a list that contains some NA
# and some actual data. This function helps us remove the NAs in the list.

# Function to remove empty/NAs from the list
f_remove_empty <- function(x){
  if(is.list(x)) {
    x %>%
      purrr::discard(rlang::is_na) %>%
      purrr::map(f_remove_empty)
  } else {
    x
  }
}

# completed and Rm NAs
gAll_f <- f_remove_empty(gAll)

usgs_ffc_ref <- gAll_f

# SAVE OUT FULL LIST HERE

# save
save(usgs_ffc_ref, file = "data_output/usgs_ref_ffc_list.rda")

# so have 84% of the data

# 03. GET PERCENTILES AND FLATTEN TO DF -------------------------------

# Get all the Percentiles and put into one single large dataframe
g_ref_percentiles <- map(usgs_ffc_ref, ~{bind_rows(.x$ffc_percentiles,  .x$predicted_percentiles)}) %>% bind_rows(., .id = "list_id") # since gage_id already in df (just a double check here)

# save 
save(g_ref_percentiles, file = "data_output/usgs_ref_ffc_percentiles.rda")
write_csv(g_ref_percentiles, path="data_output/usgs_ref_ffc_percentiles.csv")

# 04. GET ALL FFM AND FLATTEN TO DF -----------------------------------

# Get all the FFM and put into one single large dataframe
g_ref_ffc <- map(usgs_ffc_ref, ~.x$ffc_results) %>% 
  bind_rows(., .id="gage_id") # gage_id not in this df

# save 
save(g_ref_ffc, file = "data_output/usgs_ref_ffc_metrics.rda")
write_csv(g_ref_ffc, path="data_output/usgs_ref_ffc_metrics.csv")


# 05. GET ALL ALTERATION STATUS AND FLATTEN TO DF ---------------------

g_ref_alt <- map(usgs_ffc_ref, ~.x$alteration) %>% 
  bind_rows(., .id="list_id") # since gage_id already in df

# save 
save(g_ref_alt, file = "data_output/usgs_ref_ffc_alteration.rda")
write_csv(g_ref_alt, path="data_output/usgs_ref_ffc_alteration.csv")
