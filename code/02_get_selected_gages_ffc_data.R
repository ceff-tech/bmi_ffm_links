# Get Ref/Altered Gage FFM 

# Libraries ---------------------------------------------------------------

# MAIN
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library(ffcAPIClient)

# set/get the token for using the FFC
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", "")) 

# SUPPORTING
library(tidyverse) # yes
library(sf)
library(tictoc) # timing stuff
library(furrr) # parallel processing for mapping functions/loops (purrr)
library(tidylog) # good for logging what happens

options(scipen = 100)

# Functions For Running Things --------------------------------------------

# Error wrapper to return NA if the gage data doesn't exist/not enough data
get_ffc_eval <- possibly(evaluate_gage_alteration, otherwise=NA_real_)

# Some gages return NA due to lack of data need a function to remove these NAs from a list
f_remove_empty <- function(x){
  if(is.list(x)) {
    x %>%
      purrr::discard(rlang::is_na) %>%
      purrr::map(f_remove_empty)
  } else {
    x
  }
}

# Prep Data ------------------------------------------------------------

# all gage data
load("data_output/01_usgs_all_gages.rda") # final gages list
table(usgs_final_all$CEFF_type)


# Get Altered -------------------------------------------------------------

usgs_alt_list <- usgs_final_all %>% 
  filter(CEFF_type=="ALT") %>% 
  st_drop_geometry()

# Get Reference ---------------------------------------------------------------

usgs_ref_list <- usgs_final_all %>% 
  filter(CEFF_type=="REF") %>% 
  st_drop_geometry()

# Get COMIDs (if needed) --------------------------------------------------

# get the comid
# library(nhdplusTools)
# 
# usgs_list2 <- usgs_list %>% st_transform(3310) %>% 
#   group_split(gage_id) %>%
#   set_names(., usgs_list$gage_id) %>%
#   map(~discover_nhdplus_id(.x$geometry))
# 
# usgs_list2 %>% 
#   purrr::map_lgl(~ length(.x)>1) %>% 
#   #table() # 3 are FALSE
#   .[.==TRUE] # get values that are TRUE
# 
# usgs_list2["11055801"]
# usgs_list2["11055801"] <- 2775510
# 
# # flatten into single dataframe instead of list
# usgs_comids <- usgs_list2 %>% flatten_dfc() %>% t() %>% 
#   as.data.frame() %>% 
#   rename("NHDV2_COMID"=V1) %>% rownames_to_column(var = "gage_id")
# 
# # rejoin to original dataset
# usgs_list <- usgs_list %>% 
#   left_join(., usgs_comids) %>% 
#   select(gage_id:comid2, NHDV2_COMID, station_nm:geometry)
# 
# # save out again
# save(usgs_list, file = "data_output/02_usgs_list_updated_comids.rda")

# 01. DOWNLOAD Multiple Gages IN LIST ------------------------------

# remove sf class here
usgs_list <- usgs_ref_list %>% 
  select(gage_id, NHDV2_COMID) %>% 
  rename(comid = NHDV2_COMID)

# RUN
tic(msg = "Finished Getting Data") # time start
g_200 <- usgs_list %>% 
  slice(101:250) %>%  # pick a subset of rows from gage list
  split(.$gage_id) %>% # make into named list
  map(., ~{select(.x, gage_id, comid)}) %>% # pull gage ID out
  furrr::future_imap(., 
                     ~get_ffc_eval(gage_id = .x$gage_id, 
                                   token = ffctoken,
                                   comid = .x$comid,
                                   force_comid_lookup = FALSE,
                                   plot_results = FALSE), 
                     .progress = TRUE) 
beepr::beep(2) # something fun to let you know it's done
toc()

rm(usgs_list)

# 02. CLEAN NAs ----------------------------------------------------

# completed
g100f <- f_remove_empty(g_100)
g200f <- f_remove_empty(g_200)
#g300f <- f_remove_empty(g_300)
#g600f <- f_remove_empty(g600)
#g800f <- f_remove_empty(g800)

# 03. SAVE OUT ------------------------------------------------------------

# FIRST TIME THROUGH 
# bind together multiple runs into one file
usgs_ffc_all <- append(x = g100f, values=c(g200f)) 

# save
save(usgs_ffc_all, file = "data_output/02_usgs_ref_ffc_list.rda")

# If saving and rerunning/adding things subsequently
#load("data_output/02_usgs_all_ffc_list.rda") # load existing dataset

# pull actual data now:
usgs_alt_dat <- usgs_ffc_alt[c(gages_we_have)]

# 03. GET PERCENTILES AND FLATTEN TO DF -------------------------------

# Get all the Percentiles and put into one single large dataframe
g_ref_percentiles <- map(usgs_ffc_all, ~{bind_rows(.x$ffc_percentiles,  .x$predicted_percentiles)}) %>% bind_rows(., .id = "list_id") # since gage_id already in df (just a double check here)

g_ref_percentiles %>% distinct(gage_id) %>% tally()

# save 
save(g_ref_percentiles, file = "data_output/02_usgs_ref_ffc_percentiles.rda")
write_csv(g_ref_percentiles, path="data_output/02_usgs_ref_ffc_percentiles.csv")

# 04. GET ALL FFM AND FLATTEN TO DF -----------------------------------

# Get all the FFM and put into one single large dataframe
g_ref_ffc <- map(usgs_ffc_all, ~.x$ffc_results) %>% 
  bind_rows(., .id="gage_id") # gage_id not in this df

g_alt_ffc <- map(usgs_ffc_all, ~.x$ffc_results) %>% 
  bind_rows(., .id="gage_id") # gage_id not in this df

# save 
save(g_ref_ffc, file = "data_output/02_usgs_ref_ffc_metrics.rda")
write_csv(g_ref_ffc, path="data_output/02_usgs_ref_ffc_metrics.csv")

# 05. GET ALL ALTERATION STATUS AND FLATTEN TO DF ---------------------

# first pull all alteration dataframes out
g_alt_ref <- map(usgs_ffc_all, ~.x[["alteration"]])

# then convert all logicals to text before bind rows together
g_alt_ref <- rapply(g_alt_ref, 
                    as.character, # function to apply
                    "logical", # class to match in df
                      how="replace") %>% # what to do w match
     bind_rows() %>% 
  mutate(gage_id=as.character(gage_id))

# save 
save(g_alt_ref, file = "data_output/02_usgs_ref_ffc_alteration.rda")
write_csv(g_alt_ref, path="data_output/02_usgs_ref_ffc_alteration.csv")


# 06. GET WYT PERCENTILES -------------------------------------------------

# Get all the Percentiles and put into one single large dataframe
g_ref_wyt_percentiles <- map(usgs_ffc_all, ~.x$predicted_wyt_percentiles) %>%
  bind_rows(., .id = "gage_id") # since gage_id already in df 
g_ref_wyt_percentiles %>% distinct(gage_id)

# g_alt_wyt_percentiles <- map(usgs_alt_dat, ~.x$predicted_wyt_percentiles) %>%
#   bind_rows(., .id = "gage_id") # since gage_id already in df 
# g_alt_wyt_percentiles %>% distinct(gage_id)

# bind
#g_all_wyt_percentiles <- bind_rows(g_all_wyt_percentiles, g_alt_wyt_percentiles)

# save 
save(g_ref_wyt_percentiles, file = "data_output/02_usgs_ref_ffc_wytpercentiles.rda")
write_csv(g_ref_wyt_percentiles, path="data_output/02_usgs_ref_ffc_wytpercentiles.csv")
