# Get Altered Gage FFM 

# Libraries ---------------------------------------------------------------

# MAIN
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library(ffcAPIClient)

# set/get the token for using the FFC
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", "")) 

# SUPPPORTING
library(tidyverse) # yes
library(sf)
library(tictoc) # timing stuff
library(furrr) # parallel processing for mapping functions/loops (purrr)
#library(tidylog) # good for logging what happens

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

# Get ALL ALTERED Gages ---------------------------------------------------------------

# this is from TED

# read in ALL altered gage list (from Ted)
usgs_list <- read_csv("data/usgs/usgs_gages_final_altered_list.csv")

# clean up: drop the "T" from the ID, drop cols
usgs_list <- usgs_list %>% 
  mutate(gage_id=as.integer(gsub("^T",replacement = "", ID))) %>% 
  select(gage_id, ID:LONGITUDE, FLOW_YRS_POST_1950:REF_END_YEAR) 

# look at reference?
table(usgs_list$FINAL_REFERENCE) # 30=Y here, 
# the Y are included in reference data but pre-regulation, 
# so some stretch of years is a mix of ref/altered

# Test with a Single Gage -------------------------------------------------

# this works for a single gage and doesn't show plots
# g1 <- usgs_list %>% 
#   slice(5) %>% 
#   select(gage_id, NHDV2_COMID) %>% 
#   evaluate_gage_alteration(gage_id = .$gage_id,  
#                            comid = .$NHDV2_COMID,
#                            token = ffctoken, 
#                            plot_results = FALSE)   

# here we get a list of 5 df
# names(g1)
# glimpse(g1$alteration)

# Test with multi Gages w Purrr -------------------------------------------------------

# try in loop fashion
g2 <- usgs_list %>% 
  slice(1:5) %>%  # pick only a subset from gage list
  split(.$gage_id) %>% # make into named list
  map(., ~{select(.x, gage_id, NHDV2_COMID)}) %>% # pull gage ID out
  map(., ~get_ffc_eval(gage_id = .x$gage_id, 
                       token = ffctoken,
                       comid = .x$NHDV2_COMID,
                       force_comid_lookup = FALSE,
                       plot_results = FALSE))
  
# here we get a list of the gages
names(g2)

# look at first gage in list
glimpse(g2[[1]]$alteration)

# drop/check for NAs
length(g2)
g2_f <- f_remove_empty(g2) # so dropped 2
length(g2_f)

# rm
#rm(g2, g2_f)

# 01. DOWNLOAD Multiple Gages IN LIST ------------------------------

# RUN
tic(msg = "Finished Getting Data") # time start
g800 <- usgs_list %>% 
  slice(601:814) %>%  # pick a subset of rows from gage list
  split(.$gage_id) %>% # make into named list
  map(., ~{select(.x, gage_id, NHDV2_COMID)}) %>% # pull gage ID out
  furrr::future_imap(., 
                     ~get_ffc_eval(gage_id = .x$gage_id, 
                                   token = ffctoken,
                                   comid = .x$NHDV2_COMID,
                                   force_comid_lookup = FALSE,
                                   plot_results = FALSE), 
                     .progress = TRUE) 
beepr::beep(2) # something fun to let you know it's done
toc()

# 02. CLEAN NAs ----------------------------------------------------

# completed
#g100f <- f_remove_empty(g100)
#g200f <- f_remove_empty(g200)
g400f <- f_remove_empty(g400)
g600f <- f_remove_empty(g600)
g800f <- f_remove_empty(g800)

# 03. SAVE OUT ------------------------------------------------------------

# FIRST TIME THROUGH 
# bind together multiple runs into one file
usgs_ffc_alt <- append(x = g400f, values=c(g600f, g800f)) 

# save
save(usgs_ffc_alt, file = "data_output/usgs_altered_ffc_list.rda")

# If saving and rerunning/adding things subsequently
#load("data_output/usgs_altered_ffc_list.rda") # load existing dataset

# update/add more data
#usgs_ffc_alt <- append(x = usgs_ffc_alt, values=c(g800f))

# so 576/814 = 70% of gages we could get data for

# 03. GET PERCENTILES AND FLATTEN TO DF -------------------------------

# Get all the Percentiles and put into one single large dataframe
g_alt_percentiles <- map(usgs_ffc_alt, ~{bind_rows(.x$ffc_percentiles,  .x$predicted_percentiles)}) %>% bind_rows(., .id = "list_id") # since gage_id already in df (just a double check here)

# g_alt_percentiles %>% distinct(gage_id)

# save 
save(g_alt_percentiles, file = "data_output/usgs_altered_ffc_percentiles.rda")
write_csv(g_alt_percentiles, path="data_output/usgs_altered_ffc_percentiles.csv")

# 04. GET ALL FFM AND FLATTEN TO DF -----------------------------------

# Get all the FFM and put into one single large dataframe
g_alt_ffc <- map(usgs_ffc_alt, ~.x$ffc_results) %>% 
  bind_rows(., .id="gage_id") # gage_id not in this df

# g_alt_ffc %>% distinct(gage_id)

# save 
save(g_alt_ffc, file = "data_output/usgs_altered_ffc_metrics.rda")
write_csv(g_alt_ffc, path="data_output/usgs_altered_ffc_metrics.csv")


# 05. GET ALL ALTERATION STATUS AND FLATTEN TO DF ---------------------

load("data_output/usgs_altered_ffc_list.rda")

# first pull all alteration dataframes out
g_alt_alt <- map(usgs_ffc_alt, ~.x[["alteration"]])

# then convert all logicals to text before bind rows together
g_alt_alt <- rapply(g_alt_alt, 
                    as.character, # function to apply
                    "logical", # class to match in df
                      how="replace") %>% # what to do w match
     bind_rows()

# save 
save(g_alt_alt, file = "data_output/usgs_altered_ffc_alteration.rda")
write_csv(g_alt_alt, path="data_output/usgs_altered_ffc_alteration.csv")


# 06. GET WYT PERCENTILES -------------------------------------------------

# Get all the Percentiles and put into one single large dataframe
g_alt_wyt_percentiles <- map(usgs_ffc_alt, ~.x$predicted_wyt_percentiles) %>%
  bind_rows(., .id = "gage_id") # since gage_id already in df 
g_alt_wyt_percentiles %>% distinct(gage_id)

# save 
save(g_alt_wyt_percentiles, file = "data_output/usgs_altered_ffc_wytpercentiles.rda")
write_csv(g_alt_wyt_percentiles, path="data_output/usgs_altered_ffc_wytpercentiles.csv")
