# Get Altered Gage FFM 

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

# read in altered gage list
usgs_list <- read_csv("data/FinalAlteredList.csv")

# clean up: drop the "T" from the ID, drop cols
usgs_list <- usgs_list %>% 
  mutate(gage_id=as.integer(gsub("^T",replacement = "", ID))) %>% 
  select(gage_id, ID:LONGITUDE, FLOW_YRS_POST_1950:REF_END_YEAR) 

# look at reference?
table(usgs_list$FINAL_REFERENCE) # 30=Y here, included in reference data but pre-regulation, so some stretch of years is a mix of ref/altered

# Test with a Single Gage -------------------------------------------------

# this works for a single gage and doesn't show plots
g1 <- usgs_list %>% 
  slice(5) %>% pull(gage_id) %>% 
  evaluate_gage_alteration(gage_id = ., 
                           token = ffctoken, 
                           plot_results = FALSE)  

# here we get a list of 5 df
names(g1)

# pull just one out
g1$alteration


# 01. DOWNLOAD Multiple Gages IN LIST ------------------------------

# Error wrapper to return NA if the gage data doesn't exist/not enough data
get_ffc_eval <- possibly(evaluate_gage_alteration, otherwise=NA_real_)

# this works for a list of gages and adds list cols, no plots
tic(msg = "Starting Download") # time start
g100 <- usgs_list %>% 
  slice(1:100) %>%  # pick only a subset from gage list
  split(.$gage_id) %>% # make into named list
  map(., ~{pluck(.x$gage_id)}) #%>% # pull only the gage ID out
  furrr::future_imap(., 
                     ~get_ffc_eval(gage_id = .x, 
                                   token = ffctoken, 
                                   plot_results = FALSE), 
                     .progress = TRUE) 
beepr::beep(2) # something fun to let you know it's done
toc()

gAll <- g100

# Make sure the list is named
names(gAll)

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

# remove NAs
gAll_filt <- f_remove_empty(gAll)
names(gAll_filt)

# SAVE OUT FULL LIST HERE

# bind together multiple runs into one file (can save and read in iteratively)
# usgs_ffc_alt <- append(g200, g300, g500)
usgs_ffc_alt <- gAll # rename

# save
save(usgs_ffc_alt, file = "data_output/usgs_altered_ffc.rda")


# 03. GET PERCENTILES AND FLATTEN TO DF -------------------------------

# Get all the Percentiles and put into one single large dataframe
gAll_percentiles <- map(gAll_filt, ~{bind_rows(.x$ffc_percentiles,  .x$predicted_percentiles)}) %>% bind_rows(., .id = "list_id") # since gage_id already in df (just a double check here)

# save 
save(gAll_percentiles, file = "data_output/usgs_altered_ffc_percentiles.rda")
write_csv(gAll_percentiles, path="data_output/usgs_altered_ffc_percentiles.csv")

# 04. GET ALL FFM AND FLATTEN TO DF -----------------------------------

# Get all the FFM and put into one single large dataframe
gAll_ffc <- map(gAll_filt, ~.x$ffc_results) %>% 
  bind_rows(., .id="gage_id") # gage_id not in this df

# save 
save(gAll_ffc, file = "data_output/usgs_altered_ffc_metrics.rda")
write_csv(gAll_ffc, path="data_output/usgs_altered_ffc_metrics.csv")


# 05. GET ALL ALTERATION STATUS AND FLATTEN TO DF ---------------------

gAll_alt <- map(gAll_filt, ~.x$alteration) %>% 
  bind_rows(., .id="list_id") # since gage_id already in df

# save 
save(gAll_alt, file = "data_output/usgs_altered_ffc_alteration.rda")
write_csv(gAll_alt, path="data_output/usgs_altered_ffc_alteration.csv")
