# Get Altered Gage FFM 

# Libraries ---------------------------------------------------------------

# MAIN
devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
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

# look at just one
glimpse(g1$alteration)


# 01. DOWNLOAD Multiple Gages IN LIST ------------------------------

# Error wrapper to return NA if the gage data doesn't exist/not enough data
get_ffc_eval <- possibly(evaluate_gage_alteration, otherwise=NA_real_)

# this works for a list of gages and adds list cols, no plots
tic(msg = "Finished Getting Data") # time start
g800 <- usgs_list %>% 
  slice(701:814) %>%  # pick only a subset from gage list
  split(.$gage_id) %>% # make into named list
  map(., ~{pluck(.x$gage_id)}) %>% # pull only the gage ID out
  furrr::future_imap(., 
                     ~get_ffc_eval(gage_id = .x, 
                                   token = ffctoken, 
                                   plot_results = FALSE), 
                     .progress = TRUE) 
beepr::beep(2) # something fun to let you know it's done
toc()

# Make sure the list is named
#names(gAll)

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
#gAll_filt <- f_remove_empty(gAll)
#names(gAll_filt)

# completed
#g100f <- f_remove_empty(g100)
#g200f <- f_remove_empty(g200)
#g300f <- f_remove_empty(g300)
#g400f <- f_remove_empty(g400)
#g500f <- f_remove_empty(g500)
#g550f <- f_remove_empty(g550)
#g600f <- f_remove_empty(g600)
#g700f <- f_remove_empty(g700)
g800f <- f_remove_empty(g800)

# SAVE OUT FULL LIST HERE

# FIRST TIME: bind together multiple runs into one file
# usgs_ffc_alt <- append(x = g100f, values=c(g200f, g300f)) # first time

# Subsequently
load("data_output/usgs_altered_ffc_list.rda") # load existing dataset
usgs_ffc_alt <- append(x = usgs_ffc_alt, values=c(g800f)) # merge

# save
save(usgs_ffc_alt, file = "data_output/usgs_altered_ffc_list.rda")

# so have 84% of the data

# 03. GET PERCENTILES AND FLATTEN TO DF -------------------------------

# Get all the Percentiles and put into one single large dataframe
g_alt_percentiles <- map(usgs_ffc_alt, ~{bind_rows(.x$ffc_percentiles,  .x$predicted_percentiles)}) %>% bind_rows(., .id = "list_id") # since gage_id already in df (just a double check here)

# save 
save(g_alt_percentiles, file = "data_output/usgs_altered_ffc_percentiles.rda")
write_csv(g_alt_percentiles, path="data_output/usgs_altered_ffc_percentiles.csv")

# 04. GET ALL FFM AND FLATTEN TO DF -----------------------------------

# Get all the FFM and put into one single large dataframe
g_alt_ffc <- map(usgs_ffc_alt, ~.x$ffc_results) %>% 
  bind_rows(., .id="gage_id") # gage_id not in this df

# save 
save(g_alt_ffc, file = "data_output/usgs_altered_ffc_metrics.rda")
write_csv(g_alt_ffc, path="data_output/usgs_altered_ffc_metrics.csv")


# 05. GET ALL ALTERATION STATUS AND FLATTEN TO DF ---------------------

g_alt_alt <- map(usgs_ffc_alt, ~.x$alteration) %>% 
  bind_rows(., .id="list_id") # since gage_id already in df

# save 
save(g_alt_alt, file = "data_output/usgs_altered_ffc_alteration.rda")
write_csv(g_alt_alt, path="data_output/usgs_altered_ffc_alteration.csv")
