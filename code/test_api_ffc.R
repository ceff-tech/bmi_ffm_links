library(tidyverse)
options(dplyr.print_max = 50)
library(lubridate)

#load("data/usgs_Q_daily_to_review.rda")
#ffm_missing <-  read_rds("data_output/06_ffm_gages_missing_metrics.rds")

# get gageNo
gageNo <- "11427000"

# check by gage number and return XY data
gageInfo <- dataRetrieval::findNLDI(nwis = gageNo)
gageInfo

# use XY to get comid
gageCOMID <- ffcAPIClient::get_comid_for_lon_lat(longitude = gageInfo$X, latitude = gageInfo$Y)
gageCOMID

# pull in some data
g1 <- dataRetrieval::readNWISdv(siteNumbers = gageNo, parameterCd = "00060")
g1 <- g1 %>% dataRetrieval::renameNWISColumns()

# clean
# format dataframe
g1_clean <- g1 %>% janitor::clean_names() %>% 
  select(flow, date) %>% as.data.frame()


# FFC ---------------------------------------------------------------------

library(ffcAPIClient)
# set/get the token for using the FFC
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", "")) 

# get predicted data:
ffcAPIClient::get_predicted_flow_metrics(comid = gageCOMID, online = TRUE)

# return everything
tst <- FFCProcessor$new()
# works
#tst$set_up(gage_id = gageNo, token = ffctoken, comid=gageCOMID)

# error
tst$flow_field = "flow"
tst$date_field = "date"
tst$date_format_string <- "%Y-%m-%d"
tst$set_up(timeseries = g1_clean, token = ffctoken, comid=gageCOMID)

# using your own data:
tst$step_one_functional_flow_results(timeseries = g1_clean, comid=COMID,
                                     token = ffctoken, output_folder = "~/Downloads/ffc_api_tst/")

# observe percentiles
as_tibble(tst$alteration)
as_tibble(tst$ffc_percentiles)
# (tst$ffc_results) %>% View()

# TRY EVALUATE function
tst$step_three_assess_alteration()


ffcAPIClient::evaluate_alteration(
  timeseries_df = g1_clean,
  token = ffctoken,
  date_format_string = "%Y-%m-%d",
  plot_output_folder = "~/Downloads/ffc_api_tst2",
  #longitude = gageInfo$X, latitude = gageInfo$Y)
  comid=gageCOMID) # REQUIRED OR specify lat/lon

# this error
#Error in `[<-.data.frame`(`*tmp*`, timeseries$calendar_month < 10, "water_year",  : 
#                            missing values are not allowed in subscripted assignments of data #frames


# Clean Demo ---------------------------------------------------

library(ffcAPIClient)
library(dplyr)

# set/get the token for using the FFC
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", "")) 

# find out COMID for your lat/lon:
gageCOMID <- ffcAPIClient::get_comid_for_lon_lat(longitude = gageInfo$X, 
                                                 latitude = gageInfo$Y)

# set up a FFC processor
tst <- FFCProcessor$new()
tst$flow_field = "flow" # name of column in your dataframe
tst$date_field = "date" # name of column in your dataframe
tst$date_format_string <- "%Y-%m-%d" # match format of your date
tst$set_up(timeseries = g1_clean, token = ffctoken, comid=gageCOMID)

# run step one first
tst$step_one_functional_flow_results(timeseries = g1_clean, comid=COMID,
                                     token = ffctoken, output_folder = "~/Downloads/ffc_api_tst/")

# observe percentiles
as_tibble(tst$alteration)
as_tibble(tst$ffc_percentiles)
as_tibble(tst$ffc_results)

# TRY this instead of evaluate_alteration function
tst$step_three_assess_alteration()
tst$alteration
