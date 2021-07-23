library(tidyverse)
options(dplyr.print_max = 50)
library(lubridate)

load("data/usgs_Q_daily_to_review.rda")

ffm_missing <-  read_rds("data_output/06_ffm_gages_missing_metrics.rds")

# get gageNo
gageNo <- "10336626"
# get comid:
COMID <- 8943577

# filter to a gage dataset:
g_1 <- usgs_Q_daily %>% filter(site_no == gageNo)
ggplot(data=g_1, aes(x=Date, y=Flow)) + geom_line()


# FFC ---------------------------------------------------------------------

library(ffcAPIClient)
# set/get the token for using the FFC
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", "")) 

# get predicted data:
ffcAPIClient::get_predicted_flow_metrics(comid = COMID, online = TRUE)

# return everything
tst <- FFCProcessor$new()
tst$set_up(timeseries = g_1, token = ffctoken, comid=COMID)
tst$flow_field = "Flow"
tst$date_field = "Date"
tst$date_format_string <- "%Y-%m-%d"

# using your own data:
tst$step_one_functional_flow_results(timeseries = g_1, comid=COMID,
                                     token = ffctoken, output_folder = "tst")

# observe percentiles
as_tibble(tst$alteration)
as_tibble(tst$ffc_percentiles)
(tst$ffc_results) %>% View()

# run and get obs percentiles
ffc$gage_start_date = "1979-10-01"

ffc$run()
ffc$ffc_percentiles %>% as.data.frame()
