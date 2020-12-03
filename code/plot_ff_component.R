# functional flow component 

# get NFA and make mean annual flow

library(dataRetrieval)
library(sf)
library(tidyverse)
library(mapview)
library(purrr)
library(lubridate)
library(tidylog)


# GET SITES ---------------------------------------------------------------

# Site NFA 11427000
nfa <- "11427000"
paramCD <- "00060" # discharge (cfs) (temperature=00010, stage=00065)
dataInterval <- "dv" # daily interval

# Run function and clean/rename columns
nfa_meta <- dataRetrieval::whatNWISdata(siteNumber=nfa,
                                        service=dataInterval, 
                                        parameterCd=paramCD) %>% 
  dplyr::select(site_no, station_nm, dec_lat_va, dec_long_va,
                dec_coord_datum_cd, alt_va, huc_cd, data_type_cd,
                parm_cd, stat_cd, begin_date:count_nu) %>%
  # rename cols
  dplyr::rename(interval=data_type_cd, lat = dec_lat_va, lon=dec_long_va,
                huc8=huc_cd, site_id=site_no, date_begin=begin_date,
                date_end=end_date, datum=dec_coord_datum_cd, elev_m=alt_va) %>%
  sf::st_as_sf(coords=c("lon","lat"), crs=4269, remove=FALSE)


# GET DATA ----------------------------------------------------------------

nfa_dat <- dataRetrieval::readNWISdv(siteNumbers=nfa, parameterCd = paramCD) 

# cleanup
nfa_df <- nfa_dat %>% addWaterYear() %>% 
  renameNWISColumns() %>% wateRshedTools::add_WYD(., "Date")



# Summarize ---------------------------------------------------------------

nfa_mean_daily <- nfa_df %>% group_by(DOWY,site_no) %>% 
  summarize(mean_daily_Q = mean(Flow, na.rm=T),
            q10 = quantile(Flow, 0.1),
            q90 = quantile(Flow, 0.9))

# quick plot
gg1 <- ggplot() + 
  theme_classic(base_size = 14) +
  geom_ribbon(data=nfa_mean_daily, aes(x=DOWY, ymin=q10, ymax=q90), fill="gray50", color="gray50", lwd=0.3, alpha=0.3) +
  geom_line(data=nfa_mean_daily, aes(x=DOWY, y=mean_daily_Q), color="steelblue", lwd=1.7) +
  scale_x_continuous("", breaks = c(1, 93, 183, 275), labels = c("Oct", "Jan", "Apr", "Jul"))+
  labs(y="Mean Daily Discharge (cfs)") 
       #caption="Mean daily discharge (10th & 90th percentiles in gray) \n for NF American River, USGS 11427000")

gg1
# make interactive
#plotly::ggplotly(gg1)

## add flow components
gg1 + 
  # FALL PULSE
  annotate("rect", xmin=10, xmax=20, ymin=10, ymax=420, alpha=0.4, color="goldenrod1", fill="goldenrod") +
  annotate("segment", x = 15, xend = 13, y = 850, yend = 425, colour = "gray10", size=0.8, arrow=arrow(angle = 25, length = unit(0.1, "inches"))) +
  annotate("text", x = 15, y = 1000, colour = "gray10", label="Fall pulse", size=6, fontface=2) +
  # PEAK FLOW 1 (UPPER)
  annotate("rect", xmin=100, xmax=118, ymin=5500, ymax=5780, alpha=0.4, color="darkblue", fill="darkblue") +
  annotate("segment", x = 136, xend = 114, y = 4950, yend = 5475, colour = "gray10", size=0.8, arrow=arrow(angle = 25, length = unit(0.15, "inches"))) +
  annotate("text", x = 144, y = 4800, colour = "gray10", label="Peak flow", fontface=2, size=6) +
  # PEAK FLOW 2 (LOWER)
  annotate("rect", xmin=138, xmax=148, ymin=3700, ymax=3950, alpha=0.4, color="darkblue", fill="darkblue") +
  annotate("segment", x = 140, xend = 143, y = 4650, yend=3960, colour = "gray10", size=0.8, arrow=arrow(angle = 25, length = unit(0.15, "inches"))) +
  # WINTER BASE FLOW
  annotate("rect", xmin=80, xmax=227, ymin=800, ymax=2000, alpha=0.4, color="cornflowerblue", fill="cornflowerblue") +
  annotate("text", x = 163, y = 950, colour = "gray10", label="Wet-season base flow", fontface=2, size=6) +
  # SPRING RECESSION
  annotate("rect", xmin=230, xmax=309, ymin=30, ymax=3400, alpha=0.3, color="seagreen", fill="seagreen") +
  annotate("segment", x = 270, xend = 270, y =3850 , yend=3405, colour = "gray10", size=0.8, arrow=arrow(angle = 25, length = unit(0.15, "inches"))) +
  annotate("text", x = 270, y = 4000, colour = "gray10", label="Spring recession", fontface=2, size=6) +
  # DRY SEASON
  annotate("rect", xmin=310, xmax=365, ymin=0, ymax=200, alpha=0.3, color="firebrick4", fill="firebrick1") +
  annotate("segment", x = 343, xend = 343, y = 700 , yend=210, colour = "gray10", size=0.8, arrow=arrow(angle = 25, length = unit(0.1, "inches"))) +
  annotate("text", x = 343, y = 1000, colour = "gray10", label="Dry-season\nlow flow", fontface=2, size=6)

ggsave(filename = "figs/ffcomponent_w_nfa_data.png", units = "in", width = 11, height = 8, dpi = 300)

