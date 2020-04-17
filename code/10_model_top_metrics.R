# 10_model_top_metrics


library(tidyverse)
library(viridis) # colors
library(sf)
#library(rlang)
#library(purrr)

# Load Data ---------------------------------------------------------------

# RI BRT data
bmi_RI_combined <- readRDS(file = "models/08_gbm_RI_csci_por.rds")

# orig data
bmi_csci_por <- read_rds("data_output/05_selected_bmi_stations_w_csci_ffm_alt_por.rds") %>% st_drop_geometry()

# load the metrics data
load("data_output/04_usgs_all_ffc_metrics.rda") 
ffm <- g_all_ffc %>% mutate(Year=as.integer(Year)) %>% 
  distinct() # filter out duplication
rm(g_all_ffc)

# make it long not wide for joins:
ffm <- pivot_longer(ffm, cols= c(DS_Tim_Julian:Peak_Fre_5), names_to = "ffm_metric", values_to = "ffm_value") %>% 
  # drop nas
  filter(!is.na(ffm_value))

# join for POR
bmi_ffm_por <- left_join(bmi_csci_por, ffm, by=c("ID"="gage_id", "metric"="ffm_metric"))

# join for ann data:
bmi_ffm_ann <- left_join(bmi_csci_por, ffm, by=c("ID"="gage_id", "sampleyear"="Year", "metric"="ffm_metric"))

# years -1  -2
lag_yrs_1 <- unique(bmi_csci_por$sampleyear) - 1
lag_yrs_2 <- unique(bmi_csci_por$sampleyear) - 2

# make lag data
ffm_lag1 <- ffm %>% filter(Year %in% lag_yrs_1) %>% 
  mutate(year_flow = Year-1) # add for labeling purposes
ffm_lag2 <- ffm %>% filter(Year %in% lag_yrs_2) %>% 
  mutate(year_flow = Year-2) # add for labeling purposes

# rejoin
bmi_ffm_lag1 <- left_join(bmi_csci_por, ffm_lag1, by=c("ID"="gage_id", "sampleyear"="Year", "metric"="ffm_metric"))
bmi_ffm_lag2 <- left_join(bmi_csci_por, ffm_lag2, by=c("ID"="gage_id", "sampleyear"="Year", "metric"="ffm_metric"))

# PEAK 5 ------------------------------------------------------------------

# look at Peak_5
bmi_ffm_por %>% filter(metric=="Peak_5") %>%
  filter(!is.na(ffm_value)) %>% View()


# plot
(gg1 <- ggplot(data=bmi_ffm_ann %>% filter(metric=="Peak_5"), 
               aes(x=csci, y=ffm_value)) + 
    #scale_fill_viridis("Year")+
    geom_point(pch=21, size=4) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),  
                color="maroon")+
    theme_minimal())

# plot
(gg1b <- ggplot(data=bmi_ffm_por %>% filter(metric=="Peak_5"), 
              aes(x=csci, y=ffm_value)) + 
  geom_point(pch=21, color="maroon", size=4) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),  
              color="maroon")+
  theme_minimal())

plotly::ggplotly(gg1b)

cowplot::plot_grid(gg1, gg1b, nrow=2)


# DS_MAG_50 ---------------------------------------------------------------

bmi_ffm_por %>% filter(metric=="DS_Mag_50") %>% view()

(gg2 <- ggplot(data=bmi_ffm_ann %>% filter(metric=="DS_Mag_50"), 
       aes(x=csci, y=ffm_value, fill=as.factor(status_code)), size=3) + 
  geom_point(pch=21, color="steelblue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),  
              color="steelblue")+
  theme_minimal())

(gg2b <- ggplot(data=bmi_ffm_por %>% filter(metric=="DS_Mag_50"), 
              aes(x=csci, y=ffm_value)) + 
  geom_point(pch=21, color="steelblue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),  
              color="steelblue")+
  theme_minimal())
gg2b

# WET_TIM -----------------------------------------------------------------

(gg3 <- ggplot() + 
  geom_point(data=bmi_ffm_por %>% filter(metric=="Wet_Tim"), 
             aes(x=csci, y=ffm_value, fill=as.factor(status_code)), 
             pch=21, size=3, color="seagreen") +
  ggthemes::scale_fill_colorblind("Status Code") +
  geom_smooth(data=bmi_ffm_ann %>% filter(metric=="Wet_Tim"), 
              aes(x=csci, y=ffm_value), 
              method = "gam", formula = y ~ s(x, bs = "cs"),  
              color="seagreen")+
  theme_minimal() +
  labs(title="Wet Timing"))

(gg3b <- ggplot() + 
  geom_point(data=bmi_ffm_ann %>% filter(metric=="Wet_Tim"), 
             aes(x=csci, y=ffm_value, fill=as.factor(status_code)), 
             pch=21, size=3, color="seagreen") +
  ggthemes::scale_fill_colorblind("Status Code") +
  geom_smooth(data=bmi_ffm_ann %>% filter(metric=="Wet_Tim"), 
              aes(x=csci, y=ffm_value), 
              method = "gam", formula = y ~ s(x, bs = "cs"),  
              color="seagreen")+
  theme_minimal() +
  labs(title="Wet Timing"))


# SP_ROC ------------------------------------------------------------------


(gg3 <- ggplot() + 
   geom_point(data=bmi_ffm_por %>% filter(metric=="SP_ROC"), 
              aes(x=csci, y=ffm_value, fill=as.factor(status_code)), 
              pch=21, size=3) +
   ggthemes::scale_fill_colorblind("Status Code") +
   geom_smooth(data=bmi_ffm_ann %>% filter(metric=="SP_ROC"), 
               aes(x=csci, y=ffm_value), 
               method = "gam", 
               #formula = y ~ s(x, bs = "cs"),  
               color="purple2")+
   theme_minimal() +
   labs(title="Spring Recession Rate", y="Spring Recession Rate", x="CSCI"))

(gg3b <- ggplot() + 
    geom_point(data=bmi_ffm_ann %>% filter(metric=="SP_ROC"), 
               aes(x=csci, y=ffm_value, fill=as.factor(status_code)), 
               pch=21, size=3) +
    ggthemes::scale_fill_colorblind("Status Code") +
    geom_smooth(data=bmi_ffm_ann %>% filter(metric=="SP_ROC"), 
                aes(x=csci, y=ffm_value), 
                method = "gam", 
                #formula = y ~ s(x, bs = "cs"),  
                color="purple4")+
    theme_minimal() +
    labs(title="Spring Recession Rate: Ann"))

(gg3c <- ggplot() + 
    geom_point(data=bmi_ffm_lag1 %>% filter(metric=="SP_ROC"), 
               aes(x=csci, y=ffm_value, fill=as.factor(status_code)), 
               pch=21, size=3) +
    ggthemes::scale_fill_colorblind("Status Code") +
    geom_smooth(data=bmi_ffm_lag1 %>% filter(metric=="SP_ROC"), 
                aes(x=csci, y=ffm_value), 
                method = "gam", 
                #formula = y ~ s(x, bs = "cs"),  
                color="purple4")+
    theme_minimal() +
    labs(title="Spring Recession Rate: Lag1"))

(gg3d <- ggplot() + 
    geom_point(data=bmi_ffm_lag2 %>% filter(metric=="SP_ROC"), 
               aes(x=csci, y=ffm_value, fill=as.factor(status_code)), 
               pch=21, size=3) +
    ggthemes::scale_fill_colorblind("Status Code") +
    geom_smooth(data=bmi_ffm_lag2 %>% filter(metric=="SP_ROC"), 
                aes(x=csci, y=ffm_value), 
                method = "gam", 
                #formula = y ~ s(x, bs = "cs"),  
                color="purple4")+
    theme_minimal() +
    labs(title="Spring Recession Rate: Lag2"))



# FA_MAG ------------------------------------------------------------------


(gg5 <- ggplot(data=bmi_ffm_ann %>% filter(metric=="FA_Mag"), 
               aes(x=csci, y=ffm_value)) + 
   geom_point(pch=21, color="forestgreen") +
   geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
               color="forestgreen")+
   #scale_x_continuous(breaks = seq(0, 1.2, 0.2), limits = c(0,1.2)) +
   labs(x="CSCI", subtitle = "FA_Mag: Annual data")+
   theme_minimal())


(gg5b <- ggplot(data=bmi_ffm_por %>% filter(metric=="FA_Mag"), 
               aes(x=csci, y=ffm_value)) + 
    geom_point(pch=21, color="forestgreen") +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
                color="forestgreen")+
    scale_x_continuous(breaks = seq(0, 1.2, 0.2)) +
    labs(x="CSCI", subtitle = "FA_Mag: POR data")+
    theme_minimal())

# Cowplot them together ---------------------------------------------------

library(cowplot)

plot_grid(gg1, gg2, gg3, gg4, gg5, ncol=3)
plot_grid(gg1b, gg2b, gg3b, gg4b, gg5b, ncol=3)
