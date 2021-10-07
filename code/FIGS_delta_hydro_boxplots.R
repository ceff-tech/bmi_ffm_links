# BOXPLOTS OF DELTA HYDRO
# R. Peek 2021

# Libraries ---------------------------------------------------------------

library(glue)
options(tidyverse.quiet = TRUE)
library(tidyverse) # load quietly
library(viridis) # colors
library(sf) # spatial operations
library(mapview) # web maps
# set background basemaps/default options:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron")

mapviewOptions(homebutton = FALSE, 
               basemaps=basemapsList, 
               viewer.suppress = FALSE,
               fgb = FALSE)

library(rlang)
library(tidylog)

# Load Data ------------------------------------------------

# load updated data:
bio_ffm<- read_rds("https://github.com/ryanpeek/flow_seasonality/blob/main/output/10_ffc_filtered_final_combined_rev.rds?raw=true")

## Add Delta Hydrology for FFM ---------------------------------------

# need to add a "delta hydro: delta_p50 = (p50_obs-p50_pred)/p50_pred)"
bio_ffm <- bio_ffm %>% ungroup() %>% 
  # add delta hydro
  mutate(delta_p50 = (p50_obs-p50_pred) / p50_pred) %>% 
  # fix zeros and NaNs
  mutate(delta_p50 = case_when(
    is.infinite(delta_p50) ~ 0, # replace "Not a Number" w zero
    is.nan(delta_p50) ~ 0, # replace "Not a Number" w zero
    delta_p50 == NaN ~ NA_real_,
    TRUE ~ delta_p50
  )) %>% 
  relocate(delta_p50, .after = "p50_pred") %>% 
  mutate(delta_p50_scale = 
           as.vector(scale(delta_p50, scale=TRUE)), 
         .after="delta_p50") %>% 
  filter(!is.na(delta_p50))

# summarize
summary(bio_ffm$delta_p50, useNA="ifany")
summary(bio_ffm$delta_p50_scale, useNA="ifany")
bio_ffm %>% select(contains("p50")) %>% summary
bio_ffm %>% select(contains("csci"), contains("bio")) %>% summary

## Check for Duplicates ------------------------------------

# check for duplicates
bio_ffm %>%
  filter(bioindicator=="CSCI") %>%
  distinct(StationCode, gageid, biovalue, .keep_all=TRUE) %>%
  group_by(StationCode, gageid) %>%
  tally() %>%
  arrange(desc(n)) %>% #View()
  filter(n>1) %>% nrow() # CSCI n= 364 sites total
  #filter(n>1) %>%  nrow() # # CSCI n= 147 sites w dups

# Make a Table of RI's ----------------------------------------------------

library(readxl)
ff_defs <- readxl::read_xlsx("docs/Functional_Flow_Metrics_List_and_Definitions_final.xlsx", range = "A1:F25", .name_repair = "universal", trim_ws = TRUE) 

# join with the full RI table
bio_ffm_ord <- left_join(bio_ffm, ff_defs, by=c("metric"="Flow.Metric.Code"))

# drop unused factors in flow component:
bio_ffm_ord <- bio_ffm_ord %>% 
  mutate(flow_component=forcats::fct_drop(Flow.Component),
         Flow.Metric.Name = case_when(
           metric == "Power.avg" ~ "Wavelet Interannual",
           metric == "MP_metric" ~ "Colwell's M/P",
           TRUE ~ Flow.Metric.Name),
         Flow.Metric.Name = as.factor(Flow.Metric.Name),
         Flow.Metric.Name = forcats::fct_reorder(Flow.Metric.Name, Flow.Component)) 

# GET Colors
# darker for peak flow
flowcomponent_colors <- c("Fall pulse flow" = "#F0E442", "Wet-season baseflow" = "#56B4E9",
                          "Peak flow" = "#404788FF", "Spring recession flow" = "#009E73", 
                          "Dry-season baseflow" = "#D55E00") 
                          #"Seasonality" = "gray")



## Box Plots ------------------------------------------------------------

# need to make this by flow component colors 
bio_ffm_ord %>%
  #filter(bioindicator=="ASCI") %>% 
  # some extreme values so filter to everything below 98 quantile
  #filter(delta_p50 < quantile(bio_ffm$delta_p50, na.rm=TRUE, 0.98)) %>% 
  ggplot() +
  geom_jitter(aes(x=Flow.Metric.Name, y=delta_p50), col="gray",
              alpha=0.4, show.legend = FALSE) +
  geom_hline(yintercept = 0, color="deeppink3", alpha=1, lwd=1.3, lty=1) +
  geom_boxplot(aes(x=Flow.Metric.Name, y=delta_p50, fill=flow_component), 
               alpha=0.8, show.legend = TRUE, outlier.shape = NA) +
  cowplot::theme_cowplot(font_family = "Roboto Condensed") +
  labs(x="", y="Delta Hydrology ([p50 obs - p50 pred]/p50 pred)") +
  ylim(c(-1,2.5)) +
  scale_color_manual("Flow Component", values=flowcomponent_colors, guide="none") +
  scale_fill_manual("Flow Component", values=flowcomponent_colors) + 
  coord_flip() +
  #labs(subtitle = "ASCI") +
  theme(plot.background = element_rect(fill="white"),
        panel.border = element_rect(color="gray30")) +
  facet_wrap(.~bioindicator)

ggsave(filename = "figs/fig4_boxplot_of_asci_csci_ffm_by_deltaH.tiff", 
        width = 20, height = 15.545, 
        dpi=300, units="cm")


# Point Plot Outliers -------------------------------------------------------

bio_ffm_ord %>%
  # some extreme values so filter to everything below 98 quantile
  filter(delta_p50 > quantile(delta_p50, na.rm=TRUE, 0.98)) %>% 
  ggplot() +
  geom_point(aes(x=Flow.Metric.Name, y=delta_p50, fill=flow_component), pch=21,
              alpha=0.8, show.legend = FALSE, size=4) +
  cowplot::theme_cowplot(font_family = "Roboto Condensed") +
  labs(x="", y="Delta Hydrology", subtitle = ">98 percentile values") +
  scale_color_manual("Flow Component", values=flowcomponent_colors, guide="none") +
  scale_fill_manual("Flow Component", values=flowcomponent_colors) + 
  coord_flip() +
  theme(plot.background = element_rect(fill="white"),
        panel.border = element_rect(color="gray")) +
  facet_grid(.~bioindicator)

ggsave(filename = "figs/points_of_asci_csci_ffm_by_deltaH_outliers_98.png", 
       width = 11, height = 8, 
       dpi=300, units="in")


# Outlier Maps ------------------------------------------------------------


# where are they?
bio_ffm %>%
  #filter(bioindicator == "ASCI") %>% 
  # some extreme values so filter to everything below 98 quantile
  filter(delta_p50 > quantile(delta_p50, na.rm=TRUE, 0.97)) %>% 
  #filter(delta_p50 > 10) %>% 
  distinct(delta_p50, .keep_all=TRUE) %>% 
  st_as_sf(coords=c("usgs_lon", "usgs_lat"), crs=4326, remove=FALSE) %>% 
  mapview(zcol="delta_p50", layer.name="Delta H<br>>98%")

