# 11 Identify top RI Flow Metrics
# summarize data from all GBMs

# Libraries ---------------------------------------------------------------

library(gt)
library(tidyverse)
library(sf)
library(viridis) # colors
library(rlang)
library(purrr)

# Load Data ---------------------------------------------------------------

# orig data
load("data_output/07_selected_bmi_csci_por_trim_w_huc_region.rda")
load("models/09_all_ri_all_regions_csci.rda")
hydroDat <- "POR"

## ONLY IF YOU NEED MODEL NAMES/DATA
## "all_ca_ffc_only", "central_valley", "great_basin", "north_coast", "south_coast", 
# modname <- "great_basin" # model name 
# bmiVar <- quote(csci) # select response var

# make pathnames
# (mod_savename <- tolower(paste0("08_gbm_", as_name(bmiVar), "_",hydroDat, "_",modname)))

# get the gbm model:
# (top_ri <- list.files(path="models/", pattern = paste0("^", mod_savename,"_RI_combined",".*\\.rds$")))
# top_ris <- read_rds(path=paste0("models/", top_ri))

# make sep and combine
# ri_all_ca <- top_ris %>% mutate(model="all_ca")
# ri_gbasin <- top_ris %>% mutate(model="great_basin")
# ri_scoast <- top_ris %>% mutate(model="south_coast")
# ri_ncoast <- top_ris %>% mutate(model="north_coast")
# ri_cvalley <- top_ris %>% mutate(model="central_valley")
# 
# # bind 
# ri_all_regions <- bind_rows(ri_all_ca, ri_gbasin, ri_cvalley, ri_ncoast, ri_scoast)
# 
# #save out for later
# save(ri_all_regions, file = "models/09_all_ri_all_regions_csci.rda")


# Make a Table of RI's ----------------------------------------------------

library(readxl)
ff_defs <- readxl::read_xlsx("docs/Functional_Flow_Metrics_List_and_Definitions_final.xlsx", range = "A1:F25", .name_repair = "universal", trim_ws = TRUE) 

# join with the full RI table
ri_table <- left_join(ri_all_regions, ff_defs, by=c("var"="Flow.Metric.Code"))

# drop unused factors in flow component:
ri_table <- ri_table %>% 
  mutate(flow_component=forcats::fct_drop(flow_component),
         var = as.factor(var),
         var = fct_reorder2(var, flow_component, var))

levels(ri_table$var)
levels(ri_table$flow_component)
summary(ri_table)

# Plot & Summarize All RI Combined ----------------------------------------

# most common hydrometric by flowdata type?
ri_table %>% group_by(flowdat, var) %>% 
  summarize(meanRI = mean(RI),
            sumRI = sum(RI)) %>% 
  top_n(5) %>% 
  arrange(flowdat, desc(meanRI))

# so best hydrometric across model?
ri_table %>% group_by(model, var) %>% 
  summarize(meanRI = mean(RI),
            medianRI = median(RI),
            maxRI = max(RI),
            SD = sd(RI)) %>% 
  top_n(5) %>% group_by(var) %>% tally() %>% arrange(desc(n))

# top metrics based on frequency across regions
# Wet_BFL_Mag_50     5
# DS_Mag_50          3
# DS_Tim             3
# Peak_5             3
# SP_ROC             3
# DS_Dur_WS          2


# Summary Table ALL CA -----------------------------------------------------------

library(glue)

model_name <- "All CA"
## ALL CA Table
# Create a gt table based on preprocessed table
ri_table %>%
  dplyr::filter(model=="all_ca", method=="mse") %>%
  dplyr::select(-c(Ymetric, flowdat, flow_component, method, model, Flow.Characteristic)) %>%
  dplyr::select(Flow.Component, var, Flow.Metric.Name:Flow.Metric.Description, RI) %>%
  arrange(Flow.Component, var) %>% #View() 
  gt() %>%
  tab_header(
    title = "Relative Influence of Functional Flow Metrics on CSCI",
    subtitle = glue::glue("Model {model_name}")
  ) %>%
  fmt_number(
    columns = vars(RI), decimals = 1, 
    drop_trailing_zeros = T
  )

# Summary Table Regional -----------------------------------------------------------

model_name <- "HUC Regions"

# Create a gt table based on preprocessed table
ri_table %>%
  dplyr::filter(!model=="all_ca", method=="mse") %>%
  dplyr::select(-c(Ymetric, flowdat, flow_component, method, Flow.Characteristic)) %>%
  pivot_wider(names_from = model, values_from = RI) %>% #View()
  dplyr::select(Flow.Component, var, Flow.Metric.Name:Unit, great_basin:south_coast) %>%
  arrange(Flow.Component, var) %>% #View() 
  gt() %>%
  tab_header(
    title = "Relative Influence of Functional Flow Metrics on CSCI",
    subtitle = glue::glue("Model {model_name}")
  ) %>%
  fmt_number(
    columns = vars(central_valley, north_coast, south_coast, great_basin), decimals = 1, 
    drop_trailing_zeros = T
  )

# Summary Plot ALL CA ------------------------------------------------------------

# Faceted by hydrodat and flow metrics:
ri_table %>% group_by(flowdat, var, flow_component) %>% 
  summarize(meanRI = mean(RI)) %>% 
  #top_n(5) %>% 
  arrange(desc(meanRI)) %>% 
  filter(flow_component!="General") %>% 
  ggplot(.) +
  geom_col(aes(x=var,#x=forcats::fct_reorder2(var, flow_component, var),
               y=meanRI, fill=flow_component), color="gray20", lwd=.1,
           position="dodge") +
  coord_flip() +
  scale_fill_viridis_d("Flow Component")+
  labs(x="", y="Mean Relative Inf (%)", subtitle="Top Flow Metrics across all BMI Metrics") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_grid(.~flowdat)

#ggsave(filename = "figs/09_faceted_RI_by_flowcomp_hydrodat.png", width = 9, height = 6, units = "in", dpi = 300)

# Faceted by BMI metrics and flow components:
# ri_table %>% group_by(flowdat, var, Ymetric, flow_component) %>% 
#   summarize(meanRI = mean(RI)) %>% 
#   #top_n(6) %>% 
#   arrange(desc(meanRI)) %>% 
#   filter(flow_component!="General", flowdat=="Annual") %>%  
#   ggplot(.) +
#   geom_col(aes(x=var, y=meanRI, fill=flow_component), color="gray20", lwd=.1, position="dodge") +
#   coord_flip() +
#   scale_fill_viridis_d("Flow Components")+
#   labs(x="", y="Mean Relative Inf (%)", subtitle="ANNUAL: Top Flow Metrics across BMI Metrics") +
#   theme_classic(base_family = "Roboto Condensed") +
#   facet_grid(.~Ymetric)
# 
# ggsave(filename = "figs/faceted_RI_by_flowcomp_bmi_ANNUAL.png", width = 9, height = 6, units = "in", dpi = 300)
  
# Faceted by BMI metrics and flow components:
# ri_table %>% group_by(flowdat, var, Ymetric, flow_component) %>% 
#   summarize(meanRI = mean(RI)) %>% 
#  # top_n(6) %>% 
#   arrange(desc(meanRI)) %>% 
#   filter(flow_component!="General", flowdat=="Lag1") %>%  
#   ggplot(.) +
#   geom_col(aes(x=var, y=meanRI, fill=flow_component), color="gray20", lwd=.1, position="dodge") +
#   coord_flip() +
#   scale_fill_viridis_d("Flow Components")+
#   labs(x="", y="Mean Relative Inf (%)", subtitle="LAG-1: Top Flow Metrics across BMI Metrics") +
#   theme_classic(base_family = "Roboto Condensed") +
#   facet_grid(.~Ymetric)
# 
# ggsave(filename = "figs/faceted_RI_by_flowcomp_bmi_LAG1.png", width = 9, height = 6, units = "in", dpi = 300)
#   
# # Faceted by BMI metrics and flow components:
# ri_table %>% group_by(flowdat, var, Ymetric, flow_component) %>% 
#   summarize(meanRI = mean(RI)) %>% 
#   # top_n(6) %>% 
#   arrange(desc(meanRI)) %>% 
#   filter(flow_component!="General", flowdat=="Lag2") %>%  
#   ggplot(.) +
#   geom_col(aes(x=var, y=meanRI, fill=flow_component), color="gray20", lwd=.1, position="dodge") +
#   coord_flip() +
#   scale_fill_viridis_d("Flow Components")+
#   labs(x="", y="Mean Relative Inf (%)", subtitle="LAG-2: Top Flow Metrics across BMI Metrics") +
#   theme_classic(base_family = "Roboto Condensed") +
#   facet_grid(.~Ymetric)
# 
# ggsave(filename = "figs/faceted_RI_by_flowcomp_bmi_LAG2.png", width = 9, height = 6, units = "in", dpi = 300)
# 
# 
