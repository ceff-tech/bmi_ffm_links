# Identify top RI Flow Metrics
# summarize data from all GBMs

# Libraries ---------------------------------------------------------------

library(gt)
library(tidyverse)
library(sf)
library(viridis) # colors
library(rlang)
library(glue)
library(purrr)

# load updated data w regions:
bio_ffm<- read_rds("https://github.com/ryanpeek/flow_seasonality/blob/main/output/10_ffc_filtered_final_combined.rds?raw=true")

# need to add a "delta hydro: delta_p50 = (p50_obs-p50_pred)/p50_pred)"
bio_ffm <- bio_ffm %>% 
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

# load RI outputs
load("models/09_asci_ri_all_ca.rda")
asci_ri_all <- ri_all_regions
load("models/09_csci_ri_all_ca.rda")
csci_ri_all <- ri_all_regions

rm(ri_all_regions)


# Save ALL data -----------------------------------------------------------

save(asci_ri_all, csci_ri_all, bio_ffm,
     file = "models/10_csci_asci_ri_por_trim_all_regions.rda")


# Merge ASCI/CSCI DATA ----------------------------------------------------

# add specific ASCI vs CSCI col
asci_ri_all <- asci_ri_all %>% mutate(index="ASCI")
csci_ri_all <- csci_ri_all %>% mutate(index="CSCI")

# combine
ri_all <- bind_rows(asci_ri_all, csci_ri_all)

# Make a Table of RI's ----------------------------------------------------

library(readxl)
ff_defs <- readxl::read_xlsx("docs/Functional_Flow_Metrics_List_and_Definitions_final.xlsx", range = "A1:F25", .name_repair = "universal", trim_ws = TRUE) 

# join with the full RI table
ri_table <- left_join(ri_all, ff_defs, by=c("var"="Flow.Metric.Code"))

# drop unused factors in flow component:
ri_table <- ri_table %>% 
  mutate(flow_component=forcats::fct_drop(flow_component),
         var = as.factor(var),
         var = fct_reorder2(var, flow_component, var),
         model=as.factor(model),
         Flow.Metric.Name = case_when(
           flow_component == "Stream Class" ~ "Stream Class",
           var == "Power.avg" ~ "Wavelet Interannual",
           var == "MP_metric" ~ "Colwell's M/P Intrannual",
           TRUE ~ Flow.Metric.Name),
         Flow.Metric.Name = as.factor(Flow.Metric.Name),
         Flow.Metric.Name = forcats::fct_reorder2(Flow.Metric.Name, model, RI)) 


# generate order by CA wide RI for flow metrics:
forder_csci <- ri_table %>% 
  filter(model=="all_ca", index=="CSCI",
         method=="mse") %>% 
  mutate(model=as.factor(model),
         Flow.Metric.Name = as.factor(Flow.Metric.Name),
         Flow.Metric.Name = forcats::fct_reorder2(Flow.Metric.Name, model, RI)) %>% 
  group_by(model) %>% 
  arrange(desc(model, RI)) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% arrange(id) %>% 
  select(Flow.Metric.Name,Ymetric, id) # get the just the names for ordering things

forder_asci <- ri_table %>% 
  filter(model=="all_ca", index=="ASCI",
         method=="mse") %>% 
  mutate(model=as.factor(model),
         Flow.Metric.Name = as.factor(Flow.Metric.Name),
         Flow.Metric.Name = forcats::fct_reorder2(Flow.Metric.Name, model, RI)) %>% 
  group_by(model) %>% 
  arrange(desc(model, RI)) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% arrange(id) %>% 
  select(Flow.Metric.Name, Ymetric, id) # get the just the names for ordering things

# join these together for later
forder_all <- full_join(forder_csci, forder_asci)

# Plot & Summarize All RI Combined ----------------------------------------

# highest RI hydrometric variable across both ASCI and CSCI
# and including all regions for Period of Record data
ri_table %>% filter(method=="mse") %>% 
  group_by(var) %>% # only group by hydrometric
  summarize(meanRI = mean(RI),
            medianRI = median(RI)) %>% 
  ungroup() %>% 
  arrange(desc(meanRI)) %>% 
  top_n(10)

# so most consistent hydrometric including all_ca + regional models in each ASCI and CSCI
ri_table %>% filter(method=="mse") %>% 
  group_by(index, var) %>% 
  summarize(meanRI = mean(RI),
            medianRI = median(RI)) %>% 
  top_n(10) %>% 
  arrange(index, desc(meanRI)) 
  

# Summary Table ALL CA -----------------------------------------------------------

library(glue)

model_name <- "All CA"
modname <- "all_ca"

## ALL CA Table
# Create a gt table based on preprocessed table
ri_table %>%
  dplyr::filter(model=="all_ca", method=="mse") %>%
  dplyr::select(-c(Ymetric, flowdat, Flow.Component, method, model, Flow.Characteristic)) %>%
  mutate(Flow.Metric.Name_unit = glue("{Flow.Metric.Name} ({Unit})")) %>% 
  dplyr::select(index, flow_component, Flow.Metric.Name_unit, Flow.Metric.Description, RI) %>%
  arrange(desc(RI), flow_component) %>% #View() 
  gt() %>%
  tab_header(
    title = "Relative Influence of Functional Flow Metrics on ASCI/CSCI",
    subtitle = glue::glue("Model {model_name}")
  ) %>%
  gt::cols_label(
    index = "Index",
    flow_component = "Flow Component",
    Flow.Metric.Description = "Description",
    Flow.Metric.Name_unit = "Flow Metric (unit)"
  ) %>% 
  fmt_number(
    columns = vars(RI), decimals = 1, 
    drop_trailing_zeros = T
  )

# # Summary Table Regional -----------------------------------------------------------
# 
# model_name <- "modified Ecoregions"
# 
# # Create a gt table based on preprocessed table
# ri_table %>%
#   dplyr::filter(!model=="all_ca", method=="mse") %>%
#   dplyr::select(-c(Ymetric, flowdat, flow_component, method, Flow.Characteristic)) %>%
#   pivot_wider(names_from = model, values_from = RI) %>%
#   mutate(Flow.Metric.Name_unit = glue("{Flow.Metric.Name} ({Unit})")) %>% 
#   dplyr::select(index, Flow.Component, Flow.Metric.Name_unit, so_cal:north_coast) %>%
#   arrange(index, Flow.Component) %>% # View() 
#   gt() %>%
#   tab_header(
#     title = "Relative Influence of Functional Flow Metrics on ASCI/CSCI",
#     subtitle = glue::glue("Using {model_name}")
#   ) %>%
#   gt::cols_label(
#     index = "Index",
#     Flow.Component = "Flow Component",
#     Flow.Metric.Name_unit = "Flow Metric (unit)",
#     sierras = "S. Nevada",
#     north_coast = "N. Coast",
#     cent_coast = "C. Coast",
#     so_cal = "So Cal") %>% 
#   fmt_number(
#     columns = vars(so_cal, cent_coast, sierras, north_coast), decimals = 1, 
#     drop_trailing_zeros = T
#   )


# Summary Plot ALL CA ------------------------------------------------------------

# color palette 
flowcomponent_colors <- c("Fall pulse flow" = "#F0E442", "Wet-season baseflow" = "#56B4E9",
                          "Peak flow" = "#404788FF", "Spring recession flow" = "#009E73", 
                          "Dry-season baseflow" = "#D55E00",
                          "Stream Class" = "#9370DB", "Seasonality" = "gray")

# Faceted by hydrodat and flow metrics:
ri_table %>% 
  filter(model=="all_ca", 
         method=="mse") %>% 
  ggplot() +
  #geom_hline(yintercept = 5, color="gray40", lwd=0.8, lty=2, alpha=0.5)+
  geom_linerange(aes(x=reorder(Flow.Metric.Name, RI),
                     ymax=RI, ymin=0, group=model, color=flow_component), 
                 lwd=.5, show.legend = F, alpha=0.7, lty=1)+
  geom_point(aes(x=Flow.Metric.Name, y=RI, group=model, 
                 shape=index, 
                 color=flow_component,
                 fill=flow_component,
                 size=RI)) +
  geom_point(aes(x=Flow.Metric.Name, y=RI, group=model, 
                 shape=index, size=RI), 
             color="black", show.legend = FALSE, alpha=0.8) +
  scale_shape_manual("Index", values=c("ASCI"=23, "CSCI"=21))+
  scale_color_manual("Flow Component", values=flowcomponent_colors) +
  scale_fill_manual("Flow Component", values=flowcomponent_colors, guide="none") +
  scale_size_area("", guide=FALSE) +
  #scale_shape_manual("Method", values=c("mse"=16, "permtest"=17))+
  coord_flip() +
  ylim(c(0,20))+
  labs(title = glue::glue('All Site Pairs (CA)'),
       x="", y="Relative Influence (%)") +
  # fix legend
  guides(color = guide_legend(override.aes = list(size = 4, pch=21, 
                                                  fill=flowcomponent_colors, color="gray40"),
                              order = 1),
         shape = guide_legend(override.aes = list(size = 4),
                              order = 2),
         size= "none") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(legend.position = c(0.8, 0.3),
        plot.background = element_rect(fill="white"),
        legend.background = element_rect(color="white"))

# save out
ggsave(filename=tolower(glue("figs/10_gbm_asci_csci_{modname}_ri_sized_points_w_lines_ranked.png")), width = 9, height = 7, units = "in", dpi = 300)

# # Summary Plot Regions ------------------------------------------------------------
# 
# # now plot w facets (but use same ordering for ALL CA)
# ri_table %>% 
#   filter(model!="all_ca",
#          method=="mse") %>% 
#   left_join(., forder_asci, by="Flow.Metric.Name") %>% 
#   arrange(id) %>% #View() 
#   ggplot() +
#   facet_grid(cols = vars(model), labeller = labeller(model=c("all_ca"="All CA", "cent_coast"="C. Coast", "sierras"="Sierra Nevada", "north_coast"="N. Coast", "so_cal"="S. California"))) +
#   geom_linerange(aes(x=reorder(Flow.Metric.Name, desc(id)), ymax=RI, ymin=0, color=flow_component, group=model), 
#                   lwd=.5, show.legend = F, alpha=0.7, lty=1)+
#   geom_point(aes(x=reorder(Flow.Metric.Name, desc(id)), y=RI,
#                  fill=flow_component, size=RI, group=model, shape=index),
#              color="black", alpha=0.8) +
#   scale_fill_manual("Flow Component", values=flowcomponent_colors) +
#   scale_color_manual("Flow Component", values=flowcomponent_colors, guide=FALSE) +
#   scale_shape_manual("Index", values=c("ASCI"=23, "CSCI"=21))+
#   scale_size_area("", guide=FALSE) +
#   coord_flip() +
#   ylim(c(0,27))+
#   labs(title = "ASCI & CSCI BRT Models",
#        x="", y="Relative Influence (%)") +
#   theme_minimal(base_family = "Roboto Condensed") +
#   theme(legend.position = "bottom", 
#         legend.box="vertical",
#         legend.spacing.y = unit(-0.1,"cm"),
#         legend.justification = "center",
#         legend.margin = margin(6,0,6, 0),
#         legend.box.spacing = unit(-.01,"cm"),
#         legend.background = element_rect(color="white")) +
#   guides(fill = guide_legend(override.aes = 
#                                list(size = 4, pch=21, fill=flowcomponent_colors, 
#                                     color="gray40"), nrow = 1, order = 1, direction="horizontal"),
#          shape = guide_legend(override.aes = list(size = 4,
#                                                   direction="horizontal"), order = 2),
#          size= "none")
# 
# # REGIONS ONLY
# ggsave(filename=tolower(glue("figs/10_gbm_asci_csci_{hydroDat}_regions_ri_sized_points_w_lines_ranked.png")), width = 10, height = 7, units = "in", dpi = 300)
# 
# # now plot w CA and regions
# ri_table %>% 
#   filter(#model!="all_ca",
#          method=="mse") %>% 
#   left_join(., forder_asci, by="Flow.Metric.Name") %>% 
#   arrange(id) %>% #View() 
#   ggplot() +
#   facet_grid(cols = vars(model), labeller = labeller(model=c("all_ca"="All CA", "cent_coast"="C. Coast", "sierras"="Sierra Nevada", "north_coast"="N. Coast", "so_cal"="S. California"))) +
#   geom_linerange(aes(x=reorder(Flow.Metric.Name, desc(id)), ymax=RI, ymin=0, color=flow_component, group=model), 
#                  lwd=.5, show.legend = F, alpha=0.7, lty=1)+
#   geom_point(aes(x=reorder(Flow.Metric.Name, desc(id)), y=RI,
#                  fill=flow_component, size=RI, group=model, shape=index),
#              color="black", alpha=0.8) +
#   scale_fill_manual("Flow Component", values=flowcomponent_colors) +
#   scale_color_manual("Flow Component", values=flowcomponent_colors, guide=FALSE) +
#   scale_shape_manual("Index", values=c("ASCI"=23, "CSCI"=21))+
#   scale_size_area("", guide=FALSE) +
#   coord_flip() +
#   ylim(c(0,27))+
#   labs(title = "ASCI & CSCI BRT Models",
#        x="", y="Relative Influence (%)") +
#   theme_minimal(base_family = "Roboto Condensed") +
#   theme(legend.position = "bottom", 
#         legend.box="vertical",
#         legend.spacing.y = unit(-0.1,"cm"),
#         legend.justification = "center",
#         legend.margin = margin(6,0,6, 0),
#         legend.box.spacing = unit(-.01,"cm"),
#         legend.background = element_rect(color="white")) +
#   guides(fill = guide_legend(override.aes = 
#                                list(size = 4, pch=21, fill=flowcomponent_colors, 
#                                     color="gray40"), nrow = 1, order = 1, direction="horizontal"),
#          shape = guide_legend(override.aes = list(size = 4,                                                  direction="horizontal"), order = 2),
#          size= "none")
# 
# # WITH CA
# ggsave(filename=tolower(glue("figs/10_gbm_asci_csci_{hydroDat}_ca_and_regions_ri_sized_points_w_lines_ranked.png")), width = 10, height = 7, units = "in", dpi = 300)
# 

# Other Plots -------------------------------------------------------------



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
