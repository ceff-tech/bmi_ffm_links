# Identify top RI Flow Metrics
# summarize data from all GBMs

# Libraries ---------------------------------------------------------------

library(gt)
library(glue)
suppressPackageStartupMessages(library(tidyverse))
library(sf)
sf_use_s2(FALSE)

library(viridis) # colors
library(cowplot)
library(patchwork)

# Load Data ---------------------------------------------------------------

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


# CSCI: ALL CA Seasonality ----------------------

bioVar <- "csci"
(modname <- glue("{bioVar}_all_ca_seasonality")) # model name 

# make pathnames
(mod_savename <- tolower(glue("08_gbm_{modname}")))

# get the gbm model:
(top_ri <- list.files(path="models/", pattern = glue("^{mod_savename}_RI_combined.*\\.rds$")))
#top_ris <- read_rds(file=glue("models/{top_ri}"))
top_ris <- read_rds(file=glue("models/{top_ri[1]}"))

# make sep and combine
ri_csci_all_ca <- top_ris

# ASCI: ALL CA Seasonality ----------------------

# change biovar
bioVar <- "asci"
(modname <- glue("{bioVar}_all_ca_seasonality")) # model name 

# make pathnames
(mod_savename <- tolower(glue("08_gbm_{modname}")))

# get the gbm model:
(top_ri <- list.files(path="models/", pattern = glue("^{mod_savename}_RI_combined.*\\.rds$")))
#top_ris <- read_rds(file=glue("models/{top_ri}"))
top_ris <- read_rds(file=glue("models/{top_ri[1]}"))

# make sep and combine
ri_asci_all_ca <- top_ris

# CSCI: Mixed Seasonality ----------------------

# change biovar
bioVar <- "csci"
(modname <- glue("{bioVar}_mixed_seasonality")) # model name 

# make pathnames
(mod_savename <- tolower(glue("08_gbm_{modname}")))

# get the gbm model:
(top_ri <- list.files(path="models/", pattern = glue("^{mod_savename}_RI_combined.*\\.rds$")))
top_ris <- read_rds(file=glue("models/{top_ri}"))

# make sep and combine
ri_csci_mixed <- top_ris


# ASCI: Mixed Seasonality ----------------------

# change biovar
bioVar <- "asci"
(modname <- glue("{bioVar}_mixed_seasonality")) # model name 

# make pathnames
(mod_savename <- tolower(glue("08_gbm_{modname}")))

# get the gbm model:
(top_ri <- list.files(path="models/", pattern = glue("^{mod_savename}_RI_combined.*\\.rds$")))
top_ris <- read_rds(file=glue("models/{top_ri}"))

# make sep and combine
ri_asci_mixed <- top_ris

# CSCI: RAIN Seasonality ----------------------

# change biovar
bioVar <- "csci"
(modname <- glue("{bioVar}_rain_seasonality")) # model name 

# make pathnames
(mod_savename <- tolower(glue("08_gbm_{modname}")))

# get the gbm model:
(top_ri <- list.files(path="models/", pattern = glue("^{mod_savename}_RI_combined.*\\.rds$")))
top_ris <- read_rds(file=glue("models/{top_ri}"))

# make sep and combine
ri_csci_rain <- top_ris


# ASCI: RAIN Seasonality ----------------------

# change biovar
bioVar <- "asci"
(modname <- glue("{bioVar}_rain_seasonality")) # model name 

# make pathnames
(mod_savename <- tolower(glue("08_gbm_{modname}")))

# get the gbm model:
(top_ri <- list.files(path="models/", pattern = glue("^{mod_savename}_RI_combined.*\\.rds$")))
top_ris <- read_rds(file=glue("models/{top_ri}"))

# make sep and combine
ri_asci_rain <- top_ris

# CSCI: SNOW Seasonality ----------------------

# change biovar
bioVar <- "csci"
(modname <- glue("{bioVar}_snow_seasonality")) # model name 

# make pathnames
(mod_savename <- tolower(glue("08_gbm_{modname}")))

# get the gbm model:
(top_ri <- list.files(path="models/", pattern = glue("^{mod_savename}_RI_combined.*\\.rds$")))
top_ris <- read_rds(file=glue("models/{top_ri}"))

# make sep and combine
ri_csci_snow <- top_ris

# ASCI: SNOW Seasonality ----------------------

# change biovar
bioVar <- "asci"
(modname <- glue("{bioVar}_snow_seasonality")) # model name 

# make pathnames
(mod_savename <- tolower(glue("08_gbm_{modname}")))

# get the gbm model:
(top_ri <- list.files(path="models/", pattern = glue("^{mod_savename}_RI_combined.*\\.rds$")))
top_ris <- read_rds(file=glue("models/{top_ri}"))

# make sep and combine
ri_asci_snow <- top_ris


# BIND ALL TOGETHER -------------------------------------------------------

## bind
ri_all_regions <- bind_rows(ri_csci_all_ca, ri_asci_all_ca)

# ri_all_regions <- bind_rows(ri_csci_all_ca, ri_asci_all_ca,
#                             ri_csci_rain, ri_asci_rain,
#                             ri_csci_mixed, ri_asci_mixed,
#                             ri_csci_snow, ri_asci_snow)

# Make a Table of RI's ----------------------------------------------------

library(readxl)
ff_defs <- readxl::read_xlsx("docs/Functional_Flow_Metrics_List_and_Definitions_final.xlsx", range = "A1:F25", .name_repair = "universal", trim_ws = TRUE) 

# join with the full RI table
ri_table <- left_join(ri_all_regions, ff_defs, by=c("var"="Flow.Metric.Code"))

# drop unused factors in flow component:
ri_table <- ri_table %>% 
  mutate(flow_component=forcats::fct_drop(flow_component),
         var = as.factor(var),
         var = fct_reorder2(var, flow_component, var),
         model=as.factor(model),
         Flow.Metric.Name = case_when(
           #flow_component == "Stream Class" ~ "Stream Class",
           var == "Power.avg" ~ "Wavelet Interannual",
           var == "MP_metric" ~ "Colwell's M/P",
           TRUE ~ Flow.Metric.Name),
         Flow.Metric.Name = as.factor(Flow.Metric.Name),
         Flow.Metric.Name = forcats::fct_reorder2(Flow.Metric.Name, model, RI)) 

levels(ri_table$var)
levels(ri_table$flow_component)
levels(ri_table$Flow.Metric.Name)
summary(ri_table)

# save out:
#write_rds(ri_table, file = glue("models/09_combined_ri_all_ca_seasonality.rds"))
#save(ri_table, file = tolower(glue("models/09_combined_ri_all_ca_seasonality.rda")))

# generate order by CA wide RI for flow metrics:
forder <- ri_table %>% 
  filter(model=="all_ca_seasonality", 
         method=="mse") %>% 
  mutate(model=as.factor(modname),
         Flow.Metric.Name = as.factor(Flow.Metric.Name),
         Flow.Metric.Name = forcats::fct_reorder2(Flow.Metric.Name, model, RI)) %>% 
  group_by(model) %>% 
  arrange(desc(model, RI)) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% arrange(id) %>% 
  select(Flow.Metric.Name, Ymetric, id) # get the just the names for ordering things


# Plot & Summarize All RI Combined ----------------------------------------

# most common hydrometric by model
# ri_table %>% group_by(model, var) %>% 
#   summarize(meanRI = mean(RI),
#             sumRI = sum(RI)) %>% 
#   top_n(5) %>% 
#   arrange(model, desc(meanRI))
# 
# # so best hydrometric across model?
# ri_table %>% group_by(var, model) %>% 
#   summarize(meanRI = mean(RI),
#             medianRI = median(RI),
#             maxRI = max(RI),
#             SD = sd(RI)) %>% 
#   group_by(model) %>% 
#   arrange(model, desc(meanRI)) %>% top_n(5)
# 
# ri_table %>% group_by(var, model) %>% 
#   summarize(meanRI = mean(RI),
#             medianRI = median(RI),
#             maxRI = max(RI),
#             SD = sd(RI)) %>% 
#   group_by(model) %>% 
#   arrange(model, desc(meanRI)) %>% top_n(5)


# Summary Table ALL CA -----------------------------------------------------------

# library(glue)
# 
# model_name <- "All CA"
# ## ALL CA Table
# # Create a gt table based on preprocessed table
# ri_table %>%
#   dplyr::filter(method=="mse", model=="all_ca_seasonality") %>%
#   dplyr::select(-c(Flow.Component, flowdat,  method, Flow.Characteristic)) %>%
#   dplyr::select(Ymetric, flow_component, var, Flow.Metric.Name:Flow.Metric.Description, RI) %>%
#   #arrange(Flow.Component, var) %>% #View() 
#   arrange(RI, flow_component) %>% #View() 
#   select(-var) %>% 
#   gt() %>%
#   tab_header(
#     title = "Relative Influence of Functional Flow Metrics",
#     subtitle = glue::glue("{model_name}")
#   ) %>%
#   fmt_number(
#     columns = c(RI), decimals = 1, 
#     drop_trailing_zeros = T
#   )


# Lollipop Plot: ALL CA with seasonality -----------------------------------------------

# darker for peak flow
flowcomponent_colors <- c("Fall pulse flow" = "#F0E442", "Wet-season baseflow" = "#56B4E9",
                          "Peak flow" = "#404788FF", "Spring recession flow" = "#009E73", 
                          "Dry-season baseflow" = "#D55E00", 
                          #"Stream Class" = "skyblue", 
                          "Seasonality" = "gray")

# set up values
plotname <- "All CA"
modname <- "all_ca_seasonality"
(plot_savename <- tolower(glue("09_gbm_{modname}_all")))

# plot
(ri_table %>% 
    filter(model==modname, 
           method=="mse") %>% 
    ggplot() +
    #geom_hline(yintercept = 5, color="gray40", lwd=0.8, lty=2, alpha=0.5)+
    geom_linerange(aes(x=reorder(Flow.Metric.Name, RI),
                       ymax=RI, ymin=0, group=model, color=flow_component), 
                   lwd=.5, show.legend = F, alpha=0.7, lty=1)+
    geom_point(aes(x=Flow.Metric.Name, y=RI, group=model, 
                   shape=Ymetric, 
                   color=flow_component,
                   fill=flow_component,
                   size=RI)) +
    geom_point(aes(x=Flow.Metric.Name, y=RI, group=model, 
                   shape=Ymetric, size=RI), 
               color="black", show.legend = FALSE, alpha=0.8) +
    scale_shape_manual("Index", values=c("asci"=23, "csci"=21),
                       labels=c("ASCI","CSCI"))+
    scale_color_manual("Flow Component", values=flowcomponent_colors) +
    scale_fill_manual("Flow Component", values=flowcomponent_colors, guide="none") +
    scale_size_area("", guide=FALSE) +
    #scale_shape_manual("Method", values=c("mse"=16, "permtest"=17))+
    coord_flip() +
    ylim(c(0,25))+
    labs(title = glue::glue('{plotname}'),
         x="", y="Relative Influence (%)") +
    # fix legend
    guides(color = guide_legend(override.aes = list(size = 4, pch=21, 
                                                    fill=flowcomponent_colors, color="gray40"),
                                order = 1),
           shape = guide_legend(override.aes = list(size = 4),
                                order = 2),
           size= "none") +
    theme_minimal_grid(font_family = "Roboto Condensed") +
    theme(legend.position = c(0.7, 0.25),
          plot.background = element_rect(fill="white"),
          panel.border = element_rect(color = "white"),
          legend.box.background = element_rect(fill="white", color = "white")) -> gg1)


# save out
ggsave(filename=tolower(glue("models/{plot_savename}_all_ri_sized_points_w_lines_ranked.png")), width = 10, height = 7, units = "in", dpi = 300)


# Lollipop Plot: MIXED with seasonality -----------------------------------------------

# set up values
plotname <- "All CA: Mixed"
modname <- "mixed_seasonality"
(plot_savename <- tolower(glue("09_gbm_{modname}")))

# plot
(ri_table %>% 
    filter(model==modname, 
           method=="mse") %>% 
    ggplot() +
    #geom_hline(yintercept = 5, color="gray40", lwd=0.8, lty=2, alpha=0.5)+
    geom_linerange(aes(x=reorder(Flow.Metric.Name, RI),
                       ymax=RI, ymin=0, group=model, color=flow_component), 
                   lwd=.5, show.legend = F, alpha=0.7, lty=1)+
    geom_point(aes(x=Flow.Metric.Name, y=RI, group=model, 
                   shape=Ymetric, 
                   color=flow_component,
                   fill=flow_component,
                   size=RI)) +
    geom_point(aes(x=Flow.Metric.Name, y=RI, group=model, 
                   shape=Ymetric, size=RI), 
               color="black", show.legend = FALSE, alpha=0.8) +
    scale_shape_manual("Index", values=c("asci"=23, "csci"=21), labels=c("ASCI","CSCI"))+
    scale_color_manual("Flow Component", values=flowcomponent_colors) +
    scale_fill_manual("Flow Component", values=flowcomponent_colors, guide="none") +
    scale_size_area("", guide=FALSE) +
    coord_flip() +
    ylim(c(0,42))+
    labs(title = glue::glue('{plotname}'),
         x="", y="Relative Influence (%)") +
    # fix legend
    guides(color = guide_legend(override.aes = list(size = 4, pch=21, 
                                                    fill=flowcomponent_colors, color="gray40"),
                                order = 1),
           shape = guide_legend(override.aes = list(size = 4),
                                order = 2),
           size= "none") +
    theme_minimal_grid(font_family = "Roboto Condensed") +
    theme(legend.position = c(0.6, 0.25),
          plot.background = element_rect(fill="white"),
          panel.border = element_rect(color = "white"),
          legend.box.background = element_rect(fill="white", color = "white")) -> gg2)

# save out
ggsave(filename=tolower(glue("models/{plot_savename}_all_ri_sized_points_w_lines_ranked.png")), width = 10, height = 7, units = "in", dpi = 300)


# Lollipop Plot: RAIN with seasonality -----------------------------------------------

# set up values
plotname <- "All CA: Rain"
modname <- "rain_seasonality"
(plot_savename <- tolower(glue("09_gbm_{modname}")))

# plot
(ri_table %>% 
    filter(model==modname, 
           method=="mse") %>% 
    ggplot() +
    #geom_hline(yintercept = 5, color="gray40", lwd=0.8, lty=2, alpha=0.5)+
    geom_linerange(aes(x=reorder(Flow.Metric.Name, RI),
                       ymax=RI, ymin=0, group=model, color=flow_component), 
                   lwd=.5, show.legend = F, alpha=0.7, lty=1)+
    geom_point(aes(x=Flow.Metric.Name, y=RI, group=model, 
                   shape=Ymetric, 
                   color=flow_component,
                   fill=flow_component,
                   size=RI)) +
    geom_point(aes(x=Flow.Metric.Name, y=RI, group=model, 
                   shape=Ymetric, size=RI), 
               color="black", show.legend = FALSE, alpha=0.8) +
    scale_shape_manual("Index", values=c("asci"=23, "csci"=21), labels=c("ASCI","CSCI"))+
    scale_color_manual("Flow Component", values=flowcomponent_colors) +
    scale_fill_manual("Flow Component", values=flowcomponent_colors, guide="none") +
    scale_size_area("", guide=FALSE) +
    coord_flip() +
    ylim(c(0,25))+
    labs(title = glue::glue('{plotname}'),
         x="", y="Relative Influence (%)") +
    # fix legend
    guides(color = guide_legend(override.aes = list(size = 4, pch=21, 
                                                    fill=flowcomponent_colors, color="gray40"),
                                order = 1),
           shape = guide_legend(override.aes = list(size = 4),
                                order = 2),
           size= "none") +
    theme_minimal_grid(font_family = "Roboto Condensed") +
    theme(legend.margin=margin(),
          legend.position = c(0.6, 0.25),
          plot.background = element_rect(fill="white"),
          panel.border = element_rect(color = "white"),
          legend.box.background = element_rect(fill="white", color = "white")) -> gg3)

# save out
ggsave(filename=tolower(glue("models/{plot_savename}_all_ri_sized_points_w_lines_ranked.png")), width = 10, height = 7, units = "in", dpi = 300)

# Lollipop Plot: SNOW with seasonality -----------------------------------------------

# set up values
plotname <- "All CA: Snowmelt"
modname <- "snow_seasonality"
(plot_savename <- tolower(glue("09_gbm_{modname}")))

# plot
(ri_table %>% 
    filter(model==modname, 
           method=="mse") %>% 
    ggplot() +
    #geom_hline(yintercept = 5, color="gray40", lwd=0.8, lty=2, alpha=0.5)+
    geom_linerange(aes(x=reorder(Flow.Metric.Name, RI),
                       ymax=RI, ymin=0, group=model, color=flow_component), 
                   lwd=.5, show.legend = F, alpha=0.7, lty=1)+
    geom_point(aes(x=Flow.Metric.Name, y=RI, group=model, 
                   shape=Ymetric, 
                   color=flow_component,
                   fill=flow_component,
                   size=RI)) +
    geom_point(aes(x=Flow.Metric.Name, y=RI, group=model, 
                   shape=Ymetric, size=RI), 
               color="black", show.legend = FALSE, alpha=0.8) +
    scale_shape_manual("Index", values=c("asci"=23, "csci"=21), labels=c("ASCI","CSCI"))+
    scale_color_manual("Flow Component", values=flowcomponent_colors) +
    scale_fill_manual("Flow Component", values=flowcomponent_colors, guide="none") +
    scale_size_area("", guide=FALSE) +
    coord_flip() +
    ylim(c(0,25))+
    labs(title = glue::glue('{plotname}'),
         x="", y="Relative Influence (%)") +
    # fix legend
    guides(color = guide_legend(override.aes = list(size = 4, pch=21, 
                                                    fill=flowcomponent_colors, color="gray40"),
                                order = 1),
           shape = guide_legend(override.aes = list(size = 4),
                                order = 2),
           size= "none") +
    theme_minimal_grid(font_family = "Roboto Condensed") +
    theme(#legend.position = c(0.6, 0.25),
          legend.position = c(0.45, 0.2), # for combined plot
          #legend.text = element_text(size=10),
          plot.background = element_rect(fill="white"),
          panel.border = element_rect(color = "white"),
          legend.box.background = element_rect(fill="white", color = "white")) -> gg4)

# save out
ggsave(filename=tolower(glue("models/{plot_savename}_all_ri_sized_points_w_lines_ranked.png")), width = 10, height = 7, units = "in", dpi = 300)


# COMBINE REGIONS ---------------------------------------------------------

# library(cowplot)
# my_legend <- get_legend(gg2)
# library(ggpubr)
# as_ggplot(my_legend)

library(patchwork)
# remove legend from gg2 and gg3
gg2_noleg <- gg2 + theme(legend.position = "none")
gg3_noleg <- gg3 + theme(legend.position = "none")
#gg4_noleg <- gg4 + theme(legend.position="none")

gg2_noleg + gg3_noleg + gg4 +
  #plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')

ggsave(filename = glue("models/09_gbm_combined_snow_rain_mixed_seasonality_all_ri_sized_points_w_lines_horiz.png"),
       width = 16, height = 8, scale = 1, units="in", dpi=300)


# add trend plot of seasonality

# Seasonality Plot --------------------------------------------------------

bio_ffm %>% 
  ggplot() + 
  geom_point(aes(y=MP_metric, x=biovalue, fill=bioindicator), pch=21, size=2.7, alpha=0.9, show.legend = TRUE) +
  stat_smooth(aes(y=MP_metric, x=biovalue, color=bioindicator), 
              method = "glm", show.legend=FALSE) +
  theme_classic(base_family = "Roboto Condensed") +
  scale_color_viridis_d(option = "B", "Index") +
  scale_fill_viridis_d(option = "A", "Index") +
  labs(y="Seasonality \n(Colwell's M/P)", x="Bio Index",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)") + 
  facet_wrap(.~class3_name)

ggsave(filename = "figs/colwells_vs_csci_asci_all_gages_trend.png", width = 11, height = 8, dpi = 300, units = "in")
