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

ri_table <- read_rds("models/09_combined_ri_all_ca_seasonality.rds")

# add seasonality description
ri_table <- ri_table %>% 
  mutate(Flow.Metric.Description = case_when(
    var == "MP_metric" ~ "Seasonality calculated by Colwell's measure of contingency standardized by within-season predictability",
    TRUE ~ Flow.Metric.Description))

# generate order by CA wide RI for flow metrics:
forder <- ri_table %>% 
  filter(model=="all_ca_seasonality", 
         method=="mse") %>% 
  mutate(Flow.Metric.Name = as.factor(Flow.Metric.Name),
         Flow.Metric.Name = forcats::fct_reorder2(Flow.Metric.Name, model, RI)) %>% 
  group_by(model) %>% 
  arrange(desc(model, RI)) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% arrange(id) %>% 
  select(Flow.Metric.Name, Ymetric, id) # get the just the names for ordering things

# Summary Table ALL CA -----------------------------------------------------------

library(glue)

# rearrange data so csci and asci are separate columns
ri_summ_table <- ri_table %>% 
  filter(method=="mse") %>% 
  select(var:Ymetric, model, flow_component, Flow.Characteristic:Flow.Metric.Description) %>% 
  pivot_wider(names_from=Ymetric, values_from = RI)

# now make table 
model_name <- "All CA"

## ALL CA Table
# Create a gt table based on preprocessed table
ri_summ_table %>% 
  filter(model=="all_ca_seasonality") %>% 
  arrange(flow_component, Flow.Metric.Name) %>% #View()
  select(-c(var, model, Flow.Characteristic)) %>%
  gt() %>%
  tab_header(
    title = "Relative Influence of Functional Flow Metrics",
    subtitle = glue::glue("{model_name}")
  ) %>%
  gt::cols_label(flow_component="Flow Component", 
                 Flow.Metric.Name = "Metric Name",
                 Flow.Metric.Description = "Description",
                 csci = "CSCI", asci="ASCI") %>% 
  fmt_number(
    columns = c(csci, asci), decimals = 1,
    drop_trailing_zeros = T
  )

# Stream Class Table ---------------------------------------------------------

model_name <- "Stream Classes"

ri_strmclass_table <- ri_table %>% 
  filter(method=="mse",
         model!="all_ca_seasonality") %>% 
  select(var:Ymetric, model, flow_component, Flow.Metric.Name) %>% 
  pivot_wider(names_from=c(Ymetric, model), values_from = RI)

# how many samples per stream class?
bio_ffm %>% 
  select(gageid, StationCode, class3_name, bioindicator) %>% 
  distinct(.keep_all = TRUE) %>% 
  #group_by(class3_name, bioindicator) %>% 
  group_by(bioindicator) %>% #View()
  tally() %>% 
  gt() %>%
  tab_header(
    title = "Samples by Stream Class",
    subtitle = glue::glue("{model_name}")
  ) %>% 
  gt::cols_label(bioindicator = "Bioindicator")
  


# Create a gt table based on preprocessed table
ri_strmclass_table %>% 
  arrange(flow_component, Flow.Metric.Name) %>% #View()
  select(-c(var, flow_component)) %>%
  rename_all(~gsub("_seasonality", "", .)) %>% 
  gt() %>%
  tab_header(
    title = "Relative Influence of Functional Flow Metrics",
    subtitle = glue::glue("{model_name}")
  ) %>%
  gt::cols_label(Flow.Metric.Name = "Metric Name",
                 csci_rain = "CSCI",
                 asci_rain = "ASCI",
                 csci_mixed = "CSCI",
                 asci_mixed = "ASCI",
                 csci_snow = "CSCI",
                 asci_snow = "ASCI") %>% 
  tab_spanner(
    label = "Rain",
    columns = c(csci_rain, asci_rain)) %>% 
  tab_spanner(
    label = "Mixed",
    columns = c(csci_mixed, asci_mixed)) %>% 
  tab_spanner(
    label = "Snowmelt",
    columns = c(csci_snow, asci_snow)) %>% 
  fmt_number(
    columns = c(csci_rain:asci_snow), decimals = 1,
    drop_trailing_zeros = T
  ) #%>% 
  #as_rtf()



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
plotname <- "Mixed"
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
    #ylim(c(0,40)) +
    scale_y_continuous(breaks = c(seq(0,40,5)))+
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
plotname <- "Rain"
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
plotname <- "Snowmelt"
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


# TREND PLOTS -----------------------------------------

library(ggpmisc) # for R2

# Reduce Replicates -------------------------------------------------------

# calc records by StationCode (how many mult years)
# bio_ffm %>%
#   filter(bioindicator=="CSCI") %>% 
#   distinct(StationCode, gageid, SampleID, .keep_all=TRUE) %>% 
#   group_by(StationCode, gageid) %>% 
#   tally() %>% 
#   arrange(desc(n)) %>% 
#   filter(n>1) -> csci_sites_to_combine # CSCI n= 147 sites w dups
# 
# csci_med <- bio_ffm %>% 
#   filter(bioindicator=="CSCI") %>%
#   select(-c(p50_obs:metric, result_type, HUC_12:CLASS_NAME)) %>% 
#   distinct(.keep_all=TRUE) %>% 
#   inner_join(., csci_sites_to_combine) %>% 
#   group_by(StationCode, gageid) %>% 
#   summarize(csci_med = median(csci))
# 
# # rejoin
# csci_ffm <- bio_ffm %>% 
#   filter(bioindicator=="CSCI") %>%
#   left_join(., csci_med) %>% 
#   relocate(csci_med, .after="csci") %>% 
#   #clean up and reset bioindicator value
#   mutate(csci2 = case_when(
#     is.na(csci_med) ~ csci,
#     TRUE ~ csci_med), .after="csci") %>% 
#   mutate(biovalue = csci2) %>% 
#   select(-csci2)
# 
# # rebind and save dataset
# asci_ffm <- bio_ffm %>% 
#   filter(bioindicator == "ASCI")
# 
# bio_ffm_rev <- bind_rows(asci_ffm, csci_ffm) %>% 
#   relocate(csci_med, .after="csci") %>% 
#   distinct(.keep_all = TRUE)


## Trendplot Seasonality -----------------------------------------------------

# reduce the replicates
bio_ffm %>% 
  select(biovalue, MP_metric, bioindicator, StationCode, gageid, class3_name) %>% 
  group_by(StationCode, bioindicator, class3_name) %>% 
  summarize(biovalue) %>% 
  distinct(StationCode, .keep_all = TRUE) %>% 
  ungroup() %>%
  # ungroup() %>% group_by(bioindicator, class3_name) %>% tally() %>% View()
  left_join(., bio_ffm %>% select(StationCode, gageid, MP_metric, class3_name, bioindicator) %>% distinct(.keep_all=TRUE)) %>% # n=720
  ggplot() + 
  geom_point(aes(y=MP_metric, x=biovalue, fill=bioindicator, shape=bioindicator), size=2.7, alpha=0.9, show.legend = TRUE) +
  scale_shape_manual("Index", values=c(23,21)) +
  stat_smooth(aes(y=MP_metric, x=biovalue, color=bioindicator), 
              method = "glm", formula=y ~ poly(x, 2), se=FALSE, lwd=1.6, show.legend=FALSE) +
  ggpmisc::stat_poly_eq(aes(y=MP_metric, x=biovalue, color=bioindicator), 
                        formula = y ~ poly(x, 2), na.rm = TRUE)+
  theme_classic(base_family = "Roboto Condensed") +
  ggthemes::scale_color_colorblind("Index") +
  ggthemes::scale_fill_colorblind("Index") +
  labs(y="Seasonality \n(Colwell's M/P)", x="Bio Index") #+
       #caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)") + 
  #facet_wrap(.~class3_name)

# save

#ggsave(filename = "figs/colwells_vs_csci_asci_all_ca_gages_trend_gam.png", width = 11, height = 8, dpi = 300, units = "in")

ggsave(filename = "figs/colwells_vs_csci_asci_all_gages_trend_gam.png", width = 11, height = 8, dpi = 300, units = "in")
ggsave(filename = "figs/colwells_vs_csci_asci_all_gages_trend_glm_poly.png", width = 11, height = 8, dpi = 300, units = "in")

# split into negative and positive delta hydrology -----------------------

# bio_ffm %>% 
#   filter(metric == "FA_Tim") %>% 
#   select(biovalue, metric, delta_p50, bioindicator, StationCode, gageid, class3_name) %>% 
#   group_by(StationCode, bioindicator, class3_name) %>% 
#   summarize(biovalue) %>% 
#   distinct(StationCode, .keep_all = TRUE) %>% 
#   ungroup() %>%
#   # ungroup() %>% group_by(bioindicator, class3_name) %>% tally() %>% View()
#   left_join(., 
#             bio_ffm %>% filter(metric=="FA_Tim") %>% 
#               select(StationCode, gageid, metric, delta_p50, class3_name, bioindicator) %>%
#               distinct(.keep_all=TRUE)) %>% #View()# n=718
#   mutate(delta_dir = if_else(delta_p50 >=0, "Delta Positive", "Delta Negative"),
#          delta_dir = factor(delta_dir, levels = c("Delta Positive", "Delta Negative"))) -> bio_ffm_df
# View(bio_ffm_df)


## Fall Pulse Timing -------------------------------------------------------


# bio_ffm_df %>% 
#   ggplot() + 
#   geom_point(aes(y=delta_p50, x=biovalue, shape=bioindicator, fill=bioindicator), size=2.7, alpha=0.9, show.legend = TRUE) +
#   stat_smooth(aes(y=delta_p50, x=biovalue, color=bioindicator), 
#               # gam
#               #method = "gam", formula = y ~ s(x, k = 4), show.legend=FALSE, se = FALSE) +
#               # glm: poly 2
#               method = "glm", formula = y ~ poly(x, 2), show.legend=FALSE, lwd=1.3, se = FALSE) +
#   #stat_poly_eq(aes(y=delta_p50, x=biovalue, color=bioindicator), formula = y ~ poly(x, 2)) +
#   geom_hline(yintercept = 0, color="gray30", lwd=.5, lty=2, alpha=0.3) +
#   theme_classic(base_family = "Roboto Condensed") +
#   scale_shape_manual("Index", values=c(23,21)) +
#   ggthemes::scale_color_colorblind("Index") +
#   ggthemes::scale_fill_colorblind("Index") +
#   labs(y="Fall Pulse Timing", x="Bio Index") + 
#   facet_grid(delta_dir~class3_name, scales = "free_y")
# 
# #ggsave(filename = "figs/fall_pulse_timing_vs_csci_asci_all_gages_trend_gam.png", width = 11, height = 8, dpi = 300, units = "in")
# ggsave(filename = "figs/fall_pulse_timing_vs_csci_asci_all_gages_trend_glm_poly.png", width = 11, height = 8, dpi = 300, units = "in")


## Dry Season Baseflow -------------------------------------------------------

# bio_ffm %>% 
#   filter(metric == "DS_Mag_50") %>% 
#   select(biovalue, metric, delta_p50, bioindicator, StationCode, gageid, class3_name) %>% 
#   group_by(StationCode, bioindicator, class3_name) %>% 
#   summarize(biovalue) %>% 
#   distinct(StationCode, .keep_all = TRUE) %>% 
#   ungroup() %>%
#   # ungroup() %>% group_by(bioindicator, class3_name) %>% tally() %>% View()
#   left_join(., 
#             bio_ffm %>% filter(metric=="DS_Mag_50") %>% 
#               select(StationCode, gageid, metric, delta_p50, class3_name, bioindicator) %>%
#               distinct(.keep_all=TRUE)) %>% #View()# n=718
#   mutate(delta_dir = if_else(delta_p50 >=0, "Delta Positive", "Delta Negative"),
#          delta_dir = factor(delta_dir, levels = c("Delta Positive", "Delta Negative"))) %>% 
#   filter(delta_p50 < 3) %>% 
#   ggplot() + 
#   geom_point(aes(y=delta_p50, x=biovalue, shape=bioindicator, fill=bioindicator), 
#              size=2.7, alpha=0.9, show.legend = TRUE) +
#   stat_smooth(aes(y=delta_p50, x=biovalue, color=bioindicator), 
#               # gam
#               #method = "gam", formula = y ~ s(x, k = 4), show.legend=FALSE, se = FALSE) +
#               # glm: poly 2
#               method = "glm", formula = y ~ poly(x, 2), show.legend=FALSE, lwd=1.3, se = FALSE) +
#   stat_poly_eq(aes(y=delta_p50, x=biovalue, color=bioindicator), formula = y ~ poly(x, 2)) +
#   geom_hline(yintercept = 0, color="gray30", lwd=.5, lty=2, alpha=0.3) +
#   theme_classic(base_family = "Roboto Condensed") +
#   scale_shape_manual("Index", values=c(23,21)) +
#   ggthemes::scale_color_colorblind("Index") +
#   ggthemes::scale_fill_colorblind("Index") +
#   labs(y="Dry Season Baseflow", x="Bio Index") + 
#   facet_grid(delta_dir~class3_name, scales = "free_y")
# 
# #ggsave(filename = "figs/fall_pulse_timing_vs_csci_asci_all_gages_trend_gam.png", width = 11, height = 8, dpi = 300, units = "in")
# ggsave(filename = "figs/dry_season_baseflow_vs_csci_asci_all_gages_trend_glm_poly.png", width = 11, height = 8, dpi = 300, units = "in")



# Try Binning By Thresholds and then Boxplot ------------------------------

## try binning ASCI/CSCI by thresholds (case when assignment based on value range)
## then boxplot of variables by stream class (y is FFM, x threshold type (alt, likely alt, etc))

## biological stream condition thresholds (Mazor et al. 2016, Theroux et al. 2020 - See Table 8)
asci_breaks <- c(0, 0.75, 0.86, 0.94)
csci_breaks <- c(0, 0.63, 0.79, 0.92)
bio_labs <- c("Very likely altered", "Likely altered", "Possibly altered","Likely intact")

bio_ffm_thresh <- bio_ffm %>% 
  mutate(biothresh = case_when(
    bioindicator == "ASCI" & biovalue < 0.75 ~ "Very likely altered",
    bioindicator == "CSCI" & biovalue < 0.63 ~ "Very likely altered",
    bioindicator == "ASCI" & biovalue >= 0.75 & biovalue <= 0.86 ~ "Likely altered",
    bioindicator == "CSCI" & biovalue >= 0.63 & biovalue <= 0.79  ~ "Likely altered",
    bioindicator == "ASCI" & biovalue > 0.86 & biovalue <= 0.94 ~ "Possibly altered",
    bioindicator == "CSCI" & biovalue > 0.79 & biovalue <= 0.92  ~ "Possibly altered",
    bioindicator == "ASCI" & biovalue > 0.94 ~ "Likely intact",
    bioindicator == "CSCI" & biovalue > 0.92  ~ "Likely intact"
  ),
  biothresh = factor(biothresh, levels=c("Very likely altered", "Likely altered", "Possibly altered", "Likely intact")))

## Boxplot: All CA ----------------------------------------------------------------

bio_ffm_thresh %>% 
  filter(metric %in% c("DS_Mag_50", "FA_Tim", "FA_Mag")) %>% 
  filter(delta_p50 < 3.5) %>% 
  # join names
  left_join(., ri_strmclass_table[,c("var", "Flow.Metric.Name")], by=c("metric"="var")) %>% 
  ggplot() + 
  geom_boxplot(aes(x=biothresh, y=delta_p50, group=biothresh, fill=bioindicator), 
               alpha=0.7, show.legend = FALSE, notch = TRUE) +
  geom_hline(yintercept = 0, color="maroon", lwd=1, lty=1, alpha=0.8) +
  # theme_classic(base_family = "Roboto Condensed") +
  cowplot::theme_half_open() +
  cowplot::background_grid(major="y") +
  theme(plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 70, vjust = .5)) +
  # scale_shape_manual("Index", values=c(23,21)) +
  ggthemes::scale_color_colorblind("Index") +
  ggthemes::scale_fill_colorblind("Index") +
  labs(y="Delta Hydrology", x="") + 
  facet_grid(bioindicator~Flow.Metric.Name, scales = "free_y")

ggsave(filename = "figs/top3_ffm_vs_csci_asci_all_ca_box_notched.png", width = 11, height = 8, dpi = 300, units = "in")


## Boxplot: Seasonality ----------------------------------------------------------------

bio_ffm_thresh %>% 
  filter(delta_p50 < 3.5) %>%
  ggplot() + 
  geom_boxplot(aes(x=biothresh, y=MP_metric, group=biothresh, fill=bioindicator), 
               alpha=0.7, show.legend = FALSE, notch = TRUE) +
  cowplot::theme_half_open() +
  cowplot::background_grid(major="y") +
  theme(plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 70, vjust = .5)) +
  # scale_shape_manual("Index", values=c(23,21)) +
  ggthemes::scale_color_colorblind("Index") +
  ggthemes::scale_fill_colorblind("Index") +
  labs(y="Colwell's Seasonality (M/P)", x="") + 
  #facet_grid(bioindicator~., scales = "free_y")
  facet_grid(bioindicator~class3_name, scales = "free_y") # by stream class

ggsave(filename = "figs/seasonality_vs_csci_asci_boxplots.png", width = 11, height = 8, dpi = 300, units = "in")
ggsave(filename = "figs/seasonality_vs_csci_asci_boxplots_by_streamclass.png", width = 11, height = 8, dpi = 300, units = "in")


## Boxplot: Dry Season Baseflow ----------------------------------------------------------------

bio_ffm_thresh %>% 
  filter(metric == "DS_Mag_50") %>% 
  filter(delta_p50 < 3.5) %>% 
  ggplot() + 
  geom_boxplot(aes(x=biothresh, y=delta_p50, group=biothresh, fill=bioindicator), 
             alpha=0.7, show.legend = FALSE) +
  geom_hline(yintercept = 0, color="maroon", lwd=0.7, lty=1, alpha=0.8) +
  # theme_classic(base_family = "Roboto Condensed") +
  cowplot::theme_half_open() +
  cowplot::background_grid(major="y") +
  theme(plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 70, vjust = .5)) +
  # scale_shape_manual("Index", values=c(23,21)) +
  ggthemes::scale_color_colorblind("Index") +
  ggthemes::scale_fill_colorblind("Index") +
  labs(y="Dry Season Baseflow", x="") + 
  facet_grid(bioindicator~class3_name, scales = "free_y")

#ggsave(filename = "figs/fall_pulse_timing_vs_csci_asci_all_gages_trend_gam.png", width = 11, height = 8, dpi = 300, units = "in")
ggsave(filename = "figs/dry_season_baseflow_vs_csci_asci_boxplots_w_zeroline.png", width = 11, height = 8, dpi = 300, units = "in")




## Boxplot: 7A: Fall Pulse Timing ----------------------------------------------------------------


bio_ffm_thresh %>% 
  filter(metric == "FA_Tim") %>% 
  #filter(delta_p50 < 3) %>% 
  ggplot() + 
  geom_boxplot(aes(x=biothresh, y=delta_p50, group=biothresh, fill=bioindicator), 
               alpha=0.7, show.legend = FALSE, notch=FALSE) +
  geom_hline(yintercept = 0, color="maroon", lwd=0.7, lty=1, alpha=0.8) +
  # theme_classic(base_family = "Roboto Condensed") +
  cowplot::theme_half_open() +
  cowplot::background_grid(major="y") +
  theme(plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 70, vjust = .5)) +
  # scale_shape_manual("Index", values=c(23,21)) +
  ggthemes::scale_color_colorblind("Index") +
  ggthemes::scale_fill_colorblind("Index") +
  labs(y="Fall Pulse Timing", x="") + 
  facet_grid(bioindicator~class3_name, scales = "free_y")

ggsave(filename = "figs/fall_pulse_timing_vs_csci_asci_boxplots_w_zeroline.png", width = 11, height = 8, dpi = 300, units = "in")

## Boxplot: Fall Pulse Magnitude ----------------------------------------------------------------


bio_ffm_thresh %>% 
  filter(metric == "FA_Mag") %>% 
  filter(delta_p50 < 3) %>% 
  ggplot() + 
  geom_boxplot(aes(x=biothresh, y=delta_p50, group=biothresh, fill=bioindicator), 
               alpha=0.7, show.legend = FALSE) +
  geom_hline(yintercept = 0, color="maroon", lwd=0.7, lty=1, alpha=0.8) +
  # theme_classic(base_family = "Roboto Condensed") +
  cowplot::theme_half_open() +
  cowplot::background_grid(major="y") +
  theme(plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 70, vjust = .5)) +
  # scale_shape_manual("Index", values=c(23,21)) +
  ggthemes::scale_color_colorblind("Index") +
  ggthemes::scale_fill_colorblind("Index") +
  labs(y="Fall Pulse Magnitude", x="") + 
  facet_grid(bioindicator~class3_name, scales = "free_y")

ggsave(filename = "figs/fall_pulse_magnitude_vs_csci_asci_boxplots_w_zeroline.png", width = 11, height = 8, dpi = 300, units = "in")


## Boxplot: Wet Season Timing ----------------------------------------------------------------


bio_ffm_thresh %>% 
  filter(metric == "Wet_Tim") %>% 
  #filter(delta_p50 < 3) %>% 
  ggplot() + 
  geom_boxplot(aes(x=biothresh, y=delta_p50, group=biothresh, fill=bioindicator), 
               alpha=0.7, show.legend = FALSE) +
  geom_hline(yintercept = 0, color="maroon", lwd=0.7, lty=1, alpha=0.8) +
  # theme_classic(base_family = "Roboto Condensed") +
  cowplot::theme_half_open() +
  cowplot::background_grid(major="y") +
  theme(plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 70, vjust = .5)) +
  # scale_shape_manual("Index", values=c(23,21)) +
  ggthemes::scale_color_colorblind("Index") +
  ggthemes::scale_fill_colorblind("Index") +
  labs(y="Wet-Season Timing", x="") + 
  facet_grid(bioindicator~class3_name, scales = "free_y")

ggsave(filename = "figs/wet-season_timing_vs_csci_asci_boxplots_w_zeroline.png", width = 11, height = 8, dpi = 300, units = "in")

## Boxplot: Spring Timing ----------------------------------------------------------------


bio_ffm_thresh %>% 
  filter(metric == "SP_Tim") %>% 
  #filter(delta_p50 < 3) %>% 
  ggplot() + 
  geom_boxplot(aes(x=biothresh, y=delta_p50, group=biothresh, fill=bioindicator), 
               alpha=0.7, show.legend = FALSE) +
  geom_hline(yintercept = 0, color="maroon", lwd=0.7, lty=1, alpha=0.8) +
  # theme_classic(base_family = "Roboto Condensed") +
  cowplot::theme_half_open() +
  cowplot::background_grid(major="y") +
  theme(plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 70, vjust = .5)) +
  # scale_shape_manual("Index", values=c(23,21)) +
  ggthemes::scale_color_colorblind("Index") +
  ggthemes::scale_fill_colorblind("Index") +
  labs(y="Spring Timing", x="") + 
  facet_grid(bioindicator~class3_name, scales = "free_y")

ggsave(filename = "figs/spring_timing_vs_csci_asci_boxplots_w_zeroline.png", width = 11, height = 8, dpi = 300, units = "in")


## LOOK AT DIFF METRICS FOR DELTA H ONLY AS X FACET AND CSCI/ASCI as Y FACET ----
# california wide, see what these look like.
# bin and plot seasonality by the altered classes, try trend plots then?