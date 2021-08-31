# Identify top RI Flow Metrics
# summarize data from all GBMs

# Libraries ---------------------------------------------------------------

library(gt)
library(glue)
suppressPackageStartupMessages(library(tidyverse))
library(sf)
sf_use_s2(FALSE)

library(viridis) # colors
library(rlang)
library(purrr)

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


# ALL CA: Seasonality ----------------------

bioVar <- "csci"
(modname <- glue("{bioVar}_all_ca_seasonality")) # model name 

# make pathnames
(mod_savename <- tolower(glue("08_gbm_{modname}")))

# get the gbm model:
(top_ri <- list.files(path="models/", pattern = glue("^{mod_savename}_RI_combined.*\\.rds$")))
top_ris <- read_rds(file=glue("models/{top_ri}"))

# make sep and combine
ri_csci_all_ca <- top_ris %>% mutate(model="all_ca")

# change biovar
bioVar <- "asci"
(modname <- glue("{bioVar}_all_ca_seasonality")) # model name 

# make pathnames
(mod_savename <- tolower(glue("08_gbm_{modname}")))

# get the gbm model:
(top_ri <- list.files(path="models/", pattern = glue("^{mod_savename}_RI_combined.*\\.rds$")))
top_ris <- read_rds(file=glue("models/{top_ri}"))

# make sep and combine
ri_asci_all_ca <- top_ris %>% mutate(model="all_ca")

## bind
ri_all_regions <- bind_rows(ri_csci_all_ca, ri_asci_all_ca)

## save out for later
#save(ri_all_regions, file = tolower(glue::glue("models/09_combined_ri_all_ca_seasonality.rda")))

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
           flow_component == "Stream Class" ~ "Stream Class",
           var == "Power.avg" ~ "Wavelet Interannual",
           var == "MP_metric" ~ "Colwell's M/P Intrannual",
           TRUE ~ Flow.Metric.Name),
         Flow.Metric.Name = as.factor(Flow.Metric.Name),
         Flow.Metric.Name = forcats::fct_reorder2(Flow.Metric.Name, model, RI)) 

levels(ri_table$var)
levels(ri_table$flow_component)
levels(ri_table$Flow.Metric.Name)
summary(ri_table)

# save out:
write_rds(ri_table, file = glue("models/09_combined_ri_all_ca_seasonality.rds"))


# generate order by CA wide RI for flow metrics:
forder <- ri_table %>% 
  filter(model=="all_ca", 
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

# most common hydrometric by flowdata type?
ri_table %>% group_by(flowdat, var) %>% 
  summarize(meanRI = mean(RI),
            sumRI = sum(RI)) %>% 
  top_n(5) %>% 
  arrange(flowdat, desc(meanRI))

# so best hydrometric across model?
ri_table %>% group_by(var) %>% 
  summarize(meanRI = mean(RI),
            medianRI = median(RI),
            maxRI = max(RI),
            SD = sd(RI)) %>% 
  top_n(5) %>% group_by(var) %>% tally() %>% arrange(desc(n))


# Summary Table ALL CA -----------------------------------------------------------

library(glue)

model_name <- "All CA"
## ALL CA Table
# Create a gt table based on preprocessed table
ri_table %>%
  dplyr::filter(method=="mse", model=="all_ca") %>%
    #model=="all_ca", 
  dplyr::select(-c(Flow.Component, flowdat,  method, Flow.Characteristic)) %>%
  dplyr::select(Ymetric, flow_component, var, Flow.Metric.Name:Flow.Metric.Description, RI) %>%
  #arrange(Flow.Component, var) %>% #View() 
  arrange(RI, flow_component) %>% #View() 
  select(-var) %>% 
  gt() %>%
  tab_header(
    title = "Relative Influence of Functional Flow Metrics on CSCI",
    subtitle = glue::glue("Model {model_name}")
  ) %>%
  fmt_number(
    columns = c(RI), decimals = 1, 
    drop_trailing_zeros = T
  )

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
(plot_savename <- tolower(glue("09_gbm_combined_{modname}")))

# plot
ri_table %>% 
  filter(model=="all_ca", 
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
  scale_shape_manual("Index", values=c("asci"=23, "csci"=21))+
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
ggsave(filename=tolower(glue("models/{plot_savename}_all_ri_sized_points_w_lines_ranked.png")), width = 9, height = 7, units = "in", dpi = 300)

# Lollipop Plot: ALL CA WITHOUT seasonality ------------------------------------------------------------

# darker for peak flow
flowcomponent_colors <- c("Fall pulse flow" = "#F0E442", "Wet-season baseflow" = "#56B4E9",
                          "Peak flow" = "#404788FF", "Spring recession flow" = "#009E73", 
                          "Dry-season baseflow" = "#D55E00")

plotname <- "All CA"
modname <- "all_ca_ffc_only"
(plot_savename <- tolower(glue("09_gbm_combined_{modname}")))

# plot
ri_table %>% 
  filter(model=="all_ca", 
         method=="mse",
         !flow_component=="Seasonality") %>% 
  ggplot() +
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
  scale_shape_manual("Index", values=c("asci"=23, "csci"=21))+
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
ggsave(filename=tolower(glue("models/{plot_savename}_all_ri_sized_points_w_lines_ranked.png")), width = 9, height = 7, units = "in", dpi = 300)
