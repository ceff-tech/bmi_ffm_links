# 08 Extract and Plot TOP Flow variables from GBM
# R. Peek

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(viridis) # colors
library(cowplot)
library(gbm) # boosted regression trees
library(dismo)
library(pdp)
library(rlang)
library(glue)
#extrafont::loadfonts(quiet=TRUE)

# turn off spherical geometries
sf_use_s2(FALSE)

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

# make a simpler layer for mapping
csci_sites <- bio_ffm %>% 
  filter(bioindicator=="CSCI") %>% 
  dplyr::distinct(SampleID, gageid, .keep_all = TRUE)
table(csci_sites$class3_name) # list of unique stations

asci_sites <- bio_ffm %>% 
  filter(bioindicator=="ASCI") %>% 
  dplyr::distinct(SampleID, gageid, .keep_all = TRUE)
table(asci_sites$class3_name) # list of unique stations


# Load Data --------------------------------------------------------------------

## VARIABLES:
hydroDat <- "POR"
bioVar <- "csci" # select response var
# "all_ca_seasonality", "mixed_seasonality", "rain_seasonality"
modelname <- "rain_seasonality" 
(modname <- glue("{bioVar}_{modelname}")) # model name 
(plotname <- glue("{toupper(bioVar)}: {modelname}"))  #"All Site Pairs"

# make pathnames
(mod_pathname <- glue("07_gbm_final_{modname}"))
(mod_savename <- tolower(glue("08_gbm_{modname}")))

# get the gbm model:
(brt <- list.files(path="models/", pattern = paste0("^", mod_pathname,".*\\.rds$")))

gbm_final <- read_rds(file=paste0("models/", brt))
class(gbm_final)

# get model datasets (for PDPs)
load(glue("models/{mod_pathname}_model_data.rda"))

# rename datasets for plotting:
gbm_out_train <- data_strmclass_train # NEED TO CHANGE THESE
#gbm_out_train <- data_por_train # NEED TO CHANGE THESE

# % percent explained
(gbm_final$self.statistics$mean.null - gbm_final$cv.statistics$deviance.mean) / gbm_final$self.statistics$mean.null 

# 01A. RELATIVE INFLUENCE PLOTS (MSE) ALL VARS -------------------------------------

## RI: improvement by MSE (split criterion), vars with largest avg decrease in MSE are important
gbm_fin_RI<-as.data.frame(summary(gbm_final, plotit = F, method=relative.influence)) %>% 
  mutate("Ymetric"= as_name(bioVar),
         "flowdat" = hydroDat,
         "method" = "mse",
         "model" = glue("{modelname}"))

# add flow components for plotting purposes
gbm_fin_RI <- gbm_fin_RI %>% 
  mutate(flow_component = case_when(
    grepl("DS_", var) ~ "Dry-season baseflow",
    grepl("SP_", var) ~ "Spring recession flow",
    grepl("Peak_", var) ~ "Peak flow",
    grepl("Wet_", var) ~ "Wet-season baseflow",
    grepl("FA_", var) ~ "Fall pulse flow",
    grepl("MP_metric|Power.avg", var) ~ "Seasonality",
    grepl("class3_name", var) ~ "Stream Class",
    TRUE ~ "General"
  ),
  flow_component = factor(flow_component, levels = c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow", "Seasonality", 
                                                     "Stream Class", "General")),
  var = as.factor(var),
  var = fct_reorder2(var, flow_component, var)) %>% 
  rename(RI = rel.inf)

## Now Plot ALL
(fin_ri <- gbm_fin_RI %>% 
  arrange(desc(RI)) %>% 
  filter(flow_component!="General") %>% 
  #filter(RI > 3) %>% #View()
  ggplot(.) +
  geom_col(aes(x=forcats::fct_reorder(var, RI),
               y=RI, fill=flow_component), color="gray20", lwd=.1,
           position="dodge") +
  coord_flip() +
  geom_hline(yintercept = 5, color="gray40", lwd=1, lty=2, alpha=0.8)+
  #ylim(c(0,30))+
  scale_fill_viridis_d("Flow Component")+
  labs(title = plotname,
       subtitle="MSE Criterion",
       x="", y="Relative Influence (%)") +
  theme_classic(base_family = "Roboto Condensed")) 

# save out
ggsave(filename=tolower(glue("models/{mod_savename}_all_RI_mse.png")), width = 9, height = 7, units = "in", dpi = 300)

# 01B. RELATIVE INFLUENCE PLOTS (MSE) TOP VARS -------------------------------------

# and plot top vars only
(fin_ri_top <- gbm_fin_RI %>% 
    arrange(desc(RI)) %>% 
    filter(flow_component!="General", RI>5) %>% 
    ggplot(.) +
    geom_col(aes(x=reorder(var, RI),
                 y=RI, fill=flow_component), color="gray20", lwd=.1,
             position="dodge") +
    coord_flip() +
    geom_hline(yintercept = 5, color="gray40", lwd=1, lty=2, alpha=0.8)+
    #ylim(c(0,30))+
    scale_fill_viridis_d("Flow Component")+
    labs(title= plotname,
         x="", y="Relative Influence (%)", subtitle="MSE Criterion") +
    theme_classic(base_family = "Roboto Condensed")) 

# save out
ggsave(filename=tolower(glue("models/{mod_savename}_top_RI_mse.png")), width = 9, height = 7, units = "in", dpi = 300)


# 02A. RI PERMUTATION TEST PLOTS ALL VARS ------------------------------------------------

## PT: permutation test, decrease in accuracy is averaged and vars with largest avg decrease in accuracy are import

gbm_fin_PT<-as.data.frame(summary(gbm_final, plotit = F, method=permutation.test.gbm)) %>% 
  mutate("Ymetric"= as_name(bioVar),
         "flowdat" = hydroDat,
         "method" = "permtest",
         "model" = glue("{modelname}"))
gbm_fin_PT <- gbm_fin_PT %>% 
  mutate(flow_component = case_when(
    grepl("DS_", var) ~ "Dry-season baseflow",
    grepl("SP_", var) ~ "Spring recession flow",
    grepl("Peak_", var) ~ "Peak flow",
    grepl("Wet_", var) ~ "Wet-season baseflow",
    grepl("FA_", var) ~ "Fall pulse flow",
    grepl("MP_metric|Power.avg", var) ~ "Seasonality",
    grepl("class3_name", var) ~ "Stream Class",
    TRUE ~ "General"
  ),
  flow_component = factor(flow_component, levels = c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow", "Seasonality", 
                                                     "Stream Class", "General")),
  var = as.factor(var),
  var = fct_reorder2(var, flow_component, var)) %>% 
  rename(RI = rel.inf)

## Now Plot ALL (Most Accurate)
(fin_pt <- gbm_fin_PT %>% 
    arrange(desc(RI)) %>% 
    filter(flow_component!="General") %>% 
    ggplot(.) +
    geom_col(aes(x=var,
                 y=RI, fill=flow_component), color="gray20", lwd=.1,
             position="dodge") +
    coord_flip() +
    geom_hline(yintercept = 5, color="gray40", lwd=1, lty=2, alpha=0.8)+
    #ylim(c(0,50))+
    scale_fill_viridis_d("Flow Component")+
    labs(title= plotname,
         y="Relative Influence (%) (perm test)", x="",
         subtitle = "Permutation Test") +
    theme_classic(base_family = "Roboto Condensed")) 

# save out
ggsave(filename=tolower(glue("models/{mod_savename}_all_RI_permtest.png")), width = 9, height = 7, units = "in", dpi = 300)


# 02B. RI PERMUTATION TEST PLOTS TOP VARS ------------------------------------------------

# and plot top vars only
(fin_pt_top <- gbm_fin_PT %>% 
   arrange(desc(RI)) %>% 
   filter(flow_component!="General", RI>5) %>% 
   ggplot(.) +
   geom_col(aes(x=reorder(var, RI),
                y=RI, fill=flow_component), color="gray20", lwd=.1,
            position="dodge") +
   coord_flip() +
   geom_hline(yintercept = 5, color="gray40", lwd=1, lty=2, alpha=0.8)+
   #ylim(c(0,50))+
   scale_fill_viridis_d("Flow Component")+
   labs(title=plotname,
        y="Relative Influence (%) (perm test)", x="",
        subtitle = "Permutation Test") +
   theme_classic(base_family = "Roboto Condensed")) 

# save out
ggsave(filename=tolower(glue("models/{mod_savename}_top_RI_permtest.png")), width = 9, height = 7, units = "in", dpi = 300)

# 03. COMBINE RIs AND SAVE -----------------------------------------------------

# reassign names for RI outputs and save:
tolower(glue("{as_name(bioVar)}_{hydroDat}_RI"))
assign(x = tolower(glue("{as_name(bioVar)}_{hydroDat}_RI")), value=bind_rows(gbm_fin_PT, gbm_fin_RI))

(filepattern <- ls(pattern = paste0("^",tolower(as_name(bioVar)))))

write_rds(x = csci_por_ri, file = glue("models/{mod_savename}_RI_combined.rds"))
#write_rds(x = asci_por_ri, file = glue("models/{mod_savename}_RI_combined.rds"))

# 04. MARGINAL FX Plots ----------------------------------------------

# MARGINAL FX:: partial dependency or marginal effect plots (ALL)

#pdf(file=paste0("models/", mod_savename,"_partial_depend_plots.pdf"),
#    width = 11, height = 8)
                
# gbm.plot(gbm_final, rug = T, n.plots = 9, show.contrib = T, 
#          smooth=T, write.title = F, common.scale = T,
#          y.label = as_name(bioVar), plot.layout = c(3,3))
#title(main=glue("Flow Data: {plotname}"), outer = T, line = -1.5)
#dev.off()
