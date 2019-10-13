# BRTs (Boosted Regression Trees)
## R. Peek
## BRT Models of the BMI Metrics vs. Flw Metrics
## Use 4 different flow datasets:  Annual, lag1, lag2, and POR
## This is initial cut

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(viridis)
library(sf)
library(mapview)
library(gbm)

# Data --------------------------------------------------------------------

load("data_output/06_selected_bmi_flow_metrics_w_csci_ANN.rda")
load("data_output/06_selected_bmi_flow_metrics_w_csci_POR.rda")
load("data_output/06_selected_bmi_flow_metrics_w_csci_LAG1.rda")
load("data_output/06_selected_bmi_flow_metrics_w_csci_LAG2.rda")
load("data_output/05_selected_bmi_stations_w_comids.rda")
load("data_output/05_mainstems_us_ds_selected_gages.rda")
load("data_output/03_selected_bmi_and_gages.rda")
load("data_output/07_selected_bmi_nearest_usgs_stations.rda")

# Link Regions ------------------------------------------------------------

# read in fish regions:
load("data/07_umbrella_sp_regions.rda")

# spatial join gage sites with regions, adds ATTRIBUTES, retains ALL pts if left=TRUE,
sel_gages_bmi <- st_join(st_transform(sel_gages_bmi, 3310), left = TRUE, ca_sp_regions["huc_region"])

bmi_nearest <- st_join(st_transform(bmi_nearest, 3310), left = TRUE, ca_sp_regions["huc_region"])

mapview(sel_gages_bmi, col.regions="deepskyblue4", cex=7, alpha=0.7) + 
  mapview(mainstems, color="darkblue", lwd=0.5) +
  mapview(ca_sp_regions, zcol="huc_region", alpha.regions=0.3) + 
  mapview(bmi_coms, col.regions="orange", cex=5, alpha=.7)

# Set up Model Vars -------------------------------------------------------

bmi.metrics<-c("Shannon_Diversity", "Simpson_Diversity", "Taxonomic_Richness", "EPT_Percent", "Tolerant_Percent", "Intolerant_Percent", "csci", "csci_percentile", "mmi", "mmi_percentile")

# source functions:
source("code/functions/My.gbm.step.R")
# source("code/functions/My.gbm.fixed.R")
# source("code/functions/My.gbm.simplify.R") 
source("code/functions/brt.functions.R") 
source("code/functions/lm_R2_equation_ggplot.R")


# Select Response Var -----------------------------------------------------

#region_sel <- bmi_nearest %>% filter(huc_region=="north_coast" | huc_region=="south_coast")

## select data and arrange
data_ann <- dplyr::select(bmi_flow_metrics_ann_csci, 1, 106, 151:152, 91:93, 96, one_of(bmi.metrics), 117:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

data_lag1 <- dplyr::select(bmi_flow_metrics_lag1_csci, 1, 106, 151:152, 91:93, 96, one_of(bmi.metrics), 117:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

data_lag2 <- dplyr::select(bmi_flow_metrics_lag2_csci, 1, 106, 151:152, 91:93, 96, one_of(bmi.metrics), 117:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

data_por <- dplyr::select(bmi_flow_metrics_por_csci, 1, 106, 151:152, 91:93, 96, one_of(bmi.metrics), 117:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

# Shannon ANNUAL BRT --------------------------------------------------------

# this matches BMI from same year against flow from same year
# response is SHANNON DIVERSITY

set.seed(33)  # set seed to get repeatable model              

# select cols of interest
dat_shann <- data_ann[,c(9,18:ncol(data_ann))]

# model with Shannons'
gbm1 <- My.gbm.step(data=dat_shann,
                 gbm.x = 2:ncol(dat_shann),          
                 gbm.y = 1, 
                 family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                 tree.complexity = 3,   # thus only models 3nd-order interactions
                 learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                 bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                 n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                 n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

# get the relative % 
gbm1_RI<-as.data.frame(summary(gbm1, plotit = F)) %>% 
  mutate("Ymetric"="Shannon_Diversity",
         "flowdat" = "annual")
rownames(gbm1_RI) <- NULL
str(gbm1_RI)

# DT::datatable(gbm1_RI, caption=htmltools::tags$caption(
#   style = 'caption-side: bottom; text-align: center;',
#   htmltools::em('Table 1. '), 
#   htmltools::em('Relative Influence for ANNUAL MATCH')),
#   colnames = c("Variables"=2, "Relative Influence"=3)) %>% 
#   formatStyle('Relative Influence', 
#               color = styleInterval(c(2,5), c('#440154FF', '#21908CFF', '#FDE725FF')),
#               backgroundColor = styleInterval(c(2,5), c('gray', 'yellow', 'forestgreen'))) %>% formatRound('Relative Influence', 3)

gbm1_topn <- sum((summary(gbm1, plotit=FALSE)$rel.inf)>=5)
(gbm1_topvar <- as.character(summary(gbm1, plotit=FALSE)$var[1:gbm1_topn]))
# make df:
gbm1_ri_top <- tibble(RI=summary(gbm1, plotit=FALSE)$rel.inf[1:gbm1_topn], varnames=gbm1_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(shann_ri_ann <- ggplot() + 
    geom_col(data=gbm1_ri_top, aes(x=varnames, y=RI), 
             fill="#482677FF") +
    coord_flip() + 
    labs(title=paste0("Annual: Top ",gbm1_topn," vars for Shannon Div"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/ann_brt_shannon_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

# Shannon LAG1 BRT --------------------------------------------------------

# this matches BMI against flow from 1 year prior, 
# response is SHANNON DIVERSITY

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_shann1 <- data_lag1[,c(9,18:ncol(data_lag1))]

# model with Shannons'
gbm2 <- My.gbm.step(data=dat_shann1,
                    gbm.x = 2:ncol(dat_shann1),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm2_RI<-as.data.frame(summary(gbm2, plotit = F)) %>% 
  mutate("Ymetric"="Shannon_Diversity",
         "flowdat" = "lag1")
rownames(gbm2_RI) <- NULL

# get top vars
gbm2_topn <- sum((summary(gbm2, plotit=FALSE)$rel.inf)>=5)
(gbm2_topvar <- as.character(summary(gbm2, plotit=FALSE)$var[1:gbm2_topn]))

# make df:
gbm2_ri_top <- tibble(RI=summary(gbm2, plotit=FALSE)$rel.inf[1:gbm2_topn], varnames=gbm2_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(shann_ri_lag1 <- ggplot() + 
  geom_col(data=gbm2_ri_top, aes(x=varnames, y=RI), 
           fill="#33638DFF") + coord_flip() + 
  labs(title=paste0("Lag1: Top ", gbm2_topn," vars for Shannon Div"), y="Relative Influence (%)", x="") +
    ylim(c(0,35)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/lag1_brt_shannon_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

# Shannon LAG2 BRT --------------------------------------------------------

# this matches BMI against flow from 2 year prior, 
# response is SHANNON DIVERSITY

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_shann2 <- data_lag2[,c(9,18:ncol(data_lag2))]

# model with Shannons'
gbm3 <- My.gbm.step(data=dat_shann2,
                    gbm.x = 2:ncol(dat_shann2),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm3_RI<-as.data.frame(summary(gbm3, plotit = F)) %>% 
  mutate("Ymetric"="Shannon_Diversity",
         "flowdat" = "lag2")
rownames(gbm3_RI) <- NULL

# get top vars
gbm3_topn <- sum((summary(gbm3, plotit=FALSE)$rel.inf)>=5)
(gbm3_topvar <- as.character(summary(gbm3, plotit=FALSE)$var[1:gbm3_topn]))

# make df:
gbm3_ri_top <- tibble(RI=summary(gbm3, plotit=FALSE)$rel.inf[1:gbm3_topn], varnames=gbm3_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(shann_ri_lag2 <- ggplot() + 
    geom_col(data=gbm3_ri_top, aes(x=varnames, y=RI), 
             fill="#1F968BFF") + coord_flip() + 
    labs(title=paste0("Lag2: Top ", gbm3_topn," vars for Shannon Div"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/lag2_brt_shannon_top_RI_barplot.png", width = 7, height = 7, units = "in", dpi = 300)


# Shannon POR BRT --------------------------------------------------------

# this matches BMI against flow for period of record, 
# response is SHANNON DIVERSITY

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_shann_por <- data_por[,c(9,18:ncol(data_por))]

# model with Shannons'
gbm4 <- My.gbm.step(data=dat_shann_por,
                    gbm.x = 2:ncol(dat_shann_por),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm4_RI<-as.data.frame(summary(gbm4, plotit = F)) %>% 
  mutate("Ymetric"="Shannon_Diversity",
         "flowdat" = "POR")
rownames(gbm4_RI) <- NULL

# get top vars
gbm4_topn <- sum((summary(gbm4, plotit=FALSE)$rel.inf)>=5)
(gbm4_topvar <- as.character(summary(gbm4, plotit=FALSE)$var[1:gbm4_topn]))

# make df:
gbm4_ri_top <- tibble(RI=summary(gbm4, plotit=FALSE)$rel.inf[1:gbm4_topn], varnames=gbm4_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(shann_ri_por <- ggplot() + 
    geom_col(data=gbm4_ri_top, aes(x=varnames, y=RI), 
             fill="darkblue") + coord_flip() + 
    labs(title=paste0("Period of Record: Top ", gbm4_topn," vars for Shannon Div"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/por_brt_shannon_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)


# Shannon Combine RI Plots and Dataframes-----------------------------------------

library(cowplot)
(shann_ri_combined <- plot_grid(shann_ri_ann, shann_ri_lag1, shann_ri_lag2, shann_ri_por, nrow = 2))

# save
save_plot(shann_ri_combined, filename="figs/brt_shannon_top_RI_barplot_combined.png", base_height = 6)

# bind all the RI and plot later:
shann_RI_all <- bind_rows(gbm1_RI, gbm2_RI, gbm3_RI, gbm4_RI)

# rename and save:
shann_gbm1 <- gbm1
shann_gbm2 <- gbm2
shann_gbm3 <- gbm3
shann_gbm4 <- gbm4

save(shann_RI_all, shann_gbm1, shann_gbm2, shann_gbm3, shann_gbm4, file = "data_output/08_gbm_shannon_RI_all_noSC.rda")


# Taxon. Richness ANNUAL BRT ----------------------------------------------

# this matches BMI from same year against flow from same year
# response is TAXONOMIC_RICHNESS

set.seed(33)  # set seed to get repeatable model              

# select cols of interest
dat_taxon <- data_ann[,c(11,18:ncol(data_ann))]

# model with Taxonomic Richness
gbm1 <- My.gbm.step(data=dat_taxon,
                    gbm.x = 2:ncol(dat_taxon),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm1_RI<-as.data.frame(summary(gbm1, plotit = F)) %>% 
  mutate("Ymetric"="Taxonomic_Richness",
         "flowdat" = "annual")
rownames(gbm1_RI) <- NULL
str(gbm1_RI)

gbm1_topn <- sum((summary(gbm1, plotit=FALSE)$rel.inf)>=5)
(gbm1_topvar <- as.character(summary(gbm1, plotit=FALSE)$var[1:gbm1_topn]))
# make df:
gbm1_ri_top <- tibble(RI=summary(gbm1, plotit=FALSE)$rel.inf[1:gbm1_topn], varnames=gbm1_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(taxon_ri_ann <- ggplot() + 
    geom_col(data=gbm1_ri_top, aes(x=varnames, y=RI), 
             fill="#482677FF") +
    coord_flip() + 
    labs(title=paste0("Annual: Top ",gbm1_topn," vars for Taxon.Richness"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/ann_brt_taxon_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

# Taxon. Richness LAG1 BRT ------------------------------------------------

# this matches BMI against flow from 1 year prior, 
# response is Taxonomic_Richness

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_taxon1 <- data_lag1[,c(11,18:ncol(data_lag1))]

# model with taxons'
gbm2 <- My.gbm.step(data=dat_taxon1,
                    gbm.x = 2:ncol(dat_taxon1),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm2_RI<-as.data.frame(summary(gbm2, plotit = F)) %>% 
  mutate("Ymetric"="Taxonomic_Richness",
         "flowdat" = "lag1")
rownames(gbm2_RI) <- NULL

# get top vars
gbm2_topn <- sum((summary(gbm2, plotit=FALSE)$rel.inf)>=5)
(gbm2_topvar <- as.character(summary(gbm2, plotit=FALSE)$var[1:gbm2_topn]))

# make df:
gbm2_ri_top <- tibble(RI=summary(gbm2, plotit=FALSE)$rel.inf[1:gbm2_topn], varnames=gbm2_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(taxon_ri_lag1 <- ggplot() + 
    geom_col(data=gbm2_ri_top, aes(x=varnames, y=RI), 
             fill="#33638DFF") + coord_flip() + 
    labs(title=paste0("Lag1: Top ", gbm2_topn," vars for Taxon. Richness"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/lag1_brt_taxon_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

# Taxon. Richness LAG2 BRT ------------------------------------------------

# this matches BMI against flow from 2 year prior, 
# response is Taxonomic_Richness

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_taxon2 <- data_lag2[,c(11,18:ncol(data_lag2))]

# model with taxons'
gbm3 <- My.gbm.step(data=dat_taxon2,
                    gbm.x = 2:ncol(dat_taxon2),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm3_RI<-as.data.frame(summary(gbm3, plotit = F)) %>% 
  mutate("Ymetric"="Taxonomic_Richness",
         "flowdat" = "lag2")
rownames(gbm3_RI) <- NULL

# get top vars
gbm3_topn <- sum((summary(gbm3, plotit=FALSE)$rel.inf)>=5)
(gbm3_topvar <- as.character(summary(gbm3, plotit=FALSE)$var[1:gbm3_topn]))

# make df:
gbm3_ri_top <- tibble(RI=summary(gbm3, plotit=FALSE)$rel.inf[1:gbm3_topn], varnames=gbm3_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(taxon_ri_lag2 <- ggplot() + 
    geom_col(data=gbm3_ri_top, aes(x=varnames, y=RI), 
             fill="#1F968BFF") + coord_flip() + 
    labs(title=paste0("Lag2: Top ", gbm3_topn," vars for Taxon. Richness"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/lag2_brt_taxon_top_RI_barplot.png", width = 7, height = 7, units = "in", dpi = 300)


# Taxon. Richness POR BRT ------------------------------------------------

# this matches BMI against flow from period of recorr, 
# response is Taxonomic_Richness

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_taxon_por <- data_por[,c(11,18:ncol(data_por))]

# model with taxons'
gbm4 <- My.gbm.step(data=dat_taxon_por,
                    gbm.x = 2:ncol(dat_taxon_por),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm4_RI<-as.data.frame(summary(gbm4, plotit = F)) %>% 
  mutate("Ymetric"="Taxonomic_Richness",
         "flowdat" = "POR")
rownames(gbm4_RI) <- NULL

# get top vars
gbm4_topn <- sum((summary(gbm4, plotit=FALSE)$rel.inf)>=5)
(gbm4_topvar <- as.character(summary(gbm4, plotit=FALSE)$var[1:gbm4_topn]))

# make df:
gbm4_ri_top <- tibble(RI=summary(gbm4, plotit=FALSE)$rel.inf[1:gbm4_topn], varnames=gbm4_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(taxon_ri_por <- ggplot() + 
    geom_col(data=gbm4_ri_top, aes(x=varnames, y=RI), 
             fill="darkblue") + coord_flip() + 
    labs(title=paste0("Period of Record: Top ", gbm4_topn," vars for Taxon. Richness"), y="Relative Influence (%)", x="") +
    ylim(c(0,40)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/por_brt_taxon_top_RI_barplot.png", width = 7, height = 7, units = "in", dpi = 300)


# Taxon. Richness Combine RI Plots and Dataframes--------------------------

library(cowplot)
(taxon_ri_combined <- plot_grid(taxon_ri_ann, taxon_ri_lag1, taxon_ri_lag2, taxon_ri_por, nrow = 2))
save_plot(taxon_ri_combined, filename="figs/brt_taxon_top_RI_barplot_combined.png", base_height = 7)

# save all now:
(shann_taxon_ri_combined <- plot_grid(
  taxon_ri_ann, taxon_ri_lag1, taxon_ri_lag2, taxon_ri_por,
  shann_ri_ann, shann_ri_lag1, shann_ri_lag2, shann_ri_por,
  nrow=2))

save_plot(shann_taxon_ri_combined, filename="figs/brt_shann_taxon_top_RI_barplot_combined.png", base_height = 8)

# bind all the RI and plot later:
taxon_RI_all <- bind_rows(gbm1_RI, gbm2_RI, gbm3_RI, gbm4_RI)

# rename
taxon_gbm1 <- gbm1
taxon_gbm2 <- gbm2
taxon_gbm3 <- gbm3
taxon_gbm4 <- gbm4

# save
save(taxon_RI_all, taxon_gbm1, taxon_gbm2, taxon_gbm3, taxon_gbm4, file = "data_output/08_gbm_taxonrichness_RI_all.rda")


# CSCI ANNUAL BRT ----------------------------------------------

# this matches BMI from same year against flow from same year
# response is CSCI

set.seed(33)  # set seed to get repeatable model              

# select cols of interest
dat_csci <- data_ann[,c(3,18:ncol(data_ann))] %>% 
  filter(!is.na(csci)) %>% as.data.frame()

# model with Taxonomic Richness
gbm1 <- My.gbm.step(data=dat_csci,
                    gbm.x = 2:ncol(dat_csci),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm1_RI<-as.data.frame(summary(gbm1, plotit = F)) %>% 
  mutate("Ymetric"="CSCI_percentile",
         "flowdat" = "annual")
rownames(gbm1_RI) <- NULL

gbm1_topn <- sum((summary(gbm1, plotit=FALSE)$rel.inf)>=5)
(gbm1_topvar <- as.character(summary(gbm1, plotit=FALSE)$var[1:gbm1_topn]))
# make df:
gbm1_ri_top <- tibble(RI=summary(gbm1, plotit=FALSE)$rel.inf[1:gbm1_topn], varnames=gbm1_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(csci_ri_ann <- ggplot() + 
    geom_col(data=gbm1_ri_top, aes(x=varnames, y=RI), 
             fill="#482677FF") +
    coord_flip() + 
    labs(title=paste0("Annual: Top ",gbm1_topn," vars for CSCI"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/ann_brt_csci_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

# CSCI LAG1 BRT ------------------------------------------------

# this matches BMI against flow from 1 year prior, 
# response is CSCI_percentile

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_csci1 <- data_lag1[,c(3,18:ncol(data_lag1))] %>% 
  filter(!is.na(csci)) %>% as.data.frame()

# model with cscis'
gbm2 <- My.gbm.step(data=dat_csci1,
                    gbm.x = 2:ncol(dat_csci1),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm2_RI<-as.data.frame(summary(gbm2, plotit = F)) %>% 
  mutate("Ymetric"="CSCI_percentile",
         "flowdat" = "lag1")
rownames(gbm2_RI) <- NULL

# get top vars
gbm2_topn <- sum((summary(gbm2, plotit=FALSE)$rel.inf)>=5)
(gbm2_topvar <- as.character(summary(gbm2, plotit=FALSE)$var[1:gbm2_topn]))

# make df:
gbm2_ri_top <- tibble(RI=summary(gbm2, plotit=FALSE)$rel.inf[1:gbm2_topn], varnames=gbm2_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(csci_ri_lag1 <- ggplot() + 
    geom_col(data=gbm2_ri_top, aes(x=varnames, y=RI), 
             fill="#33638DFF") + coord_flip() + 
    labs(title=paste0("Lag1: Top ", gbm2_topn," vars for CSCI"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/lag1_brt_csci_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

# CSCI LAG2 BRT ------------------------------------------------

# this matches BMI against flow from 2 year prior, 
# response is csci

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_csci2 <- data_lag2[,c(3,18:ncol(data_lag2))] %>% 
  select(-DS_No_Flow) %>% 
  filter(!is.na(csci)) %>% as.data.frame()

# model with cscis'
gbm3 <- My.gbm.step(data=dat_csci2,
                    gbm.x = 2:ncol(dat_csci2),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm3_RI<-as.data.frame(summary(gbm3, plotit = F)) %>% 
  mutate("Ymetric"="CSCI_percentile",
         "flowdat" = "lag2")
rownames(gbm3_RI) <- NULL

# get top vars
gbm3_topn <- sum((summary(gbm3, plotit=FALSE)$rel.inf)>=5)
(gbm3_topvar <- as.character(summary(gbm3, plotit=FALSE)$var[1:gbm3_topn]))

# make df:
gbm3_ri_top <- tibble(RI=summary(gbm3, plotit=FALSE)$rel.inf[1:gbm3_topn], varnames=gbm3_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(csci_ri_lag2 <- ggplot() + 
    geom_col(data=gbm3_ri_top, aes(x=varnames, y=RI), 
             fill="#1F968BFF") + coord_flip() + 
    labs(title=paste0("Lag2: Top ", gbm3_topn," vars for CSCI"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/lag2_brt_csci_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

# CSCI POR BRT ------------------------------------------------

# this matches BMI against flow from period of record, 
# response is csci

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_csci_por <- data_por[,c(3,18:ncol(data_por))] %>% 
  select(-DS_No_Flow) %>% 
  filter(!is.na(csci)) %>% as.data.frame()

# model with cscis'
gbm4 <- My.gbm.step(data=dat_csci_por,
                    gbm.x = 2:ncol(dat_csci_por),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.001, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm4_RI<-as.data.frame(summary(gbm4, plotit = F)) %>% 
  mutate("Ymetric"="CSCI_percentile",
         "flowdat" = "POR")
rownames(gbm4_RI) <- NULL

# get top vars
gbm4_topn <- sum((summary(gbm4, plotit=FALSE)$rel.inf)>=5)
(gbm4_topvar <- as.character(summary(gbm4, plotit=FALSE)$var[1:gbm4_topn]))

# make df:
gbm4_ri_top <- tibble(RI=summary(gbm4, plotit=FALSE)$rel.inf[1:gbm4_topn], varnames=gbm4_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(csci_ri_por <- ggplot() + 
    geom_col(data=gbm4_ri_top, aes(x=varnames, y=RI), 
             fill="darkblue") + coord_flip() + 
    labs(title=paste0("Period of Record: Top ", gbm3_topn," vars for CSCI"), y="Relative Influence (%)", x="") +
    ylim(c(0,40)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/por_brt_csci_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)


# CSCI Combine RI Plots and Dataframes--------------------------

(csci_ri_combined <- plot_grid(csci_ri_ann, csci_ri_lag1, csci_ri_lag2, csci_ri_por, nrow = 2))
save_plot(csci_ri_combined, filename="figs/brt_csci_top_RI_barplot_combined.png", base_height = 7)

# save all now:
(shann_taxon_csci_ri_combined <- plot_grid(
  taxon_ri_ann, taxon_ri_lag1, taxon_ri_lag2, taxon_ri_por,
  shann_ri_ann, shann_ri_lag1, shann_ri_lag2, shann_ri_por,
  csci_ri_ann, csci_ri_lag1, csci_ri_lag2, csci_ri_por,
  nrow=3))

save_plot(shann_taxon_csci_ri_combined, filename="figs/brt_shann_taxon_csci_top_RI_barplot_combined.png", base_height = 10)

# bind all the RI and plot later:
csci_RI_all <- bind_rows(gbm1_RI, gbm2_RI, gbm3_RI, gbm4_RI)
#View(csci_RI_all)

#rename
csci_gbm1 <- gbm1
csci_gbm2 <- gbm2
csci_gbm3 <- gbm3
csci_gbm4 <- gbm4

# save
save(csci_RI_all, csci_gbm1, csci_gbm2, csci_gbm3, csci_gbm4, file = "data_output/08_gbm_csci_RI_all.rda")


# EPT ANNUAL BRT ----------------------------------------------

# this matches BMI from same year against flow from same year
# response is EPT

set.seed(33)  # set seed to get repeatable model              

# select cols of interest
dat_ept <- data_ann[,c(12,18:ncol(data_ann))] 

# model with EPT
gbm1 <- My.gbm.step(data=dat_ept,
                    gbm.x = 2:ncol(dat_ept),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm1_RI<-as.data.frame(summary(gbm1, plotit = F)) %>% 
  mutate("Ymetric"="EPT_Percent",
         "flowdat" = "annual")
rownames(gbm1_RI) <- NULL

gbm1_topn <- sum((summary(gbm1, plotit=FALSE)$rel.inf)>=5)
(gbm1_topvar <- as.character(summary(gbm1, plotit=FALSE)$var[1:gbm1_topn]))

# make df:
gbm1_ri_top <- tibble(RI=summary(gbm1, plotit=FALSE)$rel.inf[1:gbm1_topn], varnames=gbm1_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(ept_ri_ann <- ggplot() + 
    geom_col(data=gbm1_ri_top, aes(x=varnames, y=RI), 
             fill="#482677FF") +
    coord_flip() + 
    labs(title=paste0("Annual: Top ",gbm1_topn," vars for EPT"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2)+
    #ggdark::dark_theme_classic(base_family = "Roboto Condensed"))
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/ann_brt_ept_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

# EPT LAG1 BRT ------------------------------------------------

# this matches BMI against flow from 1 year prior, 
# response is EPT

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_ept1 <- data_lag1[,c(12,18:ncol(data_lag1))]

# model with EPT
gbm2 <- My.gbm.step(data=dat_ept1,
                    gbm.x = 2:ncol(dat_ept1),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm2_RI<-as.data.frame(summary(gbm2, plotit = F)) %>% 
  mutate("Ymetric"="EPT_Percent",
         "flowdat" = "lag1")
rownames(gbm2_RI) <- NULL

# get top vars
gbm2_topn <- sum((summary(gbm2, plotit=FALSE)$rel.inf)>=5)
(gbm2_topvar <- as.character(summary(gbm2, plotit=FALSE)$var[1:gbm2_topn]))

# make df:
gbm2_ri_top <- tibble(RI=summary(gbm2, plotit=FALSE)$rel.inf[1:gbm2_topn], varnames=gbm2_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(ept_ri_lag1 <- ggplot() + 
    geom_col(data=gbm2_ri_top, aes(x=varnames, y=RI), 
             fill="#33638DFF") + coord_flip() + 
    labs(title=paste0("Lag1: Top ", gbm2_topn," vars for EPT"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/lag1_brt_ept_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

# EPT LAG2 BRT ------------------------------------------------

# this matches BMI against flow from 2 year prior, 
# response is EPT

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_ept2 <- data_lag2[,c(12,18:ncol(data_lag2))]

# model with EPT
gbm3 <- My.gbm.step(data=dat_ept2,
                    gbm.x = 2:ncol(dat_ept2),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm3_RI<-as.data.frame(summary(gbm3, plotit = F)) %>% 
  mutate("Ymetric"="EPT_Percent",
         "flowdat" = "lag2")
rownames(gbm3_RI) <- NULL

# get top vars
gbm3_topn <- sum((summary(gbm3, plotit=FALSE)$rel.inf)>=5)
(gbm3_topvar <- as.character(summary(gbm3, plotit=FALSE)$var[1:gbm3_topn]))

# make df:
gbm3_ri_top <- tibble(RI=summary(gbm3, plotit=FALSE)$rel.inf[1:gbm3_topn], varnames=gbm3_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(ept_ri_lag2 <- ggplot() + 
    geom_col(data=gbm3_ri_top, aes(x=varnames, y=RI), 
             fill="#1F968BFF") + coord_flip() + 
    labs(title=paste0("Lag2: Top ", gbm3_topn," vars for EPT"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/lag2_brt_ept_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)



# EPT POR BRT ------------------------------------------------

# this matches BMI against flow from POR, 
# response is EPT

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_ept_por <- data_por[,c(18,15:ncol(data_por))]

# model with EPT
gbm4 <- My.gbm.step(data=dat_ept_por,
                    gbm.x = 2:ncol(dat_ept_por),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm4_RI<-as.data.frame(summary(gbm4, plotit = F)) %>% 
  mutate("Ymetric"="EPT_Percent",
         "flowdat" = "POR")
rownames(gbm4_RI) <- NULL

# get top vars
gbm4_topn <- sum((summary(gbm4, plotit=FALSE)$rel.inf)>=5)
(gbm4_topvar <- as.character(summary(gbm4, plotit=FALSE)$var[1:gbm4_topn]))

# make df:
gbm4_ri_top <- tibble(RI=summary(gbm4, plotit=FALSE)$rel.inf[1:gbm4_topn], varnames=gbm4_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(ept_ri_por <- ggplot() + 
    geom_col(data=gbm3_ri_top, aes(x=varnames, y=RI), 
             fill="darkblue") + coord_flip() + 
    labs(title=paste0("Period of Record: Top ", gbm3_topn," vars for EPT"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/por_brt_ept_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)


# EPT Combine RI Plots and Dataframes--------------------------

library(cowplot)
(ept_ri_combined <- plot_grid(ept_ri_ann, ept_ri_lag1, ept_ri_lag2, ept_ri_por, nrow = 2))
save_plot(ept_ri_combined, filename="figs/brt_ept_top_RI_barplot_combined.png", base_height = 7)

# save all now:
(shann_taxon_csci_ept_ri_combined <- plot_grid(
  taxon_ri_ann, taxon_ri_lag1, taxon_ri_lag2, taxon_ri_por,
  shann_ri_ann, shann_ri_lag1, shann_ri_lag2, shann_ri_por,
  csci_ri_ann, csci_ri_lag1, csci_ri_lag2, csci_ri_por,
  ept_ri_ann, ept_ri_lag1, ept_ri_lag2, csci_ri_por,
  nrow=4))

save_plot(shann_taxon_csci_ept_ri_combined, filename="figs/brt_shann_taxon_csci_ept_top_RI_barplot_combined.png", base_height = 10)

# bind all the RI and plot later:
ept_RI_all <- bind_rows(gbm1_RI, gbm2_RI, gbm3_RI, gbm4_RI)
#View(ept_RI_all)

# rename
ept_gbm1 <- gbm1
ept_gbm2 <- gbm2
ept_gbm3 <- gbm3
ept_gbm4 <- gbm4

# save
save(ept_RI_all, ept_gbm1, ept_gbm2, ept_gbm3, ept_gbm4, file = "data_output/08_gbm_ept_RI_all.rda")


# Tolerant ANNUAL BRT ----------------------------------------------

# this matches BMI from same year against flow from same year
# response is Tolerant

set.seed(33)  # set seed to get repeatable model              

# select cols of interest
dat_tol <- data_ann[,c(13,18:ncol(data_ann))] 

# model with % Tolerant
gbm1 <- My.gbm.step(data=dat_tol,
                    gbm.x = 2:ncol(dat_tol),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm1_RI<-as.data.frame(summary(gbm1, plotit = F)) %>% 
  mutate("Ymetric"="Tolerant_Percent",
         "flowdat" = "annual")
rownames(gbm1_RI) <- NULL
str(gbm1_RI)

gbm1_topn <- sum((summary(gbm1, plotit=FALSE)$rel.inf)>=5)
(gbm1_topvar <- as.character(summary(gbm1, plotit=FALSE)$var[1:gbm1_topn]))

# make df:
gbm1_ri_top <- tibble(RI=summary(gbm1, plotit=FALSE)$rel.inf[1:gbm1_topn], varnames=gbm1_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(tol_ri_ann <- ggplot() + 
    geom_col(data=gbm1_ri_top, aes(x=varnames, y=RI), 
             fill="#482677FF") +
    coord_flip() + 
    labs(title=paste0("Annual: Top ",gbm1_topn," vars for Tolerant %"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/ann_brt_tol_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

# Tolerant LAG1 BRT ------------------------------------------------

# this matches BMI against flow from 1 year prior, 
# response is % Tol

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_tol1 <- data_lag1[,c(13,18:ncol(data_lag1))]

# model with tol
gbm2 <- My.gbm.step(data=dat_tol1,
                    gbm.x = 2:ncol(dat_tol1),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm2_RI<-as.data.frame(summary(gbm2, plotit = F)) %>% 
  mutate("Ymetric"="Tolerant_Percent",
         "flowdat" = "lag1")
rownames(gbm2_RI) <- NULL

# get top vars
gbm2_topn <- sum((summary(gbm2, plotit=FALSE)$rel.inf)>=5)
(gbm2_topvar <- as.character(summary(gbm2, plotit=FALSE)$var[1:gbm2_topn]))

# make df:
gbm2_ri_top <- tibble(RI=summary(gbm2, plotit=FALSE)$rel.inf[1:gbm2_topn], varnames=gbm2_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(tol_ri_lag1 <- ggplot() + 
    geom_col(data=gbm2_ri_top, aes(x=varnames, y=RI), 
             fill="#33638DFF") + coord_flip() + 
    labs(title=paste0("Lag1: Top ", gbm2_topn," vars for Tolerant %"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/lag1_brt_tol_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

# Tolerant LAG2 BRT ------------------------------------------------

# this matches BMI against flow from 2 year prior, 
# response is tol

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_tol2 <- data_lag2[,c(13,18:ncol(data_lag2))]

# model with tol
gbm3 <- My.gbm.step(data=dat_tol2,
                    gbm.x = 2:ncol(dat_tol2),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.002, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm3_RI<-as.data.frame(summary(gbm3, plotit = F)) %>% 
  mutate("Ymetric"="Tolerant_Percent",
         "flowdat" = "lag2")
rownames(gbm3_RI) <- NULL

# get top vars
gbm3_topn <- sum((summary(gbm3, plotit=FALSE)$rel.inf)>=5)
(gbm3_topvar <- as.character(summary(gbm3, plotit=FALSE)$var[1:gbm3_topn]))

# make df:
gbm3_ri_top <- tibble(RI=summary(gbm3, plotit=FALSE)$rel.inf[1:gbm3_topn], varnames=gbm3_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(tol_ri_lag2 <- ggplot() + 
    geom_col(data=gbm3_ri_top, aes(x=varnames, y=RI), 
             fill="#1F968BFF") + coord_flip() + 
    labs(title=paste0("Lag2: Top ", gbm3_topn," vars for Tolerant %"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/lag2_brt_tol_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)



# Tolerant POR BRT ------------------------------------------------

# this matches BMI against flow from 2 year prior, 
# response is tol

set.seed(33)  # set seed to get repeatable model              

# select dat
dat_tol_por <- data_por[,c(13,18:ncol(data_por))]

# model with tol
gbm4 <- My.gbm.step(data=dat_tol_por,
                    gbm.x = 2:ncol(dat_tol_por),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode

gbm4_RI<-as.data.frame(summary(gbm4, plotit = F)) %>% 
  mutate("Ymetric"="Tolerant_Percent",
         "flowdat" = "POR")
rownames(gbm4_RI) <- NULL

# get top vars
gbm4_topn <- sum((summary(gbm4, plotit=FALSE)$rel.inf)>=5)
(gbm4_topvar <- as.character(summary(gbm4, plotit=FALSE)$var[1:gbm4_topn]))

# make df:
gbm4_ri_top <- tibble(RI=summary(gbm4, plotit=FALSE)$rel.inf[1:gbm4_topn], varnames=gbm4_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(tol_ri_por <- ggplot() + 
    geom_col(data=gbm4_ri_top, aes(x=varnames, y=RI), 
             fill="darkblue") + coord_flip() + 
    labs(title=paste0("Period of Record: Top ", gbm3_topn," vars for Tolerant %"), y="Relative Influence (%)", x="") +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    theme_classic(base_family = "Roboto Condensed"))

ggsave(filename = "figs/por_brt_tol_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)


# Tolerant Combine RI Plots and Dataframes--------------------------

library(cowplot)
(tol_ri_combined <- plot_grid(tol_ri_ann, tol_ri_lag1, tol_ri_lag2, tol_ri_por, nrow = 2))
save_plot(tol_ri_combined, filename="figs/brt_tol_top_RI_barplot_combined.png", base_height = 7)

# save all now:
(shann_taxon_csci_tol_ri_combined <- plot_grid(
  taxon_ri_ann, taxon_ri_lag1, taxon_ri_lag2, taxon_ri_por,
  shann_ri_ann, shann_ri_lag1, shann_ri_lag2, shann_ri_por,
  csci_ri_ann, csci_ri_lag1, csci_ri_lag2, csci_ri_por,
  tol_ri_ann, tol_ri_lag1, tol_ri_lag2, tol_ri_por,
  nrow=4))

save_plot(shann_taxon_csci_tol_ri_combined, filename="figs/brt_shann_taxon_csci_tol_top_RI_barplot_combined.png", base_height = 10)

# bind all the RI and plot later:
tol_RI_all <- bind_rows(gbm1_RI, gbm2_RI, gbm3_RI, gbm4_RI)
#View(tol_RI_all)

# rename
tol_gbm1 <- gbm1
tol_gbm2 <- gbm2
tol_gbm3 <- gbm3
tol_gbm4 <- gbm4

# save
save(tol_RI_all, tol_gbm1, tol_gbm2, tol_gbm3, tol_gbm4, file = "data_output/08_gbm_tol_RI_all.rda")


# COMBINE ALL RIs ---------------------------------------------------------

bmi_RI_combined <- bind_rows(csci_RI_all, shann_RI_all, taxon_RI_all, ept_RI_all, tol_RI_all)

save(bmi_RI_combined, file = "data_output/08_gbm_bmi_metrics_RI_combined_noSC.rda")



