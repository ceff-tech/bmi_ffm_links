# 10 Id top flow variables from BRT


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(leaflet)
library(mapview)
library(viridis) # colors
library(gbm) # boosted regression trees
library(dismo)
library(pdp)
library(rlang)


# Data --------------------------------------------------------------------

# get data from GBM outputs:
(brt <- list.files(path="data_output/gbms", pattern = "^10_gbm_final.*\\.rds$"))

gbm_final <- read_rds(path=paste0("data_output/gbms/", brt))

# get hydrodatasets (for PDPs)
load("data_output/gbms/10_gbm_final_csci_percentile_hydrodata.rda")

## VARIABLES:
hydroDat <- "Annual" # can be Annual, Lag1, Lag2, POR
bmiVar <- quote(csci_percentile) # select response var from list above

#load("data_output/08_gbm_bmi_metrics_RI_combined_noSC.rda")
load("data_output/05_selected_bmi_stations_w_comids.rda")
load("data_output/07_mainstems_bmi_selected_gages.rda")


# MAKE RI RELATIVE INFLUENCE PLOTS (MSE) -------------------------------------

## RI: improvement by MSE (split criterion), vars with largest avg decrease in MSE are important

gbm_fin_RI<-as.data.frame(summary(gbm_final, plotit = F, method=relative.influence)) %>% 
  mutate("Ymetric"= as_name(bmiVar),
         "flowdat" = hydroDat,
         "method" = "mse")

rownames(gbm_fin_RI) <- NULL

# get top vars >=5 RI
gbm_fin_topn <- sum((summary(gbm_final, plotit=FALSE)$rel.inf)>=5)

# get var names
(gbm_fin_topvar <- as.character(summary(gbm_final, plotit=FALSE)$var[1:gbm_fin_topn]))

# make df:
gbm_fin_ri_top <- tibble(RI=summary(gbm_final, plotit=FALSE)$rel.inf[1:gbm_fin_topn], varnames=gbm_fin_topvar) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), RI))

# barplot
(fin_ri_ann <- ggplot() + 
    geom_col(data=gbm_fin_ri_top, aes(x=varnames, y=RI), 
             fill="skyblue") + coord_flip() + 
    labs(title=paste0(hydroDat, ": Top ", gbm_fin_topn," vars: ", as_label(bmiVar)), 
         y="Relative Influence (%)", x="",
         subtitle = "MSE Criterion") +
    ylim(c(0,30)) +
    ggdark::dark_theme_classic(base_family = "Roboto Condensed")+
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2)) 

# save out
ggsave(filename=tolower(paste0("figs/10_gbm_", as_name(bmiVar), "_", hydroDat,"_top_RI", ".png")), width = 8, height = 7, units = "in", dpi = 300)

# MAKE RI PERMUTATION TEST PLOTS ------------------------------------------------

## PT: permutation test, decrease in accuracy is averaged and vars with largest avg decrease in accuracy are import

gbm_fin_PT<-as.data.frame(summary(gbm_final, plotit = F, method=permutation.test.gbm)) %>% 
  mutate("Ymetric"= as_name(bmiVar),
         "flowdat" = hydroDat,
         "method" = "permtest")
rownames(gbm_fin_PT) <- NULL

# get top vars
gbm_fin_topn_pt <- sum((summary(gbm_final, method=permutation.test.gbm, plotit=FALSE)$rel.inf)>=5)
(gbm_fin_topvar_pt <- as.character(summary(gbm_final, 
                                           method=permutation.test.gbm, plotit=FALSE)$var[1:gbm_fin_topn_pt]))

# make df:
gbm_fin_pt_top <- tibble(PT=summary(gbm_final, method=permutation.test.gbm, plotit=FALSE)$rel.inf[1:gbm_fin_topn_pt], 
                         varnames=gbm_fin_topvar_pt) %>% 
  mutate(varnames=fct_reorder(as.factor(varnames), PT))

# barplot by most ACCURATE
(fin_pt_ann <- ggplot() + 
    geom_col(data=gbm_fin_pt_top, aes(x=varnames, y=PT), 
             fill="skyblue") + coord_flip() + 
    labs(title=paste0(hydroDat, ": Top ", gbm_fin_topn," vars: ", as_label(bmiVar)), 
         y="Relative Influence (%) (perm test)", x="",
         subtitle = "Permutation Test"
    ) +
    ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    ggdark::dark_theme_classic(base_family = "Roboto Condensed"))

# save
ggsave(filename=tolower(paste0("figs/10_gbm_", as_name(bmiVar), "_", hydroDat,"_top_RI_pt", ".png")), width = 8, height = 7, units = "in", dpi = 300)


# COMBINE RIs AND SAVE -----------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(paste0(as_name(bmiVar),"_",hydroDat,"_RI")), value=bind_rows(gbm_fin_PT, gbm_fin_RI))
ls(pattern = as_name(bmiVar))
write_rds(x = ls(pattern = as_name(bmiVar)), path = paste0("data_output/gbms/10_gbm_final_RI_",as_name(bmiVar),"_", tolower(hydroDat), ".rds"), compress = "gz")

# DISMO: Marginal FX Plots ----------------------------------------------

# MARGINAL FX:: partial dependency or marginal effect plots (ALL)
pdf(file=paste0("figs/10_gbm_marginal_effects_",as_name(bmiVar),
                "_",tolower(hydroDat),".pdf"),
    width = 9, height = 6)
                
gbm.plot(gbm_final, rug = T, n.plots = 8, show.contrib = T, 
         smooth=T, write.title = F, common.scale = T,
         y.label = "CSCI", plot.layout = c(2,4))

dev.off()

# PDP: Partial Dependence Plots -----------------------------------------

library(pdp)

## displays avg change in predicted Y VAR as we vary an X VAR while holding everything else constant

# get top var
(bestHydro_ri <- gbm_fin_ri_top %>% top_n(n = 3, RI))
#(bestHydro_pt <- gbm_fin_pt_top %>% top_n(n = 3, PT))

# set top var Number
varNo <- 1 # single number makes single plot

# SINGLE PLOT
gbm_final %>%
  partial(pred.var = bestHydro_ri$varnames[varNo], train=data_ann_tr,
          n.trees = gbm_final$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = data_ann_tr) +
  ggdark::dark_theme_classic(base_family = "Roboto Condensed") +
  labs(subtitle = paste0("Partial Dependence Plot: ",bestHydro_ri$varnames[varNo]),
       x=paste0(bestHydro_ri$varnames[varNo]),
       y=paste0("Predicted ", as_name(bmiVar)))

## HEAT MAP PLOT
varNo <- c(1:2) # mult number makes heat plot

# DUAL PLOT (HEATMAP)
gbm_final %>%
  partial(pred.var = bestHydro_ri$varnames[varNo], 
          train=data_ann_tr,
          n.trees = gbm_final$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = data_ann_tr) +
  scale_fill_viridis(as_name(bmiVar)) + 
  ggdark::dark_theme_classic(base_family = "Roboto Condensed") +
  labs(subtitle = paste0("Partial Dependence Plot: RI"),
       x=paste0(bestHydro_ri$varnames[varNo][2]),
       y=paste0(bestHydro_ri$varnames[varNo][1]))

# Save 
ggsave(filename=tolower(paste0("figs/10_gbm_final_pdp_ri_",as_name(bmiVar),  "_top_vars.png")), width = 11, height = 7, units = "in", dpi=300)

