# 07 Extract and Plot TOP Flow variables from GBM
# R. Peek

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

# GBM evaluation
library(DALEX)
library(ingredients)


# Data --------------------------------------------------------------------

## VARIABLES:
hydroDat <- "POR" # can be Annual, Lag1, Lag2, POR
bmiVar <- quote(csci) # select response var

# get data from GBM outputs:
(brt <- list.files(path="models/", pattern = paste0("^06_gbm_final_", tolower(bmiVar), ".*",tolower(hydroDat),"\\.rds$")))

(brt <- brt[grepl(tolower(hydroDat), x = brt)])

gbm_final <- read_rds(path=paste0("models/", brt))
class(gbm_final)

# get hydrodatasets (for PDPs)
load(paste0("models/06_gbm_final_", tolower(bmiVar),"_hydrodata.rda"))

# rename datasets for plotting:
gbm_out_tr <- data_por_tr # NEED TO CHANGE THESE
gbm_out_te <- data_por_te

bmi_csci <- read_rds("data_output/04_selected_bmi_stations_w_csci.rds")
load("data_output/05_selected_mainstems_final.rda")

# USE DALEX ---------------------------------------------------------------

gbm_explain <- explain(gbm_final, data=gbm_out_tr[,-1], y=gbm_out_tr$csci, na.rm=TRUE)

# feature importance
gbm_feat <- feature_importance(gbm_explain)
plot(gbm_feat, max_vars=12)

# look at explained variance
library(iBreakDown)
gbm_cr <- break_down(gbm_explain, new_observation = gbm_out_tr[1,])
plot(gbm_cr)

## doesn't work with NAs

# partial dependency using ingredients
#gbm_pd <- partial_dependency(gbm_explain, variables="Peak_10",)
#plot(gbm_pd)

# ceteris_paribus
#gbm_cp_pg <- ceteris_paribus(gbm_explain, 
#                             new_observation = gbm_out_tr[1,],
#                             variables="CV")

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
ggsave(filename=tolower(paste0("models/06_gbm_", as_name(bmiVar), "_", hydroDat,"_top_RI_mse", ".png")), width = 8, height = 7, units = "in", dpi = 300)

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
    labs(title=paste0(hydroDat, ": Top ", gbm_fin_topn_pt," vars: ", as_label(bmiVar)), 
         y="Relative Influence (%) (perm test)", x="",
         subtitle = "Permutation Test"
    ) +
    #ylim(c(0,30)) +
    geom_hline(yintercept = 5, color="maroon", lwd=1, lty=2) +
    ggdark::dark_theme_classic(base_family = "Roboto Condensed"))

# save
ggsave(filename=tolower(paste0("models/06_gbm_", as_name(bmiVar), "_", hydroDat,"_top_RI_permtest", ".png")), width = 8, height = 7, units = "in", dpi = 300)


# COMBINE RIs AND SAVE -----------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(paste0(as_name(bmiVar),"_",hydroDat,"_RI")), value=bind_rows(gbm_fin_PT, gbm_fin_RI))
ls(pattern = tolower(as_name(bmiVar)))

write_rds(x = get(ls(pattern = tolower(as_name(bmiVar)))), path = paste0("models/06_gbm_RI_",tolower(as_name(bmiVar)),"_", tolower(hydroDat), ".rds"), compress = "gz")

# DISMO: Marginal FX Plots ----------------------------------------------

# MARGINAL FX:: partial dependency or marginal effect plots (ALL)
pdf(file=paste0("models/06_gbm_marginal_effects_",
                tolower(as_name(bmiVar)),
                "_",tolower(hydroDat),".pdf"),
    width = 9, height = 6)
                
gbm.plot(gbm_final, rug = T, n.plots = 8, show.contrib = T, 
         smooth=T, write.title = F, common.scale = T,
         y.label = as_name(bmiVar), plot.layout = c(2,4))
title(main=paste0("Flow Data: ", hydroDat), outer = T, line = -1.5)

dev.off()

# PDP: Partial Dependence Plots -----------------------------------------

library(pdp)

## displays avg change in predicted Y VAR as we vary an X VAR while holding everything else constant

# get top var
(bestHydro_ri <- gbm_fin_ri_top %>% top_n(n = 3, RI))
(bestHydro_pt <- gbm_fin_pt_top %>% top_n(n = 3, PT))

# set top var Number
varNo <- 1 # single number makes single plot

# SINGLE PLOT
gbm_final %>%
  partial(pred.var = bestHydro_ri$varnames[varNo], train=gbm_out_tr,
          n.trees = gbm_final$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = gbm_out_tr) +
  ggdark::dark_theme_classic(base_family = "Roboto Condensed") +
  labs(subtitle = paste0("Partial Dependence Plot: ",bestHydro_ri$varnames[varNo]),
       x=paste0(bestHydro_ri$varnames[varNo]),
       y=paste0("Predicted ", as_name(bmiVar)))

## HEAT MAP PLOT
varNos <- c(1:2) # mult number makes heat plot

# DUAL PLOT (HEATMAP)
gbm_final %>%
  partial(pred.var = bestHydro_ri$varnames[varNos], 
          train=gbm_out_tr,
          n.trees = gbm_final$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = gbm_out_tr) +
  scale_fill_viridis(as_name(bmiVar)) + 
  ggdark::dark_theme_classic(base_family = "Roboto Condensed") +
  labs(subtitle = paste0("Partial Dependence Plot: RI"),
       x=paste0(bestHydro_ri$varnames[varNos][2]),
       y=paste0(bestHydro_ri$varnames[varNos][1]))

# Save 
#ggsave(filename=tolower(paste0("figs/10_gbm_final_pdp_ri_",as_name(bmiVar),  "_top_vars.png")), width = 11, height = 7, units = "in", dpi=300)

# ICE: Indiv Conditional Expectation PLOTS --------------------------------------------------

## ICE (Individual conditonal expectation) plots: rather than plot the average marginal effect on the response variable, we plot the change in the predicted response variable for each observation as we vary each predictor variable.

## The equivalent to a PDP for individual data instances is called individual conditional expectation (ICE) plot (Goldstein et al. 2017). An ICE plot visualizes the dependence of the prediction on a feature for each instance separately, resulting in one line per instance, compared to one line overall in partial dependence plots.

# When the curves have a wide range of intercepts and are consequently “stacked” on each other, heterogeneity in the response variable values due to marginal changes in the predictor variable of interest can be difficult to discern, thus centering can help

# PT
# ice1_pt <- gbm.fit.final %>%
#   partial(
#     pred.var = as.character(bestHydro_pt$varnames[varNo]), 
#     n.trees = gbm.fit.final$n.trees, 
#     grid.resolution = 100,
#     ice = TRUE) %>%
#   autoplot(rug = TRUE, train = gbm_out, alpha = .1) +
#   labs(subtitle = paste0("ICE (PT): ", bestHydro_pt$varnames[varNo]),
#        y=paste0("Predicted ", as_name(bmiVar))) +
#   ggdark::dark_theme_classic(base_family = "Roboto Condensed")
# ice1_pt

(ice2_pt <- gbm_final %>%
   partial(
     pred.var = as.character(bestHydro_pt$varnames[varNo]), 
     n.trees = gbm_final$n.trees, train=gbm_out_tr,
     grid.resolution = 100,
     ice = TRUE) %>%
   autoplot(rug = TRUE, train = gbm_out_tr, alpha = .1, center=TRUE) +
   labs(subtitle = paste0("ICE Centered (PT): ", 
                          bestHydro_pt$varnames[varNo], " for ", hydroDat),
        y=paste0("Predicted ", as_name(bmiVar))) +
   ggdark::dark_theme_classic(base_family = "Roboto Condensed"))

# RI

(ice2_ri <- gbm_final %>%
  partial(
    pred.var = as.character(bestHydro_ri$varnames[varNo]), 
    n.trees = gbm_final$n.trees, train=gbm_out_tr,
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = gbm_out_tr, alpha = .1, center = TRUE) +
    labs(subtitle = paste0("ICE Centered (RI): ", bestHydro_ri$varnames[varNo], " for ", hydroDat),
         y=paste0("Predicted ", as_name(bmiVar))) +
  ggdark::dark_theme_classic(base_family = "Roboto Condensed"))

# RI save:
ggsave(filename=paste0("figs/pdp_ice_",tolower(as_name(bmiVar)), "_",hydroDat,
                       "_top_var_", as.character(bestHydro_ri$varnames[varNo]),
                       ".png"), width = 11, height = 7, units = "in", dpi=300)


# plot the non-centered
# gridExtra::grid.arrange(ice1_pt, ice1_ri, nrow = 1)
# plot centered
# gridExtra::grid.arrange(ice2_pt, ice2_ri, nrow = 1)
cowplot::plot_grid(ice2_pt, ice2_ri, nrow = 1, labels = "POR", 
                   label_colour = "white", label_fontfamily = "Roboto Condensed", label_size = 10)

# permutation test:
ggsave(filename=paste0("figs/pdp_ice_",tolower(as_name(bmiVar)),  
                       "_top_var_", 
                       as.character(bestHydro_pt$varnames[varNo]),"_",
                       as.character(bestHydro_ri$varnames[varNo]),
                       ".png"), width = 11, height = 7, units = "in", dpi=300)


# Predict -----------------------------------------------------------------

# predict values for test data
pred <- predict(gbm_final, n.trees = gbm_final$n.trees, gbm_out_te)

# results
caret::RMSE(pred, gbm_out_tr[,1])
