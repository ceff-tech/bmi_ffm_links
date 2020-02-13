# pca

library(factoextra)
library(tidyverse)
library(lubridate)
library(viridis)
library(cowplot)


# Load Data ---------------------------------------------------------------

load("data_output/selected_bmi_flow_metrics_w_csci_ANN.rda")
load("data_output/selected_bmi_flow_metrics_w_csci_POR.rda")
load("data_output/selected_bmi_flow_metrics_w_csci_LAG1.rda")
load("data_output/selected_bmi_flow_metrics_w_csci_LAG2.rda")
load("data_output/selected_bmi_stations_w_comids.rda")
load("data_output/maintems_us_ds_selected_gages.rda")

# Set up Model Vars -------------------------------------------------------

bmi.metrics<-c("Shannon_Diversity", "Simpson_Diversity", "Taxonomic_Richness", "EPT_Percent", "Tolerant_Percent")

## select data and arrange
data_ann <- dplyr::select(bmi_flow_metrics_ann_csci, 1, 106, 151, 91:93, 153:154, one_of(bmi.metrics), 95, 112, 117:150) %>% 
  mutate(stream_class = as.factor(stream_class)) %>% 
  as.data.frame() %>% 
  drop_na() # DROPS A LOT OF VARS (200 > 24)

data_lag1 <- dplyr::select(bmi_flow_metrics_lag1_csci, 1, 106, 151, 91:93, 153:154, one_of(bmi.metrics), 95, 112, 117:150) %>% 
  mutate(stream_class = as.factor(stream_class)) %>% 
  as.data.frame() %>% 
  drop_na() # DROP 200 to 27

data_lag2 <- dplyr::select(bmi_flow_metrics_lag2_csci, 1, 106, 151, 91:93, 153:154, one_of(bmi.metrics), 95, 112, 117:150) %>% 
  mutate(stream_class = as.factor(stream_class)) %>% 
  as.data.frame() %>% 
  drop_na() # Drop 200 to 27

# PCA ---------------------------------------------------------------------


# simple PCA with CSCI
ann_pr <- prcomp(data_ann[c(8:13,16:49)], center = TRUE, scale = TRUE)
summary(ann_pr)

# plot top15 variables
(vars_ann<-fviz_contrib(ann_pr, choice="var", axes=c(1:2), top=15, title=FALSE,subtitle="ANNUAL")) #+
    #labs(caption="Dashed line corresponds to the expected value if the contributions were uniform."))

# lag1 pca
lag1_pr <- prcomp(data_lag1[c(8:13,16:49)], center = TRUE, scale = TRUE)
summary(lag1_pr)
(varslag1<-fviz_contrib(lag1_pr, choice="var", axes=c(1:2), top=15, title=FALSE, subtitle="LAG1"))# +
    #labs(caption="Dashed line corresponds to the expected value if the contributions were uniform."))

# lag2 pca
lag2_pr <- prcomp(data_lag2[c(8:13,16:49)], center = TRUE, scale = TRUE)
summary(lag2_pr)
(varslag2<-fviz_contrib(lag2_pr, choice="var", axes=c(1:2), top=15, title=FALSE, subtitle="LAG2") +
    labs(caption="Dashed line corresponds to the expected value if the contributions were uniform."))

# smoosh into one plot
cowplot::plot_grid(vars_ann, varslag1, varslag2, nrow = 3) %>% 
  save_plot(filename = "figs/pca_top15_vars.png", base_width = 11, base_height = 8, units="in")

# PCA PLOT & EVAL ---------------------------------------------------------

# Since an eigenvalues <1 would mean that the component actually explains less than a single explanatory variable, discard those
screeplot(ann_pr, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

# Look at cumulative variance explained by first 15 PCs
cumpro <- cumsum(ann_pr$sdev^2 / sum(ann_pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.79, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6, 79% of var"),
       col=c("blue"), lty=5, cex=0.6)

# now look at PCA biplot with vars
fviz_pca(ann_pr) +
  scale_fill_viridis() +
  ggtitle("PCA: Annual")

# this does just points
fviz_pca_ind(ann_pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = data_ann$Peak_Mag_10, 
             col.ind = "black",
             addEllipses = F,
             label = "var",
             col.var = "black",
             repel = TRUE, show.legend=FALSE) +
  scale_fill_viridis_c("Peak Mag 10") +
  ggtitle("PCA: Annual") +
  theme(plot.title = element_text(hjust = 0.5))

# now lag1
fviz_pca(lag1_pr) +
  scale_fill_viridis() +
  ggtitle("PCA: Lag 1")

fviz_pca_ind(lag1_pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = data_lag1$Peak_Mag_10, 
             col.ind = "black",
             addEllipses = F,
             label = "var",
             col.var = "black",
             repel = TRUE, show.legend=FALSE) +
  scale_fill_viridis_c("Peak Mag 10") +
  ggtitle("PCA: Lag 1") +
  theme(plot.title = element_text(hjust = 0.5))


# now lag2
fviz_pca(lag2_pr) +
  scale_fill_viridis() +
  ggtitle("PCA: Lag 2")

fviz_pca_ind(lag2_pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = data_lag2$Peak_Mag_10, 
             col.ind = "black",
             addEllipses = F,
             label = "var",
             col.var = "black",
             repel = TRUE, show.legend=FALSE) +
  scale_fill_viridis_c("Peak Mag 10") +
  ggtitle("PCA: Lag 2") +
  theme(plot.title = element_text(hjust = 0.5))


