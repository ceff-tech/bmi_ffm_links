# FFM Evaluation
# Filter Ref gages to assess best FF metrics
# to refine metrics that need to be run
# R. Peek 2021


# Libraries ---------------------------------------------------------------

library(scales)
library(glue)
options(tidyverse.quiet = TRUE)
library(tidyverse) # load quietly
library(viridis) # colors
library(janitor)

# GET DATA ----------------------------------------------------------------

# REF gages
gages_ref <- read_csv("data/usgs/gages_ref_223_period_record.csv") %>% 
  select(stream_class, gage, maxYr, minYr, YrRange) %>% 
  distinct(gage, .keep_all = TRUE) %>% 
  mutate(refgage="Ref")

# ffc data (alteration status)
ffc_alt <- read_rds(file = url("https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_alteration.rds")) %>%
  mutate(gage=as.numeric(gageid),
         status=as.factor(status),
         alteration_type=as.factor(alteration_type))

# import observed FFM data for period of record
ffc_obs <- read_rds(file = url("https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_ffc_percentiles.rds")) %>%
  mutate(gage=as.numeric(gageid))

# import predicted percentiles for period of record
ffc_pred <- read_rds(file=url("https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_predicted_percentiles.rds")) %>% 
  mutate(gage=as.numeric(gageid))

# TIDY AND CLEAN ----------------------------------------------------------

# get distinct gages (n=959)
ffc_gages <- ffc_alt %>% distinct(gageid, .keep_all=TRUE) %>% 
  mutate(gage=as.numeric(gageid)) %>% 
  left_join(., gages_ref, by="gage") %>% 
  mutate(refgage = case_when(
    is.na(refgage) ~ "Non-Ref",
    TRUE ~ refgage)) %>% 
  select(-c(metric:median_in_iqr))

# join FFC_DAT w ref gages (FROM CEFF/FFM) data to get REF/non-ref class
ffc_alt <- left_join(ffc_alt, gages_ref, by="gage") %>% 
  mutate(refgage = case_when(
    is.na(refgage) ~ "Non-Ref",
    TRUE ~ refgage))

# tally (24 metrics x 959 gages = 23016)
ffc_alt %>% group_by(refgage, status) %>%
  tally(name="total_status") %>% # tally by status
  # tally by ref gage class
  add_tally(total_status, name="total_of_refgage_class") # so only 21912, some NA's still

summary(ffc_alt$status)
summary(ffc_alt$status_code)
## so not_enough_data = NA in status_code
summary(ffc_alt$alteration_type)


# CALC PROPORTION GAGES ALTERED PER METRIC ----------------

# count total records by metric & refgage
(ffm_metric_count <- ffc_alt %>% 
   group_by(metric, refgage) %>% 
   count(name = "total_count"))

# calculate proportion of gages/total for each status class
ffm_prcnt_alt <- ffc_alt %>% 
  group_by(metric, status, refgage) %>% # for facet by refgage
  tally() %>% 
  left_join(., ffm_metric_count) %>% 
  mutate(prop_n = n/total_count)


## Plot: ALT STATUS -------------------------------------------------------

# plot faceted by REF/NON-REF type
ggplot() + geom_col(data=ffm_prcnt_alt, aes(x=metric, y=prop_n, fill=status)) +
  theme_classic(base_family = "Roboto Condensed") +
  scale_y_continuous(labels=percent) +
  scale_fill_viridis_d(direction = -1) +
  labs(title="Proportion of Gages by Alteration Status",
       subtitle="Based on 959 gages with sufficient data for FFM R Calculator",
       x="", y="Proportion of Gages Altered") +
  theme(axis.text.x = element_text(angle=270, hjust = 0.1, vjust=0.05)) +
  facet_wrap(vars(refgage))

#ggsave(filename = "figs/prop_gages_by_alt_status_faceted.png", width = 11, height = 8.5, dpi=300)

# FILTERING FFM ---------------------------------------------

## STEP 1: <10% of Ref gages altered ------------------------

# Filter to only FFM with < 10% of ref gages == altered, excluding PEAK FLOW METRICS 

ffm_prcnt_alt_keep <- ffm_prcnt_alt %>% 
  filter(refgage == "Ref") %>% # keep only REF
  filter(status == "likely_altered") %>% 
  filter(prop_n < 0.1) %>% 
  filter(!grepl("^Peak", metric))

# FILTER 1 METRIC LIST
filt_less_than_10_perc_metrics <- ffm_prcnt_alt_keep$metric

## STEP 2: <15% of Ref gages peak flows altered -------------

# Filter to only Peak flow metris w < 15% of ref gages == altered
ffm_prcnt_alt_keep_peak <- ffm_prcnt_alt %>% 
  filter(refgage == "Ref") %>% # keep only REF
  filter(status == "likely_altered") %>% 
  filter(grepl("^Peak", metric)) %>% 
  filter(prop_n < 0.15)

# FILTER 1 METRIC LIST
filt_less_than_15_peak <- ffm_prcnt_alt_keep_peak$metric

## STEP 3: <25% of Ref gages are indeterminant -----------

# <25% of ref gages are indeterminate for that metric

ffm_prcnt_alt_keep_indet <- ffm_prcnt_alt %>% 
  filter(refgage == "Ref") %>% # keep only REF
  filter(status == "indeterminate") %>% 
  filter(prop_n < 0.25)

# FILTER 1 METRIC LIST
filt_less_than_25_indeterm <- ffm_prcnt_alt_keep_indet$metric

### WRITE IT OUT
write_csv(ffm_prcnt_alt_keep_indet, file = "data_output/06_filtered_ffm_based_on_ref_prop.csv")

# ADD PLOTS/SUMMARIES ---------------------------------------
# Calc REF/NON-REF w medianIQR ------------------------------

summary(ffc_alt$median_in_iqr)

# calculate proportion of gages/total
ffm_prcnt_alt2 <- ffc_alt %>% 
  group_by(metric, median_in_iqr, refgage) %>% # for facet by refgage
  tally() %>% 
  left_join(., ffm_metric_count) %>% 
  mutate(prop_n = n/total_count)

## Plot: by median IQR ---------------------------------------------------

# plot faceted by REF/NON-REF type
ggplot() + geom_col(data=ffm_prcnt_alt2, aes(x=metric, y=prop_n, fill=median_in_iqr)) +
  theme_classic(base_family = "Roboto Condensed") +
  scale_y_continuous(labels=percent) +
  scale_fill_viridis_d(direction = -1, na.value="gray50") +
  labs(title="Proportion of Gages by Median in IQR",
       subtitle="Based on 959 gages with sufficient data for FFM R Calculator",
       x="", y="Proportion of Gages Altered") +
  theme(axis.text.x = element_text(angle=270, hjust = 0.1, vjust=0.05)) +
  facet_wrap(vars(refgage))

# ggsave(filename = "figs/prop_gages_by_medIQR_faceted.png", width = 11, height = 8.5, dpi=300)

# Calc REF/NON-REF by ALT type ------------------------------

# calculate proportion of gages/total for each alt type
ffm_prcnt_alt3 <- ffc_alt %>% 
  group_by(metric, alteration_type, refgage) %>% # for facet by refgage
  tally() %>% 
  left_join(., ffm_metric_count) %>% 
  mutate(prop_n = n/total_count,
         # fix order of alt_type
         alteration_type = forcats::fct_relevel(alteration_type, "high", "low", 
                                                "early", "late", 
                                                "none_found", 
                                                "undeterminable", "unknown"))

## Plots: by Alt Type ---------------------------------------------------

# plot faceted by REF/NON-REF type
ggplot() + geom_col(data=ffm_prcnt_alt3, aes(x=metric, y=prop_n, fill=alteration_type), alpha=0.8) +
  theme_classic(base_family = "Roboto Condensed") +
  scale_y_continuous(labels=percent) +
  scale_fill_manual(values=c("high"="#E41A1C", "low"="#377EB8", "early"="#4DAF4A", "late"="#984EA3", "none_found"="gray80", "undeterminable"="gray40","unknown"="gray20")) +
  labs(title="Proportion of Gages by Alteration Type",
       subtitle="Based on 959 gages with sufficient data for FFM R Calculator",
       x="", y="Proportion of Gages Altered") +
  theme(axis.text.x = element_text(angle=270, hjust = 0.1, vjust=0.05)) +
  facet_wrap(vars(refgage))

# ggsave(filename = "figs/prop_gages_by_alt_type_faceted.png", width = 11, height = 8.5, dpi=300)

