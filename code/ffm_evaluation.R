# FFM Evaluation


# Libraries ---------------------------------------------------------------

library(scales)
library(glue)
options(tidyverse.quiet = TRUE)
library(tidyverse) # load quietly
library(viridis) # colors
library(sf) # spatial operations
library(mapview) # web maps
mapviewOptions(fgb=FALSE)


# Data --------------------------------------------------------------------

# REF gages
gages_ref <- read_csv("data/usgs/gages_ref_223_period_record.csv") %>% 
  select(stream_class, gage, maxYr, minYr, YrRange) %>% 
  distinct(gage, .keep_all = TRUE) %>% 
  mutate(refgage="Ref")

# ffc gage data (alteration)
ffc_dat <- read_rds(file = url("https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_alteration.rds")) %>%
  mutate(gage=as.numeric(gageid),
         status=as.factor(status),
         alteration_type=as.factor(alteration_type))

# Tidy and Clean ----------------------------------------------------------

# get distinct gages (n=959)
ffc_gages <- ffc_dat %>% distinct(gageid, .keep_all=TRUE) %>% 
  mutate(gage=as.numeric(gageid)) %>% 
  left_join(., gages_ref, by="gage") %>% 
  mutate(refgage = case_when(
    is.na(refgage) ~ "Non-Ref",
    TRUE ~ refgage)) %>% 
  select(-c(metric:median_in_iqr))

# join w ref gages (FROM CEFF/FFM) data to get REF/non-ref
ffc_dat <- left_join(ffc_dat, gages_ref, by="gage") %>% 
  mutate(refgage = case_when(
    is.na(refgage) ~ "Non-Ref",
    TRUE ~ refgage))

# tally (24 metrics x 959 gages = 23016)
ffc_dat %>% group_by(refgage, status) %>%
  tally() %>% add_tally(n) # so only 21912, some NA's still

summary(ffc_dat$status)
summary(ffc_dat$alteration_type)
## so not enough data = NA in status_code.


# PLOT --------------------------------------------------------------------

# plot percent of sites with altered metric by FFM
(ffc_metric_count <- ffc_dat %>% 
   group_by(metric, refgage) %>% 
   #group_by(metric) %>% 
   count(name = "total_count"))

ffc_prcnt_alt <- ffc_dat %>% 
  group_by(metric, status, refgage) %>% # for facet by refgage
  #group_by(metric, status) %>% # for no refgage facet
  tally() %>% 
  left_join(., ffc_metric_count) %>% 
  mutate(prop_n = n/total_count)

# plot
ggplot() + geom_col(data=ffc_prcnt_alt, aes(x=metric, y=prop_n, fill=status)) +
  theme_classic(base_family = "Roboto Condensed") +
  scale_y_continuous(labels=percent) +
  scale_fill_viridis_d(direction = -1) +
  labs(title="Proportion of Gages by Alteration Status",
       subtitle="Based on 959 gages with sufficient data for FFM R Calculator",
       x="", y="Proportion of Gages Altered") +
  theme(axis.text.x = element_text(angle=270, hjust = 0.1, vjust=0.05)) +
  facet_wrap(vars(refgage))

ggsave(filename = "figs/prop_gages_by_alt_status_faceted.png", width = 11, height = 8.5, dpi=300)

