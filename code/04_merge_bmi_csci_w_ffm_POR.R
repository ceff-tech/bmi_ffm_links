# 04 Merge BMI CSCI Data with Flow Data for Period of Record
## R. Peek

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(tidylog)


# LOAD DATA ---------------------------------------------------------------


# bmi data:
### bmi_final_dat (all data)
### bmi_final_dat_trim (all data for selected site pairs btwn Jun-Sep)

bmi_final_trim <- read_rds("data_output/02c_selected_final_bmi_csci_dat_trim.rds") 

# ALL GAGES W FFC DATA
# read from ffm_comparison repo: https://github.com/ryanpeek/ffm_comparison

ffc_dat <- read_rds(file = url("https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_alteration.rds")) #%>% 

# trim data to the USGS stations with BMI CSCI data
ffc_trim <- ffc_dat %>% filter(gageid %in% bmi_final_trim$site_id)

# how many gages distinct?
ffc_trim %>% distinct(gageid, .keep_all=TRUE) %>% tally()# n=209


# JOIN DATA ---------------------------------------------------------------

# join together selected csci data with ffm alteration status 
bmi_csci_por_trim <-  inner_join(st_drop_geometry(bmi_final_trim), ffc_trim,
                            by=c("site_id"="gageid")) %>%   # n=16080
  distinct(StationCode, SampleID, metric, site_id, .keep_all=TRUE) %>%
  rename(comid_gage=comid.x, comid_ffc=comid.y)
  # n=15936

# see how many distinct sites
length(unique(bmi_csci_por_trim$site_id)) #Gages (n=209)
length(unique(bmi_csci_por_trim$StationCode)) # BMI Stations (n=246)

# how many of each gage type
bmi_csci_por_trim %>%
  dplyr::distinct(site_id, .keep_all=TRUE) %>% 
  group_by(CEFF_type) %>%  tally() 
# ALT = 156, REF = 53, same as originally

# VISUALIZE ---------------------------------------------------------------

library(ggthemes)

hist(month(bmi_csci_por_trim$sampledate))

# function to get data
stat_box_data <- function(y, upper_limit = max(bmi_csci_por_trim$csci, na.rm = TRUE)) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}


# plot CSCI w/ NAs
ggplot(data=bmi_csci_por_trim %>% 
         filter(status!="not_enough_data"), 
       aes(x=CEFF_type, y=csci)) + 
  geom_boxplot(aes(fill=status), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text", cex=3, hjust=1, vjust=0.9) +
  labs(y="CSCI", x="CEFF Gage Type", subtitle="CSCI Score by FFC Alteration Status")+
  theme_bw(base_family = "Roboto Condensed") + facet_grid(.~status) +
  scale_fill_colorblind()

ggsave(filename = "figs/04_csci_scores_by_ffc_alteration_status_gage_type.png", height = 8, width = 11, units = "in", dpi=300)

# Export Joined POR Data ---------------------------------------------

# save the bmi_csci_por data
write_rds(bmi_csci_por_trim, file = "data_output/04_selected_csci_ffm_por_trim.rds")
