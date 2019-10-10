# Thu Aug 29 16:58:54 2019 ------------------------------

# Library -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)
library(lubridate)
library(mapview)
library(janitor)
library(CSCI)
library(BMIMetrics)

# get data:
load("data_output/bmi_stations_bugs_raw.rda")
load("data_output/sf_bmi_filt_distinct_reachwide_and_gages.rda")
#load("data/phab-rafi-15apr19.rda") # this takes a minute

# additional gages:
# USGS unimpaired flow gages in the Sierras:
# NF American at NF Dam (11427000)
# mainstem Cosumnes gage (1133500)
# Indian Crk (NF Feather) (11401500)
# Kern gage (11186001)
# NF Yuba blw Goodyears Bar (114130


# Same HUC12 --------------------------------------------------------------

# same watershed, are areas within 10% of each other, not necessarily by comid because very restrictive. Could also use simple prox buffer


# Step 03: Filter flow record ---------------------------------------------

# filter flow record to same year range using 223 gages (not just 93).
# add any natural observed gages with data that are "unimpaired" and current


# Step 04: Linking BMI with Gages -----------------------------------------
# use a set of passes:
# 1) is BMI point in same NHD stream segment as USGS gage?
# 2) is BMI point in same HUC12 as USGS gage?
# 3) Ted G has flow/area buffer code (to see if point in certain region of flow/area)?


# Step 05: Generate 24 FF Metrics for selected gages ----------------------

# pull this data somehow...from selected gages


# Step 06: Assess Relationships with PCA/tree method (RF or BRT) ----------


# Step 07: For targeted metrics, use bayesian GLM to better test/m --------

## reiterate, just because certain metrics are selected, doesn't mean others *aren't* important, but it does mean relationship could be used for functional approach (representative).
