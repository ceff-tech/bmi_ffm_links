library(mapview)
library(mapedit)
library(sf)
library(dplyr)


# Make a line or some points ----------------------------------------------

load("data/example_pts.rda")

m1 <- mapview(sel_bmi_gages)

m2 <- mapedit::editMap(m1)
mpts <- m2$finished

save(mpts, file = "data/example_pts.rda")

# fix up dataset
mpts <- mpts %>% 
  mutate(id = 1:nrow(.))

# Now Calc Nearest --------------------------------------------------------

mapview(mpts)

# identify start point
mpts_start <- filter(mpts, id == 1) # has to SF object


#mapview(mpts) + mapview(mpts_start, color="red", cex=5)

# calc nearest to start
#pt_list <- st_distance(mpts_start, mpts, by_element = TRUE)
#class(pt_list)
#class(pt_list %>% as.vector())


# add this directly to our dataframe
mpts2 <- mpts %>% 
  mutate(
    #dist_to_start_units = st_distance(mpts_start, ., by_element = TRUE),
    dist_to_start_m = as.vector(st_distance(mpts_start, ., by_element = TRUE)))


# Fake Plot ---------------------------------------------------------------

library(ggplot2)

ggplot() + geom_tile(data=mpts2, aes(x=id, y=dist_to_start_m, fill=X_leaflet_id))
