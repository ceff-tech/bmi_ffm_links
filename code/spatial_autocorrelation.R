# test for autocorrelation

library(tidyverse)
library(sf)
library(spdep)
library(purrr)

# load data
load("data_output/distinct_sites_spatial_auto.rda")


# convert to meters
csci_sites<- st_transform(csci_sites, 3310)
csci_sites$x <- st_coordinates(csci_sites)[,1]
csci_sites$y <- st_coordinates(csci_sites)[,2]


# Variogram ---------------------------------------------------------------

library(gstat)
csci_sp <- as_Spatial(csci_sites)
vario <- variogram(csci~1, data=csci_sp, cutoff=2e5)
plot(vario$dist, vario$gamma)


# Distance Weights --------------------------------------------------------


# get distance weights for 10km buffer
dist_10k_nb <- dnearneigh(csci_sites, 0, 10000)
plot(dist_10k_nb, st_coordinates(csci_sites), lwd=.2, col="blue", cex = .5)
# lots of isolates

# make sure every point has a neighbor
knn_matrix <- knearneigh(st_coordinates(csci_sites))
knn2nb(knn_matrix)
k10_nb <- knn2nb(knn_matrix)


# now every site has a neighbor
plot(k10_nb, st_coordinates(csci_sites), lwd=.5, col="blue", cex = .5)

# get max distance between point and nearest neigbor:
map_dbl(nbdists(k10_nb, st_coordinates(csci_sites)), ~sort(.x)) %>% 
  max() # max is 178km!

map_dbl(nbdists(k10_nb, st_coordinates(csci_sites)), ~sort(.x)) %>% 
  summary() # mean is stil 9-10 km

# get number of neighbors for each based on 10km
card(dist_10k_nb)
card(k10_nb)

# filter csci to non zeros
dist_10k_nozero <- purrr::keep(dist_10k_nb, ~ sum(.x) > 0)
csci_sites_filt <- csci_sites[purrr::map_lgl(dist_10k_nb, ~ sum(.x) > 0),]
dist_10k_filt <- dnearneigh(csci_sites_filt, 0, 10000)

# MORANS
# for every point connected
moran.test(csci_sites$csci, nb2listw(k10_nb)) 

# with 10 k threshold
moran.test(csci_sites_filt$csci, nb2listw(dist_10k_filt))

# sfExtras ----------------------------------------------------------------

#remotes::install_github("spatialanalysis/sfExtras")

library(sfExtras)
library(spdep)

# get rook
csci_rook <- st_rook(csci_sites)

# make nb:
rook_nb <- st_as_nb(csci_rook)

# check neighbors
rook_neighbors <- lengths(csci_rook)
mean(rook_neighbors)

centroid_coords <- st_centroid_coords(csci_sites)
plot(rook_nb, centroid_coords, lwd = 0.2, cex = 0.5, col = "blue")
