################################################################################
# Geographically Weighted Regression
# instead of one global coefficient for each independent variable, coefficients
# are able to vary according to space, i.e. locally. The spatial variation in 
# coefficients can reveal interesting patterns which otherwise would be masked.
# ref: http://rstudio-pubs-static.s3.amazonaws.com/11020_1834645a316245e684e95370c5f46919.html
################################################################################

################################################################################
# many packages for GWR
# e.g.: spgwr, GWmodel, gwrr
################################################################################

library(spgwr)
library(ggplot2)
library(maptools)


## plot map
## shapefile requirement: seperate parts; WGS 84 as CRS
boroughs <- readShapePoly("D:/Experiments/foursquare checkin data/shapefile/boundaries/NYC_borough_boundaries_seperated_4326.shp")
boroughoutline <- fortify(boroughs)
ggplot(data = boroughoutline ) + 
    geom_path(aes(long, lat, group = id), colour = "grey")