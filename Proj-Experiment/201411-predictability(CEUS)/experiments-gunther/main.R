setwd("D:\\GitRepos\\work\\experiments\\experiments_with_more_data")


library(scales)
library(ggplot2)
library(reshape2)
library(gridExtra)


source("fun/prepare.checkin.R")
source("fun/plots.R")

sink("log.txt", append = TRUE)

ppi=300 

# experiments with datasets in three cities
checkin.NY = prepare.checkin("data/csv-raw/NewYorkCity.csv",is.raw=TRUE, weather.data=NA, 
                             convert.time=TRUE, add.history=TRUE)
checkin.LA = prepare.checkin("data/csv-raw/LAUrbanized.csv",is.raw=TRUE, weather.data=NA, 
                             convert.time=TRUE, add.history=TRUE)
checkin.CH = prepare.checkin("data/csv-raw/ChicagoLand.csv",is.raw=TRUE, weather.data=NA, 
                             convert.time=TRUE, add.history=TRUE)

# save all in case
# save(checkin.NY, file="data/Rda-saved/checkin.global.NY.Rda")
# save(checkin.LA, file="data/Rda-saved/checkin.global.LA.Rda")
# save(checkin.CH, file="data/Rda-saved/checkin.global.CH.Rda")


# try plot them out
png("plots/checkin_points_cities.png",width=12*ppi,height=4*ppi,res=ppi)
grid.arrange(point.plot(checkin.NY,plot.title="New York City",
                        mapdir="data/shapefiles", maplayer="NYC_borough_boundaries_WGS84"),
             point.plot(checkin.LA,plot.title="Los Angeles Urbanized Area",
                        mapdir="data/shapefiles", maplayer="bounds_LA_City_WGS84"),
             point.plot(checkin.CH,plot.title="ChicagoLand Urbanized Area",
                        mapdir="data/shapefiles", maplayer="bounds_ChicagoCity_WGS84"),
             nrow=1, ncol=3, widths=c(1,1,1))
dev.off()

############################################################
## some global statistical analysis across the three cities
############################################################
# frequency domain
png("plots/checkin_freq_dom_cities.png",width=20*ppi,height=6*ppi,res=ppi)
grid.arrange(freq.plot(checkin.NY,plot.title="New York City"),
             freq.plot(checkin.LA,plot.title="Los Angeles Urbanized Area"),
             freq.plot(checkin.CH,plot.title="ChicagoLand Urbanized Area"),
             nrow=1, ncol=3, widths=c(1,1,1))
dev.off()

# temporal distribution
png("plots/checkin_distri_cities.png",width=20*ppi,height=6*ppi,res=ppi)
grid.arrange(time.distribution.plot(checkin.NY,plot.title="New York City"),
             time.distribution.plot(checkin.LA,plot.title="Los Angeles Urbanized Area"),
             time.distribution.plot(checkin.CH,plot.title="ChicagoLand Urbanized Area"),
             nrow=1, ncol=3, widths=c(1,1,1))
dev.off()

# temoral radial plot
png("plots/checkin_radial_cities.png",width=20*ppi,height=6*ppi,res=ppi)
par(mfrow=c(3,10))
time.radial.plot(checkin.NY)
time.radial.plot(checkin.LA)
time.radial.plot(checkin.CH)
dev.off()

# transition patterns
png("plots/transition_citites.png",width=18*ppi,height=12*ppi,res=ppi)
grid.arrange(transition.plot(checkin.NY,plot.title="New York City [Unscaled]"),
             transition.plot(checkin.LA,plot.title="Los Angeles Urbanized Area [Unscaled]"),
             transition.plot(checkin.CH,plot.title="ChicagoLand Urbanized Area [Unscaled]"),
             transition.plot(checkin.NY,plot.title="New York City [Scaled]",scaled=TRUE),
             transition.plot(checkin.LA,plot.title="Los Angeles Urbanized Area [Scaled]",scaled=TRUE),
             transition.plot(checkin.CH,plot.title="ChicagoLand Urbanized Area [Scaled]",scaled=TRUE),
             nrow=2, ncol=3, widths=c(1,1,1))
dev.off()