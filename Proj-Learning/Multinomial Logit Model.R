################################################################################
# R Data Analysis Examples: Multinomial Logit Model
# ref: http://cran.at.r-project.org/web/packages/mlogit/vignettes/mlogit.pdf
# mlogit is a package for R which enables the estimation of the multinomial 
# logit models with individual and/or alternative specific variables. 
################################################################################

library("mlogit")
data("Fishing", package = "mlogit")
data("TravelMode", package = "AER")
data("Train", package = "mlogit")
# explanations of data mode: "wide" and "long" and inter-conversions
Fish <- mlogit.data(Fishing, shape = "wide", varying = 2:9, choice = "mode")
TM <- mlogit.data(TravelMode, choice = "choice", shape = "long",
                  chid.var = "individual", alt.var = "mode", drop.index = TRUE)
Tr <- mlogit.data(Train, shape = "wide", choice = "choice", varying = 4:11,
                  sep = "", alt.levels = c(1, 2), id = "id")