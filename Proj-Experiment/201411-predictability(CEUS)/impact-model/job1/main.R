#library(ggplot2)
library(nnet)
#library(reshape2) # dcast

source("../../exp.steps/functions.R")
source("../impact.model-2.R")
source("../personalization.evaulation.R")

load("../../data/checkin.poly.list.Rda")

# load configuration file
configs = read.csv("configs.txt",header = F)
city.index = configs[1,1]
k=configs[1,2]
f=configs[1,3]
Z=configs[1,4]


data.regress <- checkin.poly.list[[city.index]][,c("gid","user_id",
                                          "venue_id","cate_l1","hour","weekday",
                                          "ZIP","last.cate_l1","last.venue_id",
                                          "timestamps","time.interval")]
data.regress$weekday = as.factor(data.regress$weekday)
data.regress$hour2 = with(data.regress,as.factor(paste(hour,weekday)))
# some additional operations for sequential context
# add weight based on time interval
data.regress$wgt = 2 ^ ( -1 * data.regress$time.interval + 1)
# remove continuous check-ins by the same user (by changing the weight to 0)
data.regress$wgt = with(data.regress,ifelse(venue_id==last.venue_id, 0, wgt))
# deal with the empty values
data.regress$wgt = with(data.regress, ifelse(is.na(last.cate_l1)|time.interval>12,
                                             0.0005,wgt))
data.regress$last.cate_l1 = with(data.regress,
                             as.factor(ifelse(is.na(last.cate_l1)|time.interval>12,
                                              "Unknown",as.character(last.cate_l1))))

eva = pers.eva(data.regress, k=k, f=10^f, Z=Z)
eva$k = k
eva$f = 10^f
eva$Z = Z

save(eva,file=paste("../result/pers-eva",k,f,Z,".Rda",sep="-"))
