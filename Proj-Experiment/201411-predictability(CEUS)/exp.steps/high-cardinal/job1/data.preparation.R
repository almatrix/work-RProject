# load raw data
load("../../data/checkin.poly.list_nohistory.Rda")
# load configuration file
configs = read.csv("configs.txt",header = F)
# load the functions
source("../../functions.R")
# load the libraries
library(nnet)

# configurations
city.index = configs[1,1]
city.sp.attr = c("ZIP","Zip_Num","POSTAL")[city.index]
city.name = c("Chi","LA","NYC")[city.index]

k.threshold = configs[1,2]

# subset the required columns
checkin.city = checkin.poly.list[[city.index]][,c("user_id","cate_l1","cate_l2",
                                                  "venue_id","hour", city.sp.attr,
                                                  "timestamps")]
# unify the column name for spatial attribute for all cities
colnames(checkin.city)[which(colnames(checkin.city)==city.sp.attr)]="ZIP"
checkin.city$ZIP=as.factor(checkin.city$ZIP)

checkin.city.t <- merge(checkin.city,
                    reduce.dimensions(checkin.city,"hour", "cate_l1",
                                      from=1,to=length(unique(checkin.city$hour))-1,
                                      by=1,threshold=k.threshold),
                    by.x="hour",by.y="condition",all.x=T)
checkin.city.st <- merge(checkin.city.t,
                     reduce.dimensions(checkin.city,"ZIP", "cate_l1",
                                       from=1,to=length(unique(checkin.city$ZIP))-3,
                                       by=1,threshold=k.threshold),
                     by.x="ZIP",by.y="condition",all.x=T)
checkin.city.st <- checkin.city.st[,c(1:8,10)]
colnames(checkin.city.st)[c(8,9)]<-c("cluster.hour","cluster.ZIP")

# save the data for further use
save(checkin.city.st,file=paste("../../data/checkin.",city.name,".st_",k.threshold,".Rda",sep=""))


##############################
sink("log.txt")
# global models (test predictors)
efficiency = data.frame()

print(Sys.time())
print("********************\n(1)Null Model:")
tm.init = watch.time.memory()
mnFit.null <- multinom(cate_l1 ~ 1, data=checkin.city.st)
efficiency <- rbind(efficiency, 
	"null"=watch.time.memory(tm.init[1],tm.init[2]) )
save(mnFit.null,file=paste("../../data/testmodels/null_",city.name,"_",k.threshold,".Rda",sep=""))
save(efficiency,file=paste("../../data/testmodels/efficiency_",city.name,"_",k.threshold,".Rda",sep=""))

print(Sys.time())
print("********************\n(2)Unclustered Hour Model:")
tm.init = watch.time.memory() 
mnFit.hour.unclustered <- multinom(cate_l1 ~ hour, maxit = 1000, data=checkin.city.st)
efficiency <- rbind(efficiency, 
	"unclustered hour"=watch.time.memory(tm.init[1],tm.init[2]) )
save(mnFit.hour.unclustered,file=paste("../../data/testmodels/unc_hour_",city.name,"_",k.threshold,".Rda",sep=""))
save(efficiency,file=paste("../../data/testmodels/efficiency_",city.name,"_",k.threshold,".Rda",sep=""))

print(Sys.time())
print("********************\n(3)Hour Model:")
tm.init = watch.time.memory()
mnFit.hour <- multinom(cate_l1 ~ cluster.hour, maxit = 1000, data=checkin.city.st)
efficiency <- rbind(efficiency, 
	"hour only"=watch.time.memory(tm.init[1],tm.init[2]) )
save(mnFit.hour,file=paste("../../data/testmodels/hour_",city.name,"_",k.threshold,".Rda",sep=""))
save(efficiency,file=paste("../../data/testmodels/efficiency_",city.name,"_",k.threshold,".Rda",sep=""))

print(Sys.time())
print("********************\n(4)Unclustered Spatial Model:")
tm.init = watch.time.memory()
mnFit.zip.unclustered <- multinom(cate_l1 ~ ZIP, maxit = 1000, MaxNWts = 3000, data=checkin.city.st)
efficiency <- rbind(efficiency, 
	"unclustered zip"=watch.time.memory(tm.init[1],tm.init[2]) )
save(mnFit.zip.unclustered,file=paste("../../data/testmodels/unc_sp_",city.name,"_",k.threshold,".Rda",sep=""))
save(efficiency,file=paste("../../data/testmodels/efficiency_",city.name,"_",k.threshold,".Rda",sep=""))

print(Sys.time())
print("********************\n(5)Spatial Model:")
tm.init = watch.time.memory()
mnFit.zip <- multinom(cate_l1 ~ cluster.ZIP, maxit = 1000, data=checkin.city.st)
efficiency <- rbind(efficiency, 
  	"space only"=watch.time.memory(tm.init[1],tm.init[2]) )
save(mnFit.zip,file=paste("../../data/testmodels/sp_",city.name,"_",k.threshold,".Rda",sep=""))
save(efficiency,file=paste("../../data/testmodels/efficiency_",city.name,"_",k.threshold,".Rda",sep=""))

print(Sys.time())
print("********************\n(6)S+T Model:")
tm.init = watch.time.memory()
mnFit <- multinom(cate_l1 ~ cluster.hour + cluster.ZIP, data=checkin.city.st,
		  MaxNWts=3000, maxit = 1000)
efficiency <- rbind(efficiency, 
	"hour+space"=watch.time.memory(tm.init[1],tm.init[2]) )
save(mnFit,file=paste("../../data/testmodels/hour_sp_",city.name,"_",k.threshold,".Rda",sep=""))
save(efficiency,file=paste("../../data/testmodels/efficiency_",city.name,"_",k.threshold,".Rda",sep=""))

print(Sys.time())
print("********************\n(7)S*T Model:")
tm.init = watch.time.memory()
mnFit.interaction <- multinom(cate_l1 ~ cluster.hour * cluster.ZIP, MaxNWts = 3000, maxit = 1000, data=checkin.city.st)
efficiency <- rbind(efficiency, 
	"hour*space"=watch.time.memory(tm.init[1],tm.init[2]) )
save(mnFit.interaction,file=paste("../../data/testmodels/hour_sp_int_",city.name,"_",k.threshold,".Rda",sep=""))
save(efficiency,file=paste("../../data/testmodels/efficiency_",city.name,"_",k.threshold,".Rda",sep=""))


