library(reshape2)
library(ggplot2)
library(nnet)

source("../functions.R")
# name patterns
city.names = c("Chi","LA","NYC")
k.array = c("0.8","0.9")

#############################
# load all data

# load efficiency
# efficiency = do.call(rbind,lapply(k.array,function(k){
#     do.call(rbind,lapply(city.names,function(city){
#         filename = paste("../data/testmodels/efficiency_",city,"_",k,".Rda",sep="")
#         #print(filename)
#         load(filename)
#         colnames(efficiency)=c("duration","memory")
#         efficiency$city = city
#         efficiency$k.threshold = k
#         efficiency$model.type = factor(rownames(efficiency),
#                     levels=c("null","unclustered hour",
#                               "hour only","unclustered zip",
#                               "space only","hour+space",
#                               "hour*space"))
#         efficiency
#     }))
# }))
# save(efficiency,file="../data/testmodels/efficiency.Rda")
load("../data/testmodels/efficiency.Rda")


# load original data for the predicting models
# checkin.city.st = do.call(c,lapply(k.array,function(k){
#     lapply(city.names,function(city){
#         filename = paste("../data/checkin.",city,".st_",k,".Rda",sep="")
#         load(filename)
#         list("city"=city,"k.threshold"=k,"checkin.city.st"=checkin.city.st)
#     })
# }))
# save(checkin.city.st,file="../data/checkin.city.st.Rda")
load("../data/checkin.city.st.Rda")

# load all kinds of models
# models <- do.call(c,lapply(k.array,function(k){
#     lapply(city.names,function(city){
#         load(paste("../data/testmodels/null_",city,"_",k,".Rda",sep=""))
#         load(paste("../data/testmodels/unc_hour_",city,"_",k,".Rda",sep=""))
#         load(paste("../data/testmodels/hour_",city,"_",k,".Rda",sep=""))
#         load(paste("../data/testmodels/unc_sp_",city,"_",k,".Rda",sep=""))
#         load(paste("../data/testmodels/sp_",city,"_",k,".Rda",sep=""))
#         load(paste("../data/testmodels/hour_sp_",city,"_",k,".Rda",sep=""))
#         load(paste("../data/testmodels/hour_sp_int_",city,"_",k,".Rda",sep=""))
#         list("city"=city,"k.threshold"=k,
#              "null"=mnFit.null,
#              "unc.hour"=mnFit.hour.unclustered,
#              "hour"=mnFit.hour,
#              "unc.sp"=mnFit.zip.unclustered,
#              "sp"=mnFit.zip,
#              "st"=mnFit,
#              "st.int"=mnFit.interaction)
#     })
# }))
# save(models,file="../data/testmodels/models.Rda")
load("../data/testmodels/models.Rda")

######################
# model evaluation and comparison

# self evaluation
performance <- do.call(rbind,lapply(1:6,function(data.index){
    checkin.city.data = checkin.city.st[[data.index]]
    checkin.raw = checkin.city.data$checkin.city.st
    city = checkin.city.data$city
    k = checkin.city.data$k.threshold
    
    model.city = models[[data.index]]
    null = model.city$null
    unc.hour = model.city$unc.hour
    hour = model.city$hour
    unc.sp = model.city$unc.sp
    sp = model.city$sp
    st = model.city$st
    st.int = model.city$st.int
    
    # step 1: prediction accurarcy
    accurarcy <- rbind("null"=model.prediction(null,checkin.raw)[[2]],
          "unclustered hour"=model.prediction(unc.hour,checkin.raw)[[2]],
          "hour only"=model.prediction(hour,checkin.raw)[[2]],
          "unclustered zip"=model.prediction(unc.sp,checkin.raw)[[2]],
          "space only"=model.prediction(sp,checkin.raw)[[2]],
          "hour+space"=model.prediction(st,checkin.raw)[[2]],
          "hour*space"=model.prediction(st.int,checkin.raw)[[2]])
    
    # step 2: chi-square model fitness test for each model
    fitness <- rbind("null"=model.fitness(null),
                     "unclustered hour"=model.fitness(unc.hour),
                     "hour only"=model.fitness(hour),
                     "unclustered zip"=model.fitness(unc.sp),
                     "space only"=model.fitness(sp),
                     "hour+space"=model.fitness(st),
                     "hour*space"=model.fitness(st.int))
    
    performance <- cbind(accurarcy,fitness)
    performance$city = city
    performance$k.threshold = k
    performance$model.type = factor(rownames(performance),
                                    levels=c("null","unclustered hour",
                                             "hour only","unclustered zip",
                                             "space only","hour+space",
                                             "hour*space"))
    performance
}))

# combine performance with efficiency
perf.effi = merge(performance[,c(1,2,8:10)],efficiency)
performance.melt = melt(perf.effi, 
    id.vars=c("city","k.threshold","model.type"))
png("model.perf.png",width=3000,height=1800,res=300)
ggplot(performance.melt)+
    geom_line(aes(x=model.type,y=value,group=paste(city,k.threshold),
                  linetype=k.threshold,color=city),
              size=1)+
    facet_wrap(~variable,scales="free_y")+
    theme_bw() %+replace%
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
dev.off()

# model comparison (likelihood test & pseudo-R2)
# compare the effect of k
k.effect <- rbind(
    "Chi.null"=model.comparison(models[[1]]$null,models[[4]]$null),
    "Chi.unc.hour"=model.comparison(models[[1]]$unc.hour,models[[4]]$unc.hour),
    "Chi.hour"=model.comparison(models[[1]]$hour,models[[4]]$hour),
    "Chi.unc.space"=model.comparison(models[[1]]$unc.sp,models[[4]]$unc.sp),
    "Chi.space"=model.comparison(models[[1]]$sp,models[[4]]$sp),
    "Chi.st"=model.comparison(models[[1]]$st,models[[4]]$st),
    "Chi.st.int"=model.comparison(models[[1]]$st.int,models[[4]]$st.int),
    "LA.null"=model.comparison(models[[2]]$null,models[[5]]$null),
    "LA.unc.hour"=model.comparison(models[[2]]$unc.hour,models[[5]]$unc.hour),
    "LA.hour"=model.comparison(models[[2]]$hour,models[[5]]$hour),
    "LA.unc.space"=model.comparison(models[[2]]$unc.sp,models[[5]]$unc.sp),
    "LA.space"=model.comparison(models[[2]]$sp,models[[5]]$sp),
    "LA.st"=model.comparison(models[[2]]$st,models[[5]]$st),
    "LA.st.int"=model.comparison(models[[2]]$st.int,models[[5]]$st.int),
    "NYC.null"=model.comparison(models[[3]]$null,models[[6]]$null),
    "NYC.unc.hour"=model.comparison(models[[3]]$unc.hour,models[[6]]$unc.hour),
    "NYC.hour"=model.comparison(models[[3]]$hour,models[[6]]$hour),
    "NYC.unc.space"=model.comparison(models[[3]]$unc.sp,models[[6]]$unc.sp),
    "NYC.space"=model.comparison(models[[3]]$sp,models[[6]]$sp),
    "NYC.st"=model.comparison(models[[3]]$st,models[[6]]$st),
    "NYC.st.int"=model.comparison(models[[3]]$st.int,models[[6]]$st.int))[,c(5:7,1:4)]

cluster.effect <- rbind(
    "Chi.0.8.hour"=model.comparison(models[[1]]$hour,models[[1]]$unc.hour),
    "Chi.0.9.hour"=model.comparison(models[[4]]$hour,models[[4]]$unc.hour),
    "Chi.0.8.space"=model.comparison(models[[1]]$sp,models[[1]]$unc.sp),
    "Chi.0.9.space"=model.comparison(models[[4]]$sp,models[[4]]$unc.sp),
    "LA.0.8.hour"=model.comparison(models[[2]]$hour,models[[2]]$unc.hour),
    "LA.0.9.hour"=model.comparison(models[[5]]$hour,models[[5]]$unc.hour),
    "LA.0.8.space"=model.comparison(models[[2]]$sp,models[[2]]$unc.sp),
    "LA.0.9.space"=model.comparison(models[[5]]$sp,models[[5]]$unc.sp),
    "NYC.0.8.hour"=model.comparison(models[[3]]$hour,models[[3]]$unc.hour),
    "NYC.0.9.hour"=model.comparison(models[[6]]$hour,models[[6]]$unc.hour),
    "NYC.0.8.space"=model.comparison(models[[3]]$sp,models[[3]]$unc.sp),
    "NYC.0.9.space"=model.comparison(models[[6]]$sp,models[[6]]$unc.sp))[,c(5:7,1:4)]

interaction.effect <-rbind(
    "Chi.0.8"=model.comparison(models[[1]]$st,models[[1]]$st.int),
    "Chi.0.9"=model.comparison(models[[4]]$st,models[[4]]$st.int),
    "LA.0.8"=model.comparison(models[[2]]$st,models[[2]]$st.int),
    "LA.0.9"=model.comparison(models[[5]]$st,models[[5]]$st.int),
    "NYC.0.8"=model.comparison(models[[3]]$st,models[[3]]$st.int),
    "NYC.0.9"=model.comparison(models[[6]]$st,models[[6]]$st.int))[,c(5:7,1:4)]

predictor.effect <- rbind(
    "Chi.0.8.hour"=model.comparison(models[[1]]$null,models[[1]]$hour),
    "Chi.0.8.space"=model.comparison(models[[1]]$null,models[[1]]$sp),
    "Chi.0.8.st"=model.comparison(models[[1]]$null,models[[1]]$st),
    "Chi.0.8.st.int"=model.comparison(models[[1]]$null,models[[1]]$st.int),
    
    "Chi.0.9.hour"=model.comparison(models[[4]]$null,models[[4]]$hour),
    "Chi.0.9.space"=model.comparison(models[[4]]$null,models[[4]]$sp),
    "Chi.0.9.st"=model.comparison(models[[4]]$null,models[[4]]$st),
    "Chi.0.9.st.int"=model.comparison(models[[4]]$null,models[[4]]$st.int),
    
    "LA.0.8.hour"=model.comparison(models[[2]]$null,models[[2]]$hour),
    "LA.0.8.space"=model.comparison(models[[2]]$null,models[[2]]$sp),
    "LA.0.8.st"=model.comparison(models[[2]]$null,models[[2]]$st),
    "LA.0.8.st.int"=model.comparison(models[[2]]$null,models[[2]]$st.int),
    
    "LA.0.9.hour"=model.comparison(models[[5]]$null,models[[5]]$hour),
    "LA.0.9.space"=model.comparison(models[[5]]$null,models[[5]]$sp),
    "LA.0.9.st"=model.comparison(models[[5]]$null,models[[5]]$st),
    "LA.0.9.st.int"=model.comparison(models[[5]]$null,models[[5]]$st.int),
    
    "NYC.0.8.hour"=model.comparison(models[[3]]$null,models[[3]]$hour),
    "NYC.0.8.space"=model.comparison(models[[3]]$null,models[[3]]$sp),
    "NYC.0.8.st"=model.comparison(models[[3]]$null,models[[3]]$st),
    "NYC.0.8.st.int"=model.comparison(models[[3]]$null,models[[3]]$st.int),
    
    "NYC.0.9.hour"=model.comparison(models[[6]]$null,models[[6]]$hour),
    "NYC.0.9.space"=model.comparison(models[[6]]$null,models[[6]]$sp),
    "NYC.0.9.st"=model.comparison(models[[6]]$null,models[[6]]$st),
    "NYC.0.9.st.int"=model.comparison(models[[6]]$null,models[[6]]$st.int))[,c(5:7,1:4)]
