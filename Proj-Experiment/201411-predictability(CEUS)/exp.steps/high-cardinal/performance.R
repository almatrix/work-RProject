library(reshape2)
library(ggplot2)
library(nnet)

source("../functions.R")
# name patterns
city.names = c("Chi","LA","NYC")
k.array = c("0.8","0.9")
#data.type = c("efficiency","null","unc_hour","hour","unc_sp","sp","hour_sp","hour_sp_int")

#############################
# load all data

# load efficiency
efficiency = do.call(rbind,lapply(k.array,function(k){
    do.call(rbind,lapply(city.names,function(city){
        filename = paste("../data/testmodels/efficiency_",city,"_",k,".Rda",sep="")
        #print(filename)
        load(filename)
        colnames(efficiency)=c("duration","memory")
        efficiency$city = city
        efficiency$k.threshold = k
        efficiency$model.type = as.factor(rownames(efficiency))
        efficiency
    }))
}))
save(efficiency,file="../data/testmodels/efficiency.Rda")
load("../data/testmodels/efficiency.Rda")

# load original data for the predicting models
checkin.city.st = do.call(c,lapply(k.array,function(k){
    lapply(city.names,function(city){
        filename = paste("../data/checkin.",city,".st_",k,".Rda",sep="")
        load(filename)
        list("city"=city,"k.threshold"=k,"checkin.city.st"=checkin.city.st)
    })
}))
save(checkin.city.st,file="../data/checkin.city.st.Rda")
load("../data/checkin.city.st.Rda")

# load all kinds of models
models <- do.call(c,lapply(k.array,function(k){
    lapply(city.names,function(city){
        load(paste("../data/testmodels/null_",city,"_",k,".Rda",sep=""))
        load(paste("../data/testmodels/unc_hour_",city,"_",k,".Rda",sep=""))
        load(paste("../data/testmodels/hour_",city,"_",k,".Rda",sep=""))
        load(paste("../data/testmodels/unc_sp_",city,"_",k,".Rda",sep=""))
        load(paste("../data/testmodels/sp_",city,"_",k,".Rda",sep=""))
        load(paste("../data/testmodels/hour_sp_",city,"_",k,".Rda",sep=""))
        load(paste("../data/testmodels/hour_sp_int_",city,"_",k,".Rda",sep=""))
        list("city"=city,"k.threshold"=k,
             "null"=mnFit.null,
             "unc.hour"=mnFit.hour.unclustered,
             "hour"=mnFit.hour,
             "unc.sp"=mnFit.zip.unclustered,
             "sp"=mnFit.zip,
             "st"=mnFit,
             "st.int"=mnFit.interaction)
    })
}))
save(models,file="../data/testmodels/models.Rda")
load("../data/testmodels/models.Rda")

######################
# model evaluation and comparison

# self evaluation
performance <- do.call(rbind,lapply(1:6,function(data.index){
    checkin.raw = raw.data[[data.index]]
    null = null.models[[data.index]]
    unc.hour = unc.hour.models[[data.index]]
    hour = hour.models[[data.index]]
    unc.sp = unc.sp.models[[data.index]]
    sp = sp.models[[data.index]]
    st = hour.sp.models[[data.index]]
    st.int = hour.sp.int.models[[data.index]]
    
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
    performance$city = city.names[(data.index -1) %% 3 + 1]
    performance$k.threshold = k.array[ceiling(data.index / 3 )]
    performance$model.type = as.factor(rownames(performance))
    performance
}))

# combine performance with efficiency
performance.2 <- merge(performance[,c(1,2,8:10)],efficiency)
performance.melt = melt(performance.2, id.vars=c("city","k.threshold","model.type"))
ggplot(performance.melt)+
    geom_line(aes(x=model.type,y=value,group=paste(city,k.threshold),
                  linetype=k.threshold,color=city))+
    facet_wrap(~variable,scales="free_y")

# step 3: model comparison (likelihood test & pseudo-R2)
comparison <- rbind(
    "clustering effect t"=model.comparison(mnFit.hour,mnFit.hour.unclustered),
    "clustering effect s"=model.comparison(mnFit.hour,mnFit.zip.unclustered),
    "hour to null"=model.comparison(mnFit.null,mnFit.hour),
    "space to null"=model.comparison(mnFit.null,mnFit.zip),
    "hour+space to null"=model.comparison(mnFit.null,mnFit),
    "hour*space to null"=model.comparison(mnFit.null,mnFit.interaction),
    "hour*space to hour"=model.comparison(mnFit.hour,mnFit.interaction),
    "hour*space to space"=model.comparison(mnFit.zip,mnFit.interaction),
    "interaction effect"=model.comparison(mnFit,mnFit.interaction))

