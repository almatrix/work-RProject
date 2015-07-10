library(reshape2)
library(ggplot2)
library(nnet)

source("../functions.R")

#############################
# load all data
# prediction.Chi = do.call(rbind,lapply(1:84,function(i){
#     load(paste("result/Chi_0.8/prediction_job",i,".Rda",sep=""))
#     prediction
# }))
# prediction.LA = do.call(rbind,lapply(169:252,function(i){
#     load(paste("result/LA_0.8/prediction_job",i,".Rda",sep=""))
#     prediction
# }))

# prediction = do.call(rbind,lapply(1:6,function(i){
#     load(paste("result/NYC_0.8/prediction_NYC_0.8_job269",i,".Rda",sep=""))
#     prediction
# }))
# save(prediction,file="result/NYC_0.8/prediction_NYC_0.8_job269.Rda")
# prediction.NYC = do.call(rbind,lapply(1:350,function(i){
#     load(paste("result/NYC_0.8/prediction_NYC_0.8_job",i,".Rda",sep=""))
#     prediction
# }))

# save(prediction.Chi,file="result/Chi_0.8.Rda")
# save(prediction.LA,file="result/LA_0.8.Rda")
# save(prediction.NYC,file="result/NYC_0.8.Rda")
load("result/Chi_0.8.Rda")
load("result/LA_0.8.Rda")
load("result/NYC_0.8.Rda")

predictions = rbind(prediction.Chi,prediction.LA,prediction.NYC)[,c(1:9)]
predictions$usermodel.adj=with(predictions,
                               (usermodel*predicting.size+2)/(predicting.size+4))
predictions$globalmodel.adj=with(predictions,
                                 (globalmodel*predicting.size+2)/(predicting.size+4))
predictions$alpha.adj=with(predictions, 
                           atan(usermodel.adj/(globalmodel.adj+1e-20))/pi*180)


######################
# model evaluation and comparison

# learn the behavior of differnt weight base under the same learning ratio (0.8)
prediction.bases = predictions[which(predictions$learning.ratio==0.8),c(1:5,8:12)]
prediction.bases.melt = melt(prediction.bases,id.vars=c(1:7))
prediction.bases.plot = prediction.bases.melt[which(prediction.bases.melt$learning.size>30),]
png("weight.bases.png",width=3000,height=1500,res=300)
ggplot(prediction.bases.plot,
       aes(x=learning.size,y=value,
           group=as.factor(weight.base),color=as.factor(weight.base)))+
    geom_point(size=0.7)+
    geom_smooth(method="lm",formula=y~log(x),se=F,alpha=0.3,aes(weight=predicting.size))+
#     scale_y_continuous(limit=c(0,1))+
    scale_x_log10()+
    facet_wrap(city~variable,scales="free_y")+
    theme_bw() 
dev.off()

# learn the behavior of differnt learning ratio under the same weight base (0.001)
prediction.ratios = predictions[which(predictions$weight.base==0.001),c(1:5,8:12)]
prediction.ratios.melt = melt(prediction.ratios,id.vars=c(1:7))
png("learning.ratios.png",width=3000,height=1500,res=300)
ggplot(prediction.ratios.melt[which(prediction.ratios.melt$learning.size>30),],
       aes(x=learning.size,y=value,
           group=as.factor(learning.ratio),color=as.factor(learning.ratio)))+
    geom_point(size=0.7)+
    geom_smooth(method="lm",se=F,alpha=0.3,aes(weight=predicting.size))+
    #     scale_y_continuous(limit=c(0,1))+
    scale_x_log10()+
    facet_wrap(city~variable,scales="free_y")+
    theme_bw() 
dev.off()

png("test1.png",width=3000,height=2100,res=300)
plot(prediction.bases[sample(1:nrow(prediction.bases),
                             ceiling(nrow(prediction.bases)*0.05)),
                      c(2,5,6,8,9,10)])
dev.off()