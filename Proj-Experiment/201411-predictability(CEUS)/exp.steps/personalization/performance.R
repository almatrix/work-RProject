library(reshape2)
library(ggplot2)
library(nnet)
library(gridExtra)

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
png("weight.bases.png",width=2000,height=900,res=300)
ggplot(prediction.bases.plot,
       aes(x=learning.size,y=value,
           group=as.factor(weight.base),color=as.factor(weight.base)))+
#     geom_point(size=0.3)+
    geom_smooth(method="lm",formula=y~log(x),se=F,alpha=0.3,aes(weight=predicting.size))+
#     scale_y_continuous(limit=c(0,1))+
    scale_x_log10()+
    facet_wrap(city~variable,scales="free_y")+
    theme_bw(base_size = 10) %+replace%
    theme(plot.margin=unit(c(0,0,0,0),"cm"))
dev.off()

# learn the behavior of differnt learning ratio under the same weight base (0.001)
prediction.ratios = predictions[which(predictions$weight.base==0.001),c(1:5,8:12)]
prediction.ratios.melt = melt(prediction.ratios,id.vars=c(1:7))
png("learning.ratios.png",width=2000,height=900,res=300)
ggplot(prediction.ratios.melt[which(prediction.ratios.melt$learning.size>30),],
       aes(x=learning.size,y=value,
           group=as.factor(learning.ratio),color=as.factor(learning.ratio)))+
#     geom_point(size=0.3)+
    geom_smooth(method="lm",formula=y~log(x),se=F,alpha=0.3,aes(weight=predicting.size))+
    #     scale_y_continuous(limit=c(0,1))+
    scale_x_log10()+
    facet_wrap(city~variable,scales="free_y")+
    theme_bw(base_size = 10) %+replace%
    theme(plot.margin=unit(c(0,0,0,0),"cm"))
dev.off()


############################
dta =  predictions[which(predictions$learning.size>30&
                                  predictions$city=="Chi"),]
dta$learning.size.log=log(dta$learning.size)
scattermatrix<-scatter.regression.matrix(
    data = dta,
    xs = c("weight.base","learning.ratio","learning.size.log"),
    ys = c("globalmodel.adj","usermodel.adj","alpha.adj"),
    weight = "predicting.size")
x.density = scattermatrix$x.density
y.density = scattermatrix$y.density
regression.matrix=scattermatrix$regression.matrix
empty.plot = scattermatrix$empty.plot

png("test_Chi.png",res=300,width=3000,height=1500)
grid.arrange(x.density[[1]],x.density[[2]],x.density[[3]],empty.plot,
             regression.matrix[[1]][[1]],regression.matrix[[2]][[1]],regression.matrix[[3]][[1]],y.density[[1]],
             regression.matrix[[1]][[2]],regression.matrix[[2]][[2]],regression.matrix[[3]][[2]],y.density[[2]],
             regression.matrix[[1]][[3]],regression.matrix[[2]][[3]],regression.matrix[[3]][[3]],y.density[[3]],
             nrow=4,widths=c(1,1,1,0.8))
dev.off()

####
dta =  predictions[which(predictions$learning.size>30&
                             predictions$city=="LA"),]
dta$learning.size.log=log(dta$learning.size)
scattermatrix<-scatter.regression.matrix(
    data = dta,
    xs = c("weight.base","learning.ratio","learning.size.log"),
    ys = c("globalmodel.adj","usermodel.adj","alpha.adj"),
    weight = "predicting.size")
x.density = scattermatrix$x.density
y.density = scattermatrix$y.density
regression.matrix=scattermatrix$regression.matrix
empty.plot = scattermatrix$empty.plot

png("test_LA.png",res=300,width=3000,height=1500)
grid.arrange(x.density[[1]],x.density[[2]],x.density[[3]],empty.plot,
             regression.matrix[[1]][[1]],regression.matrix[[2]][[1]],regression.matrix[[3]][[1]],y.density[[1]],
             regression.matrix[[1]][[2]],regression.matrix[[2]][[2]],regression.matrix[[3]][[2]],y.density[[2]],
             regression.matrix[[1]][[3]],regression.matrix[[2]][[3]],regression.matrix[[3]][[3]],y.density[[3]],
             nrow=4,widths=c(1,1,1,0.8))
dev.off()

####
dta =  predictions[which(predictions$learning.size>30&
                             predictions$city=="NYC"),]
dta$learning.size.log=log(dta$learning.size)
scattermatrix<-scatter.regression.matrix(
    data = dta,
    xs = c("weight.base","learning.ratio","learning.size.log"),
    ys = c("globalmodel.adj","usermodel.adj","alpha.adj"),
    weight = "predicting.size")
x.density = scattermatrix$x.density
y.density = scattermatrix$y.density
regression.matrix=scattermatrix$regression.matrix
empty.plot = scattermatrix$empty.plot

png("test_NYC.png",res=300,width=3000,height=1500)
grid.arrange(x.density[[1]],x.density[[2]],x.density[[3]],empty.plot,
             regression.matrix[[1]][[1]],regression.matrix[[2]][[1]],regression.matrix[[3]][[1]],y.density[[1]],
             regression.matrix[[1]][[2]],regression.matrix[[2]][[2]],regression.matrix[[3]][[2]],y.density[[2]],
             regression.matrix[[1]][[3]],regression.matrix[[2]][[3]],regression.matrix[[3]][[3]],y.density[[3]],
             nrow=4,widths=c(1,1,1,0.8))
dev.off()