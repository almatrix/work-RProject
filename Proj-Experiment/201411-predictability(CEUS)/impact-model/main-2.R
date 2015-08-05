library(ggplot2)
library(nnet)
library(reshape2) # dcast
library(gridExtra)

source("../exp.steps/functions.R")
source("impact.model-2.R")
source("personalization.evaulation.R")

load("../data/checkin.poly.list.Rda")
city.guide=data.frame("city"=c("Chicago","Los Angeles","New York City"),
                      "spatial.attr" = c("ZIP","Zip_Num","POSTAL"),
                      stringsAsFactors=FALSE)
city.index = 3
data.regress <- checkin.poly.list[[city.index]][,c("gid","user_id","venue_id",
                                          "cate_l1","hour","weekday",
                                          city.guide[city.index,"spatial.attr"],
                                          "last.cate_l1","last.venue_id",
                               "timestamps","time.interval")]
colnames(data.regress)[7]="ZIP"
data.regress = data.regress[complete.cases(data.regress),]

data.regress = data.preparation(data.regress)
training.data = data.regress$training.data
reference.data = data.regress$reference.data


########### 
# test 1: global model 模型检测
# 1) 通过model fitness(CCR, AIC，R2 over null)可以观察/比较
#    各种context模型自身的统计指标/优劣
# 2) 通过anova可以证明full模型对各个离散模型的改进是明显的，
#    因此之后的personalization是基于full模型开展
###########
dta = data.regress
im.hour = impact.model(data=dta,x="hour2",y="cate_l1")
im.zip = impact.model(data=dta,x="ZIP",y="cate_l1")
im.last = impact.model(data=dta,x="last.cate_l1",y="cate_l1",w="wgt")
dta.im = apply.impact.models(dta,im.hour,"hour2")
dta.im = apply.impact.models(dta.im,im.zip,"ZIP")
dta.im = apply.impact.models(dta.im,im.last,"last.cate_l1")
rm(dta,im.hour,im.zip,im.last)

gmodel.null=multinom(data=dta.im[,c(1:7)], cate_l1~1, maxit=1000,trace=FALSE)
print(paste(Sys.time(),": null model finished."))
gmodel.hour = multinom(data=dta.im[,c(7,14:23)], cate_l1~., maxit=1000,trace=FALSE)
print(paste(Sys.time(),": temporal model finished."))
gmodel.zip = multinom(data=dta.im[,c(7,24:33)], cate_l1~., maxit=1000,trace=FALSE)
print(paste(Sys.time(),": spatial model finished."))
gmodel.last = multinom(data=dta.im[,c(7,34:43)], cate_l1~., maxit=1000,trace=FALSE)
print(paste(Sys.time(),": sequential model finished."))
gmodel.st = multinom(data=dta.im[,c(7,14:33)], cate_l1~., maxit=2000,trace=FALSE)
print(paste(Sys.time(),": spatiotemporal model finished."))
gmodel.full = multinom(data=dta.im[,c(7,14:43)], cate_l1~., maxit=2000,trace=FALSE)
print(paste(Sys.time(),": full model finished."))

model.list = list(gmodel.null,gmodel.hour,gmodel.zip,
                  gmodel.last,gmodel.st,gmodel.full)
model.name = c("null","temporal","spatial","sequential","spatiotemporal","full")

## 1) 模型fitness 检测
fitness.table <- do.call(rbind,lapply(model.list,function(model){
    fitness = model.fitness(model,dta.im)
    r2 = model.comparison(gmodel.null,model)[,5:7]
    
    cbind(fitness[[3]],r2)
    #         dta.plot = as.data.frame(fitness[[1]])
    #         dta.diag = dta.plot[which(dta.plot$cate_l1==dta.plot$prediction),]
    #         ggplot(dta.plot,aes(x=cate_l1,y=prediction,fill=Freq))+
    #             geom_tile()+
    #             geom_tile(data=dta.diag,color="white")+
    #             geom_text(data=dta.diag,color="white",
    #                       aes(label=formatC(Freq/sum(dta.plot$Freq),digits=2)))+
    #             scale_fill_continuous(limits=c(0,ceiling(sum(dta.plot$Freq)*0.26)))
}))
rownames(fitness.table)=model.name
fitness.table
#     list(fitness.gg,fitness.table)
}

## 2) anova 分析证明full的改进程度
# overall model test/ model comparison: likelihood-ratio test
comp = do.call(rbind,lapply(1:5,function(i){
    model1 = model.list[[i]]
    #     do.call(rbind,lapply((i+1):5,function(j){
    #         model2 = model.list[[j]]
    model2 = gmodel.full
    comp = anova(model1, model2)[,2:7]
    comp$significance = significance.code(comp[,6])
    comp$Test = c("",paste("full over",model.name[i]))
    comp$Model=c(model.name[i],"full")
    comp[,c(3,8,1,2,4:7)]
    #     }))
    
}))
comp

########### 
# test 2: personalized model 模型检测
# 1) 通过与global model的CCR比较可以得出个性化后的模型对全局模型的改进程度
# 2) 通过参数变化(k,f,Z)对personalized model的CCR影响，可以找到最合适的参数，
#    从而得到最优个性化模型
###########

eva.k = do.call(rbind,lapply(seq(0,100,5),function(k){
    file = paste("result/pers-eva",k,0,30,".Rda",sep="-")
    if(file.exists(file)){
        load(file)
        eva
    }else{
        print(file)
        NA
    }  
}))
eva.f = do.call(rbind,lapply(seq(-3,1,0.2),function(f){
    file = paste("result/pers-eva",15,f,30,".Rda",sep="-")
    if(file.exists(file)){
        load(file)
        eva
    }else{
        print(file)
        NA
    }
    
}))
eva.Z = do.call(rbind,lapply(seq(5,100,5),function(Z){
    file = paste("result/pers-eva",15,0,Z,".Rda",sep="-")
    if(file.exists(file)){
        load(file)
        eva
    }else{
        print(file)
        NA
    }
    
}))


overall.rate.k = do.call(
    rbind,lapply(split(eva.k, eva.k$k),function(k){
        rate.g.overall = sum(k$cate_l1==k$pred.g)/nrow(k)
        rate.p.overall = sum(k$cate_l1==k$pred.p)/nrow(k)
        
        data.frame("rate.g.overall"=rate.g.overall,
                   "rate.p.overall"=rate.p.overall,
#                    "improvement"=rate.p.overall-rate.g.overall,
                   "k"=k[1,"k"])
    }))

overall.rate.f = do.call(
    rbind,lapply(split(eva.f, eva.f$f),function(f){
        rate.g.overall = sum(f$cate_l1==f$pred.g)/nrow(f)
        rate.p.overall = sum(f$cate_l1==f$pred.p)/nrow(f)
        
        data.frame("rate.g.overall"=rate.g.overall,
                   "rate.p.overall"=rate.p.overall,
#                    "improvement"=rate.p.overall-rate.g.overall,
                   "f"=f[1,"f"])
    }))

overall.rate.Z = do.call(
    rbind,lapply(split(eva.Z, eva.Z$Z),function(Z){
        rate.g.overall = sum(Z$cate_l1==Z$pred.g)/nrow(Z)
        rate.p.overall = sum(Z$cate_l1==Z$pred.p)/nrow(Z)
        
        data.frame("rate.g.overall"=rate.g.overall,
                   "rate.p.overall"=rate.p.overall,
#                    "improvement"=rate.p.overall-rate.g.overall,
                   "Z"=Z[1,"Z"])
    }))

ggplot(melt(overall.rate.k,id.vars="k"),aes(x=k,y=value))+
    geom_line()+geom_point()+
    facet_wrap(~variable,scales="free_y")+
    theme_bw(base_size = 16)
ggplot(melt(overall.rate.f,id.vars="f"),aes(x=log10(f),y=value))+
    geom_line()+geom_point()+
    facet_wrap(~variable,scales="free_y")+
    theme_bw(base_size = 16)
ggplot(melt(overall.rate.Z,id.vars="Z"),aes(x=Z,y=value))+
    geom_line()+geom_point()+
    facet_wrap(~variable,scales="free_y")+
    theme_bw(base_size = 16)


ggplot(unique(eva.k[which(eva.k$k==15),c(1,3,4,5,7,9)]),aes(x=rate.g,y=rate.p))+
    geom_point(aes(color=entropy.adj))+
    theme_bw(base_size=16)
ggplot(unique(eva.k[which(eva.k$k==15),c(1,3,4,5,7,9)]),
       aes(x=entropy,y=rate.p-rate.g))+
    geom_point(aes(alpha=log(sample.size)))+
    theme_bw(base_size=16)
with(unique(eva.k[which(eva.k$k==15),]),cor(rate.g, entropy.adj))
with(unique(eva.k[which(eva.k$k==15),]),cor(rate.g, entropy))
with(unique(eva.k[which(eva.k$k==15),]),cor(rate.g, sample.size))
with(unique(eva.k[which(eva.k$k==15),]),cor(rate.g, entropy.adj))

# CTable.global
cTab.g <- xtabs(~cate_l1+pred.g,data=eva.k[which(eva.k$k==15),])
dta.plot.g = as.data.frame(cTab.g)
dta.plot.g$cate_l1=abbreviate(as.character(dta.plot.g$cate_l1))
dta.plot.g$pred.g=abbreviate(as.character(dta.plot.g$pred.g))
dta.plot.g=do.call(rbind,lapply(split(dta.plot.g,dta.plot.g$cate_l1),function(cate){
    cate$Precision = cate[which(cate$pred.g==cate[1,"cate_l1"]),"Freq"] / sum(cate$Freq)
    cate
}))
dta.plot.g=do.call(rbind,lapply(split(dta.plot.g,dta.plot.g$pred.g),function(cate){
    cate$Recall = cate[which(cate$cate_l1==cate[1,"pred.g"]),"Freq"] / sum(cate$Freq)
    cate
}))

dta.diag.g = dta.plot.g[which(dta.plot.g$cate_l1==dta.plot.g$pred.g),]
CT.g<-ggplot(dta.plot.g,aes(x=cate_l1,y=pred.g,fill=Freq))+
    geom_tile()+
    geom_tile(data=dta.diag,color="white")+
    geom_text(data=dta.diag,color="white",size=4,
              aes(label=formatC(Freq/sum(dta.plot.g$Freq),digits=2)))+
    scale_fill_continuous(limits=ceiling(sum(dta.plot.g$Freq)*c(0,0.26)))+
    labs(x="Real Interest\n(a)",y="Predicted Interest (Personalized)")+
    theme_bw(base_size=16) %+replace%
    theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1,vjust=1))
    
# CTable.personalized
cTab.p <- xtabs(~cate_l1+pred.p,data=eva.k[which(eva.k$k==15),])
dta.plot.p = as.data.frame(cTab.p)
dta.plot.p$cate_l1=abbreviate(as.character(dta.plot.p$cate_l1))
dta.plot.p$pred.p=abbreviate(as.character(dta.plot.p$pred.p))
dta.plot.p=do.call(rbind,lapply(split(dta.plot.p,dta.plot.p$cate_l1),function(cate){
    cate$Precision = cate[which(cate$pred.p==cate[1,"cate_l1"]),"Freq"] / sum(cate$Freq)
    cate
}))
dta.plot.p=do.call(rbind,lapply(split(dta.plot.p,dta.plot.p$pred.p),function(cate){
    cate$Recall = cate[which(cate$cate_l1==cate[1,"pred.p"]),"Freq"] / sum(cate$Freq)
    cate
}))
dta.diag.p = dta.plot.p[which(dta.plot.p$cate_l1==dta.plot.p$pred.p),]
CT.p<-ggplot(dta.plot.p,aes(x=cate_l1,y=pred.p,fill=Freq))+
    geom_tile()+
    geom_tile(data=dta.diag.p,color="white")+
    geom_text(data=dta.diag.p,color="white",size=4,
              aes(label=formatC(Freq/sum(dta.plot.p$Freq),digits=2)))+
    scale_fill_continuous(limits=ceiling(sum(dta.plot.g$Freq)*c(0,0.26)))+
    labs(x="Real Interest\n(b)",y="Predicted Interest (Personalized)")+
    theme_bw(base_size=16) %+replace%
    theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1,vjust=1))

dta.diag.g$model = "Global";dta.diag.p$model="Personalized"
precision.recall<- 
    ggplot(melt(rbind(dta.diag.g[,c(1,4,5,6)],dta.diag.p[,c(1,4,5,6)]),
           id.vars=c("cate_l1","model")))+
    geom_point(aes(x=cate_l1,y=value,color=model),size=2)+
#     geom_line(aes(x=cate_l1,y=value,color=model,group=model))
    labs(x="Interest Type\n(c)",y="Precision / Recall")+
    facet_wrap(~variable)+
    theme_bw(base_size=16) %+replace%
    theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))

    
png("g.vs.p.png",width=3000,height=2600,res=300)
grid.arrange(arrangeGrob(CT.g,CT.p,ncol=2),precision.recall,
             ncol=1,nrow=2,heights=c(1.2,1))
dev.off()

########### 
# test 3: prediction
###########


pred.result = prediction.eva(training.data, reference.data)
save(pred.result,file=paste("pred.result.",city.guide[city.index,"city"],".Rda",sep=""))
#############
# overall performance
fit = do.call(rbind,lapply(pred.result,function(user){
    user$inner
}))
fit = fit[fit$note %in% c("1","2"),]
prediction = do.call(rbind,lapply(pred.result,function(user){
    user$outer
}))
prediction = prediction[prediction$note %in% c("1","2"),]
CCR.g.overall = sum(fit$cate_l1==fit$fit.g)/nrow(fit)
CCR.p.overall = sum(fit$cate_l1==fit$fit.p)/nrow(fit)
CPR.g.overall = sum(prediction$cate_l1==prediction2$pred.g)/nrow(prediction2)
CPR.p.overall = sum(prediction$cate_l1==prediction$pred.p)/nrow(prediction)

##############
# performance by user
to.investigate = do.call(rbind,lapply(pred.result,function(user){
    inner = unique(user$inner[,c(1:5,9,10)])
    outer = unique(user$outer[,c(9:11)])
    cbind(inner,outer)
}))
to.investigate$entropy.scl = with(to.investigate, cut(entropy,breaks=quantile(entropy),include.lowest = T))
to.investigate$TP.ratio.scl = with(to.investigate, cut(TP.ratio,breaks=quantile(TP.ratio,probs=c(0,0.5,0.75,1)),include.lowest = T))
to.investigate$trainig.lng.scl = with(to.investigate, cut(user.training.length,breaks=quantile(user.training.length,probs=c(0,0.5,0.75,1)),include.lowest = T))

scatterplot.matrix(~CCR.g+CCR.p+CPR.g+CPR.p|entropy.scl, 
                   data=to.investigate[to.investigate$note=="1: Normal personal regression model.",],
                   upper.panel=NULL)
scatterplot.matrix(~CCR.g+CCR.p+CPR.g+CPR.p|TP.ratio.scl, 
                   data=to.investigate[to.investigate$note=="1: Normal personal regression model.",],
                   upper.panel=NULL)
scatterplot.matrix(~CCR.g+CCR.p+CPR.g+CPR.p|trainig.lng.scl, 
                   data=to.investigate[to.investigate$note=="1: Normal personal regression model.",],
                   upper.panel=NULL)
View(var(to.investigate[,c(2,4:9)],na.rm=T))

# CCR.g v.s. CCR.p
ggplot(to.investigate[complete.cases(to.investigate),],aes(x=CCR.g,y=CCR.p))+
    geom_point(alpha=0.6)+
    geom_smooth()
# CCR.g v.s. CPR.g
ggplot(to.investigate[complete.cases(to.investigate),],aes(x=CCR.g,y=CPR.g))+
    geom_point(alpha=0.6)+
    geom_smooth()
# CPR.g v.s. CPR.p
ggplot(to.investigate[complete.cases(to.investigate),],aes(x=CPR.g,y=CPR.p))+
    geom_point(alpha=0.6)+
    geom_smooth(method="lm")
#     geom_segment(aes(x = 0, y = 0, xend = CCR.g, yend = CCR.p, color=TP.ratio.scl),
#                  arrow = arrow(length = unit(0.5, "cm")),alpha=0.3)+
    facet_wrap(~note)
# CPR.g v.s. CPR.p
ggplot(to.investigate[(to.investigate$CPR.g|to.investigate$CPR.p),])+
    geom_point(aes(x=CPR.g,y=CPR.p))+
#     geom_segment(aes(x = 0, y = 0, xend = CPR.g, yend = CPR.p, color=TP.ratio),
#                  arrow = arrow(length = unit(0.5, "cm")),alpha=0.5)+
    facet_wrap(~note)
# CCR v.s. CPR
ggplot(to.investigate) + 
    geom_point(aes(x=CCR.g,y=CCR.p,size=entropy),color="green",shape=21,alpha=0.5)+
    geom_point(aes(x=CPR.g,y=CPR.p,size=entropy),color="red",shape=21,alpha=0.5)+
    geom_segment(aes(x = CCR.g, y = CCR.p, xend = CPR.g, yend = CPR.p, color=TP.ratio),
                 arrow = arrow(length = unit(0.5, "cm")))+
    facet_wrap(~note)
ggplot(to.investigate) + 
    geom_point(aes(x=CCR.g,y=CPR.g),color="red")+
    geom_point(aes(x=CCR.p,y=CPR.p),color="green")


##############
# performance by venue type
performance.by.type = function(real,predicted,data,model.name){
    cTab <<- as.data.frame(xtabs(as.formula(paste("~",real,"+",predicted)),data=data))
    cTab.real <<- as.data.frame(xtabs(as.formula(paste("~",real)),data=data))
    cTab.pred <<- as.data.frame(xtabs(as.formula(paste("~",predicted)),data=data))
    cTab <- merge(cTab, cTab.real, by=real)
    cTab <- merge(cTab, cTab.pred, by=predicted)
    colnames(cTab)[3:5]=c("Freq","Freq.real","Freq.predicted")
    cTab[,real]=abbreviate(as.character(cTab[,real]))
    cTab[,predicted]=abbreviate(as.character(cTab[,predicted]))
    cTab$Rate = with(cTab,Freq/sum(Freq))
    
    
    cTab.diag = cTab[which(cTab[,real]==cTab[,predicted]),]
    cTab.diag$Precision = with(cTab.diag, Freq/Freq.real)
    cTab.diag$Recall = with(cTab.diag, Freq/Freq.predicted)
    cTab.diag$model = model.name
    
    list(cTab, cTab.diag)
#     dta.plot=do.call(rbind,lapply(split(dta.plot,dta.plot[,real]),function(cate){
#         cate$Rate = cate$Freq / sum(dta.plot$Freq)
#         cate
#     }))
#     dta.plot=do.call(rbind,lapply(split(dta.plot,dta.plot[,predicted]),function(cate){
#         cate$Recall = cate[which(cate[,real]==cate[1,predicted]),"Freq"] / sum(cate$Freq)
#         cate
#     }))
#     dta.plot=do.call(rbind,lapply(split(dta.plot,dta.plot[,real]),function(cate){
#         cate$Precision = cate[which(cate[,predicted]==cate[1,real]),"Freq"] / sum(cate$Freq)
#         cate
#     }))
#     dta.plot=do.call(rbind,lapply(split(dta.plot,dta.plot[,predicted]),function(cate){
#         cate$Recall = cate[which(cate[,real]==cate[1,predicted]),"Freq"] / sum(cate$Freq)
#         cate
#     }))
#     
#     dta.diag = dta.plot[which(dta.plot[,real]==dta.plot[,predicted]),]
#     dta.diag$model = model.name
#     
#     list(dta.plot, dta.diag)
}
# CTable.global
perf.g.fit = performance.by.type("cate_l1","fit.g",fit,"Global")
perf.p.fit = performance.by.type("cate_l1","fit.p",fit,"Personalized")
perf.g.pred = performance.by.type("cate_l1","pred.g",prediction[prediction$user.training.length>0,],"Global")
perf.p.pred = performance.by.type("cate_l1","pred.p",prediction[prediction$user.training.length>0,],"Personalized")

gg.g.fit<-ggplot(perf.g.fit[[1]],aes(x=cate_l1,y=fit.g,fill=Freq))+
    geom_tile()+
    geom_tile(data=perf.g.fit[[2]],color="black")+
    geom_text(data=perf.g.fit[[2]],color="black",size=4,
              aes(label=formatC(perf.g.fit[[2]]$Rate,digits=2)))+
#               aes(label=formatC(Freq/sum(perf.g.fit[[1]]$Freq),digits=2)))+
    scale_fill_continuous(low="white",high="#66CC99",
                          limits=ceiling(sum(perf.g.fit[[1]]$Freq)*c(0,0.28)))+
    labs(x="Real Interest\n(a)",y="Estimated Interest (Global)")+
    theme_bw(base_size=16) %+replace%
    theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1,vjust=1))

gg.p.fit<-ggplot(perf.p.fit[[1]],aes(x=cate_l1,y=fit.p,fill=Freq))+
    geom_tile()+
    geom_tile(data=perf.p.fit[[2]],color="black")+
    geom_text(data=perf.p.fit[[2]],color="black",size=4,
              aes(label=formatC(perf.p.fit[[2]]$Rate,digits=2)))+
#               aes(label=formatC(Freq/sum(perf.g.fit[[1]]$Freq),digits=2)))+
    scale_fill_continuous(low="white",high="#66CC99",
                          limits=ceiling(sum(perf.g.fit[[1]]$Freq)*c(0,0.28)))+
    labs(x="Real Interest\n(b)",y="Estimated Interest (Personalized)")+
    theme_bw(base_size=16) %+replace%
    theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1,vjust=1))

gg.ref.fit<-ggplot(perf.g.fit[[1]],aes(x=cate_l1,y=cate_l1,fill=Freq.real))+
    geom_tile()+
    geom_tile(data=perf.g.fit[[2]],color="black")+
    geom_text(data=perf.g.fit[[2]],color="black",size=4,
              aes(label=formatC(perf.g.fit[[2]]$Freq.real/sum(perf.g.fit[[2]]$Freq.real),digits=2)))+
    scale_fill_continuous(low="white",high="#66CC99",
                          limits=ceiling(sum(perf.g.fit[[2]]$Freq.real)*c(0,0.28)))+
    labs(x="Real Interest\n(c)",y="Real Interest (for Reference)")+
    theme_bw(base_size=16) %+replace%
    theme(legend.position="none",panel.grid=element_blank(),
          axis.text.x=element_text(angle=30,hjust=1,vjust=1))
precision.recall.fit<- 
    ggplot(melt(rbind(perf.p.fit[[2]][,c(2,7:9)],perf.g.fit[[2]][,c(2,7:9)]),
                id.vars=c("cate_l1","model")))+
    geom_point(aes(x=cate_l1,y=value,color=model),size=2)+
    geom_line(aes(x=cate_l1,y=value,color=model,group=model))+
    labs(x="Interest Type\n(c)",y="Precision / Recall")+
    facet_wrap(~variable)+
    theme_bw(base_size=16) %+replace%
    theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))

################

gg.g.pred<-ggplot(perf.g.pred[[1]],aes(x=cate_l1,y=pred.g,fill=Freq))+
    geom_tile()+
    geom_tile(data=perf.g.pred[[2]],color="black")+
    geom_text(data=perf.g.pred[[2]],color="black",size=4,
              aes(label=formatC(perf.g.pred[[2]]$Rate,digits=2)))+
    #               aes(label=formatC(Freq/sum(perf.g.pred[[1]]$Freq),digits=2)))+
    scale_fill_continuous(low="white",high="#66CC99",
                          limits=ceiling(sum(perf.g.pred[[1]]$Freq)*c(0,0.28)))+
    labs(x="Real Interest\n(a)",y="Predicted Interest (Global)")+
    theme_bw(base_size=16) %+replace%
    theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1,vjust=1))

gg.p.pred<-ggplot(perf.p.pred[[1]],aes(x=cate_l1,y=pred.p,fill=Freq))+
    geom_tile()+
    geom_tile(data=perf.p.pred[[2]],color="black")+
    geom_text(data=perf.p.pred[[2]],color="black",size=4,
              aes(label=formatC(perf.p.pred[[2]]$Rate,digits=2)))+
    #               aes(label=formatC(Freq/sum(perf.g.pred[[1]]$Freq),digits=2)))+
    scale_fill_continuous(low="white",high="#66CC99",
                          limits=ceiling(sum(perf.g.pred[[1]]$Freq)*c(0,0.28)))+
    labs(x="Real Interest\n(b)",y="Predicted Interest (Personalized)")+
    theme_bw(base_size=16) %+replace%
    theme(legend.position="none",axis.text.x=element_text(angle=30,hjust=1,vjust=1))

gg.ref.pred<-ggplot(perf.g.pred[[1]],aes(x=cate_l1,y=cate_l1,fill=Freq.real))+
    geom_tile()+
    geom_tile(data=perf.g.pred[[2]],color="black")+
    geom_text(data=perf.g.pred[[2]],color="black",size=4,
              aes(label=formatC(perf.g.pred[[2]]$Freq.real/sum(perf.g.pred[[2]]$Freq.real),digits=2)))+
    scale_fill_continuous(low="white",high="#66CC99",
                          limits=ceiling(sum(perf.g.pred[[2]]$Freq.real)*c(0,0.28)))+
    labs(x="Real Interest\n(c)",y="Real Interest (for Reference)")+
    theme_bw(base_size=16) %+replace%
    theme(legend.position="none",panel.grid=element_blank(),
          axis.text.x=element_text(angle=30,hjust=1,vjust=1))
precision.recall.pred<- 
    ggplot(melt(rbind(perf.p.pred[[2]][,c(2,7:9)],perf.g.pred[[2]][,c(2,7:9)]),
                id.vars=c("cate_l1","model")))+
    geom_point(aes(x=cate_l1,y=value,color=model),size=2)+
    geom_line(aes(x=cate_l1,y=value,color=model,group=model))+
    labs(x="Interest Type\n(c)",y="Precision / Recall")+
    facet_wrap(~variable)+
    theme_bw(base_size=16) %+replace%
    theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))

##############
# png("g.vs.p.png",width=3000,height=2600,res=300)
grid.arrange(arrangeGrob(gg.g.fit,gg.p.fit,gg.ref.fit,ncol=3),precision.recall.fit,
             ncol=1,nrow=2,heights=c(1.4,1))
grid.arrange(arrangeGrob(gg.g.pred,gg.p.pred,gg.ref.pred,ncol=3),precision.recall.pred,
             ncol=1,nrow=2,heights=c(1.2,1))
# dev.off()