library(ggplot2)
library(nnet)
library(reshape2) # dcast

source("../exp.steps/functions.R")
source("impact.model-2.R")

load("../data/checkin.poly.list.Rda")
checkin.chi <- checkin.poly.list[[1]]
data.regress <- checkin.chi[,c("gid","user_id","venue_id","cate_l1","hour","weekday",
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

########### 
# test 1: evaluation for global model
###########


global.eva = function(dta,...){
    # im.hour = impact.model(data=dta,x="hour",y="cate_l1")
    # im.weekday = impact.model(data=dta,x="weekday",y="cate_l1")
    im.hour = impact.model(data=dta,x="hour2",y="cate_l1",...)
    im.zip = impact.model(data=dta,x="ZIP",y="cate_l1",...)
    im.last = impact.model(data=dta,x="last.cate_l1",y="cate_l1",w="wgt",...)
    # dta.im = apply.impact.models(dta,im.hour,"hour")
    # dta.im = apply.impact.models(dta.im,im.weekday,"weekday")
    dta.im = apply.impact.models(dta,im.hour,"hour2")
    dta.im = apply.impact.models(dta.im,im.zip,"ZIP")
    dta.im = apply.impact.models(dta.im,im.last,"last.cate_l1")
#     rm(dta,im.hour,im.zip,im.last)
    
    gmodel.null=multinom(data=dta.im[,c(1:7)], cate_l1~1, maxit=1000,trace=FALSE)
    print(paste(Sys.time(),": null model finished."))
    # gmodel.hour = multinom(data=dta.im[,c(9,14:23)], cate_l1~., maxit=1000)
    # gmodel.weekday = multinom(data=dta.im[,c(9,24:33)], cate_l1~., maxit=1000)
    gmodel.hour = multinom(data=dta.im[,c(7,14:23)], cate_l1~., maxit=1000,trace=FALSE)
    print(paste(Sys.time(),": temporal model finished."))
    gmodel.zip = multinom(data=dta.im[,c(7,24:33)], cate_l1~., maxit=1000,trace=FALSE)
    print(paste(Sys.time(),": spatial model finished."))
    gmodel.last = multinom(data=dta.im[,c(7,34:43)], cate_l1~., maxit=1000,trace=FALSE)
    print(paste(Sys.time(),": sequential model finished."))
    # gmodel.full = multinom(data=dta.im[,c(7,14:33,44:63)], cate_l1~., maxit=2000)
    gmodel.full = multinom(data=dta.im[,c(7,14:43)], cate_l1~., maxit=2000,trace=FALSE)
    print(paste(Sys.time(),": full model finished."))
    
    model.list = list(gmodel.null,gmodel.hour,gmodel.zip,
                      gmodel.last,gmodel.full)
    model.name = c("null","temporal","spatial","sequential","full")
    
    # access model fit
#     fitness.table<<-data.frame()
    fitness.table <- do.call(rbind,lapply(model.list,function(model){
        fitness = model.fitness(model,dta.im)
        r2 = model.comparison(gmodel.null,model)[,5:7]
        
        cbind(fitness[[3]],r2)
#         fitness.table<<-rbind(fitness.table,cbind(fitness[[3]],r2))
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

dta = data.regress
# test the effect of k
test.k <- do.call(rbind,lapply(seq(5,200,5),function(k){
    eva = global.eva(dta, k=k, f=1)
    
    eva$k = k
    eva$f = 1
    eva$model = factor(row.names(eva),levels=row.names(eva))
    
    eva
}))
save(test.k,file="test.k.Rda")

dta.half = data.regress[1:92000,]
# test the effect of k
test.k.half <- do.call(rbind,lapply(seq(5,200,5),function(k){
    eva = global.eva(dta.half, k=k, f=1)
    
    eva$k = k
    eva$f = 1
    eva$model = factor(row.names(eva),levels=row.names(eva))
    
    eva
}))
save(test.k.half,file="test.k.half.Rda")

dta.quarter = data.regress[1:46000,]
# test the effect of k
test.k.quarter <- do.call(rbind,lapply(seq(5,200,5),function(k){
    eva = global.eva(dta.quarter, k=k, f=1)
    
    eva$k = k
    eva$f = 1
    eva$model = factor(row.names(eva),levels=row.names(eva))
    
    eva
}))
save(test.k.quarter,file="test.k.quarter.Rda")

test.k$data.size = nrow(dta)
test.k.half$data.size = nrow(dta.half)
test.k.quarter$data.size = nrow(dta.quarter)
test.k.sum <- rbind(test.k, test.k.half, test.k.quarter)
    
gg1<-ggplot(test.k.sum,aes(x=k,y=CCR,
                      color=as.factor(data.size),
                      group=as.factor(data.size)))+
    geom_point(size=3)+
    geom_line()+
    facet_wrap(~model,scales="free_y",ncol=5)+
    theme_bw(base_size = 16)

gg2<-ggplot(test.k.sum,aes(x=k,y=log10(AIC),
                      color=as.factor(data.size),
                      group=as.factor(data.size)))+
    geom_point(size=3)+
    geom_line()+
    facet_wrap(~model,ncol=5)+
    theme_bw(base_size = 16)

gg3<-ggplot(test.k.sum,aes(x=k,y=CoxSnell.R2,
                      color=as.factor(data.size),
                      group=as.factor(data.size)))+
    geom_point(size=3)+
    geom_line()+
    facet_wrap(~model,scales="free_y",ncol=5)+
    theme_bw(base_size = 16)

grid.arrange(gg1,gg2,gg3,ncol=1,nrow=3)

gg1<-ggplot(test.k,aes(x=k,y=CCR,color=model,group=model))+
    geom_point()+geom_line()
gg2<-ggplot(test.k,aes(x=k,y=log10(AIC),color=model,group=model))+
    geom_point()+geom_line()
gg3<-ggplot(test.k,aes(x=k,y=McFadden.R2,color=model,group=model))+
    geom_point()+geom_line()
# coefficient tests
# model.summary = summary(gmodel.hour)
# z <- model.summary$coefficients/model.summary$standard.errors
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# p

# overall model test/ model comparison: likelihood-ratio test
comp = do.call(rbind,lapply(1:4,function(i){
    model1 = model.list[[i]]
#     do.call(rbind,lapply((i+1):5,function(j){
#         model2 = model.list[[j]]
        model2 = gmodel.full
        comp = anova(model1, model2)[,2:7]
        comp$significance = significance.code(comp[,6])
        comp$Test = c("",paste(model.name[i],"vs full"))
        comp$Model=c(model.name[i],"full")
        comp[,c(8,1:7)]
#     }))
    
}))
comp

########### 
# test 2: test for hour context, 2 types of user
###########
data1 = data.regress[1:1000,]
data2 = data.regress[which(data.regress$user_id==213),]
data3 = data.regress[which(data.regress$user_id==6837),]

im1 = impact.model(data=data1,x="hour",y="cate_l1")
im2 = impact.model(data=data2,x="hour",y="cate_l1",global.im=im1)
im3 = impact.model(data=data3,x="hour",y="cate_l1",global.im=im1)

data1.im = apply.impact.models(data1,im1,"hour")
data2.im = apply.impact.models(data2,im2,"hour")
data3.im = apply.impact.models(data3,im3,"hour")

gmodel1 = multinom(data=data1.im[,c(5,12:21)],cate_l1~.,maxit=1000)
gmodel3 = multinom(data=data3.im[,c(5,12:21)],cate_l1~.,maxit=1000)

# pred.m1.1 = predict(gmodel1)
pred.m1 = predict(gmodel1,type="class",newdata=data1.im)
# pred.m1.3 = apply(predict(gmodel1,type="prob",newdata=data1.im),1,function(i){
#     which.max(i)
# })
# View(cbind(pred.m1.1,pred.m1.2,pred.m1.3,data1.im$cate_l1))
sum(levels(pred.m1)[pred.m1]==data1.im$cate_l1)/nrow(data1.im)

pred.m3 = predict(gmodel3,type="class",newdata=data3.im)
sum(levels(pred.m3)[pred.m3]==data3.im$cate_l1)/nrow(data3.im)
pred.m3.1 = predict(gmodel1,type="class",newdata=data3.im)
sum(levels(pred.m3.1)[pred.m3.1]==data3.im$cate_l1)/nrow(data3.im)
pred.m2.1 = predict(gmodel1,type="class",newdata=data2.im)
sum(levels(pred.m2.1)[pred.m2.1]==data2.im$cate_l1)/nrow(data2.im)

########### 
# test 3: apply to all users
###########
dta = data.regress[1:5000,]

pers.eva = function(data=dta,...){
im.hour = impact.model(data=dta,x="hour2",y="cate_l1",...)
im.zip = impact.model(data=dta,x="ZIP",y="cate_l1",...)
im.last = impact.model(data=dta,x="last.cate_l1",y="cate_l1",w="wgt",...)
dta.im = apply.impact.models(dta,im.hour,"hour2")
dta.im = apply.impact.models(dta.im,im.zip,"ZIP")
dta.im = apply.impact.models(dta.im,im.last,"last.cate_l1")
#     rm(dta,im.hour,im.zip,im.last)
gmodel = multinom(data=dta.im[,c(7,14:43)], cate_l1~., maxit=2000,trace=FALSE)
print(paste(Sys.time(),": full model finished."))
result = do.call(rbind,lapply(split(dta,dta$user_id),function(user){
    print(user[1,"user_id"])
    uim.hour = impact.model(data=user,x="hour2",y="cate_l1",
                            global.im = im.hour,...)
    uim.zip = impact.model(data=user,x="ZIP",y="cate_l1",
                           global.im = im.zip,...)
    uim.last = impact.model(data=user,x="last.cate_l1",y="cate_l1",
                            w="wgt",global.im = im.last,...)
    user.im = apply.impact.models(user,im.hour,"hour2")
    user.im = apply.impact.models(user.im,im.zip,"ZIP")
    user.im = apply.impact.models(user.im,im.last,"last.cate_l1")
    
    # entropy
    stat = as.data.frame(xtabs(~cate_l1,data=user))
    stat$prob = stat$Freq / sum(stat$Freq)
    stat$prob.adj = (stat$Freq+2) / (sum(stat$Freq)+4)
    entropy = -1 * sum(stat$prob * log2(stat$prob),na.rm=T)
    entropy.adj = -1 * sum(stat$prob.adj * log2(stat$prob.adj),na.rm=T)
    
    # model
    if(length(unique(user$cate_l1))>1){
        model = multinom(data=user.im[,c(7,14:43)], cate_l1~., maxit=2000,trace=FALSE)
    }else{
        model = gmodel
    }
    
    pred.g = predict(gmodel,type="class",newdata=user.im)
    pred.p = predict(model,type="class",newdata=user.im)
    
    data.frame("user_id"=user[1,"user_id"],
               "cate_l1"=user.im$cate_l1,
               "sample.size"=nrow(user),
               "entropy"= entropy,
               "entropy.adj"= entropy.adj,
               "pred.g"=pred.g,
               "rate.g"=sum(levels(pred.g)[pred.g]==user.im$cate_l1)/nrow(user.im),
               "pred.p"=pred.p,
               "rate.p"=sum(levels(pred.p)[pred.p]==user.im$cate_l1)/nrow(user.im),
               "candidate.leng"=length(unique(user$cate_l1)))
}))
}

test.k.pers <- do.call(rbind,lapply(seq(0,100,5),function(k){
    eva = pers.eva(dta, k=k, f=1, Z=30)
    
    eva$k = k
    eva$f = 1
    eva$Z = 30
    
    eva
}))
test.k.pers$k=as.factor(test.k.pers$k)


test.Z.pers <- do.call(rbind,lapply(seq(0,100,5),function(Z){
    eva = pers.eva(dta, k=15, f=1, Z=Z)
    
    eva$k = 15
    eva$f = 1
    eva$Z = Z
    
    eva
}))

test.f.pers <- do.call(rbind,lapply(seq(-3,1,0.2),function(f){
    eva = pers.eva(dta, k=15, f=10^f, Z=30)
    
    eva$k = 15
    eva$f = 10^f
    eva$Z = 30
    
    eva
}))

overall.rate = do.call(rbind,lapply(split(test.k.pers, test.k.pers$k),function(model){

    rate.g.overall = sum(model$cate_l1==model$pred.g)/nrow(model)
    rate.p.overall = sum(model$cate_l1==model$pred.p)/nrow(model)

    data.frame("rate.g.overall"=rate.g.overall,
               "rate.p.overall"=rate.p.overall,
               "k"=model[1,"k"])
}))
ggplot(overall.rate,aes(x=k))+
    geom_line(aes(y=rate.g.overall,color="global",group=1))+
    geom_line(aes(y=rate.p.overall,color="personalized",group=1))+
    geom_line(aes(y=rate.p.overall-rate.g.overall,color="improved",group=1))+
    theme_bw(base_size = 16)

# 
# global.im = impact.model(data=dta,x="hour",y="cate_l1")
# dta.im = apply.impact.models(dta, global.im, "hour")
# gmodel = multinom(data=dta.im[,c(5,12:21)],cate_l1~.,maxit=1000)
# result = do.call(rbind,lapply(split(dta,dta$user_id),function(user){
#     print(user[1,"user_id"])
#     im = impact.model(user, x="hour",y="cate_l1",global.im=global.im)
#     user.im = apply.impact.models(user, im, "hour")
#     
#     # entropy
#     stat = as.data.frame(xtabs(~cate_l1,data=user))
#     stat$prob = stat$Freq / sum(stat$Freq)
#     entropy = -1 * sum(stat$prob * log2(stat$prob),na.rm=T)
#     
#     # model
#     if(length(unique(user$cate_l1))>1){
#         model = multinom(data=user.im[,c(5,12:21)],cate_l1~.,maxit=1000)
#     }else{
#         model = gmodel
#     }
#     
#     pred.g = predict(gmodel,type="class",newdata=user.im)
#     pred.p = predict(model,type="class",newdata=user.im)
#     
#     data.frame("user_id"=user[1,"user_id"],
#                "cate_l1"=user.im$cate_l1,
#                "sample.size"=nrow(user),
#                "entropy"= entropy,
#                "pred.g"=pred.g,
#                "rate.g"=sum(levels(pred.g)[pred.g]==user.im$cate_l1)/nrow(user.im),
#                "pred.p"=pred.p,
#                "rate.p"=sum(levels(pred.p)[pred.p]==user.im$cate_l1)/nrow(user.im),
#                "candidate.leng"=length(unique(user$cate_l1)))
# }))
# overall performance
with(result,sum(cate_l1==pred.g)/5000)
with(result,sum(cate_l1==pred.p)/5000)

# individual performance
result.unique = unique(result[,c(1,3,4,6,8,9)])
result.unique$alpha=with(result.unique,
                         atan((rate.p+1e-6)/(rate.g+1e-6))/pi*180)
result.unique$entropy.level = with(result.unique,
                                   findInterval(entropy, c(0,1,2,3)))
scatterplot.matrix(~rate.g+rate.p+alpha|entropy.level, 
                   data=result.unique,
                   upper.panel=NULL)
ggplot(result.unique,aes(x=rate.g,y=rate.p,color=alpha,size=log(sample.size)))+geom_point()
ggplot(result.unique,aes(x=rate.g,y=rate.p,color=alpha,size=candidate.leng))+geom_point()
ggplot(result.unique,aes(x=rate.g,y=rate.p,color=alpha,size=entropy))+geom_point()
# data.regress$last.cate_l1<-as.character(data.regress$last.cate_l1)
# data.regress[is.na(data.regress)]<-"Unknown"
# data.regress$last.cate_l1<-as.factor(data.regress$last.cate_l1)

# data.regress.im <- apply.impact.models(data.regress[1:50,],
#                                        xs=c("hour","ZIP","last.cate_l1"),
#                                        y="cate_l1",
#                                        ws=c(NA,NA,"wgt"),
#                                        k=10, f=5)
#     



