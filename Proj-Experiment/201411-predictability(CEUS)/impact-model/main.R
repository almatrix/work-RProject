library(ggplot2)
library(nnet)
load("data/checkin.poly.list.Rda")
source("exp.steps/functions.R")

checkin.chi <- checkin.poly.list[[1]]

im.hour <- impact.model(checkin.chi, "hour", "cate_l1", k=10, f=5)
im.hour <- xtabs(data=im.hour,impact~cate_l1+condition)

im.zip <- impact.model(checkin.chi,"ZIP","cate_l1",k=10,f=5)
im.zip <- xtabs(data=im.zip,impact~cate_l1+condition)

im.previous <- impact.model(checkin.chi,"last.cate_l1","cate_l1",k=10,f=5)
im.previous <- xtabs(data=im.previous,impact~cate_l1+condition)

data.regress <- checkin.chi[1:5000,c("cate_l1","hour","ZIP","last.cate_l1")]
counter.reset()
data.regress.im <- do.call(rbind,apply(data.regress, 1, function(row){
    counter.print(1000)
    hour = row["hour"]
    zip = row["ZIP"]
    last = row["last.cate_l1"] 
    if(is.na(last)) last= which(colnames(im.previous)=="NA")

    im.t = t(im.hour[1:9,hour]); colnames(im.t)=paste(colnames(im.t),"t",sep="_")
    im.s = t(im.zip[1:9,zip]); colnames(im.s)=paste(colnames(im.s),"s",sep="_")
    im.p = t(im.previous[1:9,last]); colnames(im.p)=paste(colnames(im.p),"p",sep="_")

    df = data.frame(row["cate_l1"]); colnames(df)="cate_l1"
    cbind(df, im.t, im.s, im.p)
    #data.frame(t(c(row["cate_l1"],im.t,im.s)))
}))

    

#gmodel = multinom(data=data.regress,cate_l1~.,maxit=1000)
gmodel.im = multinom(data=data.regress.im, cate_l1~., maxit=1000)

