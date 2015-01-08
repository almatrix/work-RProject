library(vcd) # for chi-square and cramer's V
library(ggplot2)
library(reshape2)
source("grids.R")
source("regress.job.R")
load("checkin_active.Rda")

# configs
cols = 40
rows = 36
ncls.hour = 6
ncls.grid = 20
ncls.ia.h.w = -1
ncls.ia.g.l = -1
#

checkin.active.in.grids = data.prepration(checkin.active)
checkin.active.in.grids$ugrid.id = factor(checkin.active.in.grids$grid.id)

# cramer's V in matrix
# -|-------|-------|-------|-------|-------
# 5|   L1  |
# 4|       | hour  |
# 3|       |       |weekend|
# 2|       |       |       |  grid |
# 1|       |       |       |       |last_cate
# -|---1---|---2---|---3---|---4---|---5---
#
cramer.mat = matrix(data=rep(NA,25),ncol=5)


cramer.mat[4,1] = summary(assocstats(xtabs(~cate_l1 + hour.cid, 
                                           data = checkin.active.in.grids)+0.0001))$object$cramer


cramer.mat[3,1] = summary(assocstats(xtabs(~cate_l1 + isweekend, 
                                           data = checkin.active.in.grids)+0.0001))$object$cramer
cramer.mat[3,2] = summary(assocstats(xtabs(~hour.cid + isweekend, 
                                           data = checkin.active.in.grids)+0.0001))$object$cramer

cramer.mat[2,1] = summary(assocstats(xtabs(~cate_l1 + ugrid.id.cid, 
                                           data = checkin.active.in.grids)+0.0001))$object$cramer
cramer.mat[2,2] = summary(assocstats(xtabs(~hour.cid + ugrid.id.cid, 
                                           data = checkin.active.in.grids)+0.0001))$object$cramer
cramer.mat[2,3] = summary(assocstats(xtabs(~isweekend + ugrid.id.cid, 
                                           data = checkin.active.in.grids)+0.0001))$object$cramer


cramer.mat[1,1] = summary(assocstats(xtabs(tweight~cate_l1 + last.cate_l1, 
                                           data = checkin.active.in.grids)+0.0001))$object$cramer
cramer.mat[1,2] = summary(assocstats(xtabs(tweight~hour.cid+ last.cate_l1, 
                                           data = checkin.active.in.grids)+0.0001))$object$cramer
cramer.mat[1,3] = summary(assocstats(xtabs(tweight~isweekend + last.cate_l1, 
                                           data = checkin.active.in.grids)+0.0001))$object$cramer

cramer.mat[1,4] = summary(assocstats(xtabs(tweight~ugrid.id.cid + last.cate_l1, 
                                           data = checkin.active.in.grids)+0.0001))$object$cramer

colnames(cramer.mat)=c("L1","hour","weekend","grid","last.cate")
rownames(cramer.mat)=c("last.cate","grid","weekend","hour","L1")

cramer.melt = melt(t(cramer.mat))
cramer.melt.nona = cramer.melt[!is.na(cramer.melt$value),]



cramer.image <- ggplot(cramer.melt, aes(Var1,Var2)) + 
    geom_point(aes(size = value),color = "steelblue",alpha=0.7 )  +
    annotate("text", x = cramer.melt.nona$Var1, 
             y = cramer.melt.nona$Var2, 
             label=format(cramer.melt.nona$value,digits=2), 
             size=sqrt(cramer.melt.nona$value+0.05)*8) +
    annotate("text", label = c("Category","Hour","Weekday","Grid","Former"),  
             x = c("L1","hour","weekend","grid","last.cate"),
             y = c("L1","hour","weekend","grid","last.cate"),
             size = 3) +
    geom_path(data=compose.grids.line(corners=c("left"=0.5,"right"=5.5,"top"=5.5,"bottom"=0.5),cols=5,rows=5),
              aes(x=x, y=y,group=group),size=.1,color="grey90") +
    theme(axis.title = element_blank(),legend.position="none",
          axis.text=element_blank(),axis.ticks = element_blank(),
          panel.background = element_blank(), plot.background = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.margin=unit(c(0,0,0,0),"npc"))

png("correlation_12_40.png", width = 3.5*300, height = 2.5*300, res=300,bg = "transparent")
cramer.image
dev.off()  




### phi


phi.mat = matrix(data=rep(NA,25),ncol=5)


phi.mat[4,1] = summary(assocstats(xtabs(~cate_l1 + hour, 
                                           data = checkin.active.in.grids)+0.0001))$object$phi


phi.mat[3,1] = summary(assocstats(xtabs(~cate_l1 + isweekend, 
                                           data = checkin.active.in.grids)+0.0001))$object$phi
phi.mat[3,2] = summary(assocstats(xtabs(~hour + isweekend, 
                                           data = checkin.active.in.grids)+0.0001))$object$phi

phi.mat[2,1] = summary(assocstats(xtabs(~cate_l1 + ugrid.id, 
                                           data = checkin.active.in.grids)+0.0001))$object$phi
phi.mat[2,2] = summary(assocstats(xtabs(~hour + ugrid.id, 
                                           data = checkin.active.in.grids)+0.0001))$object$phi
phi.mat[2,3] = summary(assocstats(xtabs(~isweekend + ugrid.id, 
                                           data = checkin.active.in.grids)+0.0001))$object$phi


phi.mat[1,1] = summary(assocstats(xtabs(tweight~cate_l1 + last.cate_l1, 
                                           data = checkin.active.in.grids)+0.0001))$object$phi
phi.mat[1,2] = summary(assocstats(xtabs(tweight~hour+ last.cate_l1, 
                                           data = checkin.active.in.grids)+0.0001))$object$phi
phi.mat[1,3] = summary(assocstats(xtabs(tweight~isweekend + last.cate_l1, 
                                           data = checkin.active.in.grids)+0.0001))$object$phi

phi.mat[1,4] = summary(assocstats(xtabs(tweight~ugrid.id + last.cate_l1, 
                                           data = checkin.active.in.grids)+0.0001))$object$phi

colnames(phi.mat)=c("L1","hour","weekend","grid","last.cate")
rownames(phi.mat)=c("last.cate","grid","weekend","hour","L1")

phi.melt = melt(t(phi.mat))
phi.melt.nona = phi.melt[!is.na(phi.melt$value),]



phi.image <- ggplot(phi.melt, aes(Var1,Var2)) + 
    geom_point(aes(size = value),color = "steelblue",alpha=0.7 )  +
    annotate("text", x = phi.melt.nona$Var1, 
             y = phi.melt.nona$Var2, 
             label=format(phi.melt.nona$value,digits=2), 
             size=sqrt(phi.melt.nona$value+0.05)*8) +
    annotate("text", label = c("Category","Hour","Weekday","Grid","Former"),  
             x = c("L1","hour","weekend","grid","last.cate"),
             y = c("L1","hour","weekend","grid","last.cate"),
             size = 3) +
    geom_path(data=compose.grids.line(corners=c("left"=0.5,"right"=5.5,"top"=5.5,"bottom"=0.5),cols=5,rows=5),
              aes(x=x, y=y,group=group),size=.1,color="grey90") +
    theme(axis.title = element_blank(),legend.position="none",
          axis.text=element_blank(),axis.ticks = element_blank(),
          panel.background = element_blank(), plot.background = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.margin=unit(c(0,0,0,0),"npc"))




##### 
# this is a good library to compute all kinds of associations
# e.g., likelihood ratio, effect.size(contingency efficient), cramer's V, .. as the following
# and also lambda, uncertainty coefficient
library("polytomous")

uc.mat = matrix(data=rep(NA,25),ncol=5)
colnames(uc.mat)=c("Category","Hour","Weekday","Grid","Former")
rownames(uc.mat)=c("Category","Hour","Weekday","Grid","Former")

assoc = associations(xtabs(~cate_l1 + hour,data = checkin.active.in.grids))
uc.mat["Category","Hour"] = assoc$uc.RC * 100  #uncertainty reduction in Row (cate_l1) because of Column (hour)
uc.mat["Hour","Category"] = assoc$uc.CR * 100  #uncertainty reduction in Row (hour) because of Column (cate_l1)


assoc = associations(xtabs(~cate_l1 + isweekend,data = checkin.active.in.grids))
uc.mat["Category","Weekday"] = assoc$uc.RC * 100  #uncertainty reduction in Row (cate_l1) because of Column (weekday)
uc.mat["Weekday","Category"] = assoc$uc.CR * 100  #uncertainty reduction in Row (weekday) because of Column (cate_l1)

assoc = associations(xtabs(~hour + isweekend,data = checkin.active.in.grids))
uc.mat["Hour","Weekday"] = assoc$uc.RC * 100  
uc.mat["Weekday","Hour"] = assoc$uc.CR * 100  

assoc = associations(xtabs(~cate_l1 + ugrid.id,data = checkin.active.in.grids))
uc.mat["Category","Grid"] = assoc$uc.RC * 100  
uc.mat["Grid","Category"] = assoc$uc.CR * 100  


assoc = associations(xtabs(~hour + ugrid.id,data = checkin.active.in.grids))
uc.mat["Hour","Grid"] = assoc$uc.RC * 100 
uc.mat["Grid","Hour"] = assoc$uc.CR * 100 


assoc = associations(xtabs(~isweekend + ugrid.id,data = checkin.active.in.grids))
uc.mat["Weekday","Grid"] = assoc$uc.RC * 100 
uc.mat["Grid","Weekday"] = assoc$uc.CR * 100 

assoc = associations(xtabs(tweight~cate_l1 + last.cate_l1,data = checkin.active.in.grids))
uc.mat["Category","Former"] = assoc$uc.RC * 100 
uc.mat["Former","Category"] = assoc$uc.CR * 100  

assoc = associations(xtabs(tweight~hour + last.cate_l1,data = checkin.active.in.grids))
uc.mat["Hour","Former"] = assoc$uc.RC * 100 
uc.mat["Former","Hour"] = assoc$uc.CR * 100  

assoc = associations(xtabs(tweight~isweekend + last.cate_l1,data = checkin.active.in.grids))
uc.mat["Weekday","Former"] = assoc$uc.RC * 100 
uc.mat["Former","Weekday"] = assoc$uc.CR * 100  

assoc = associations(xtabs(tweight~ugrid.id  + last.cate_l1,data = checkin.active.in.grids))
uc.mat["Grid","Former"] = assoc$uc.RC * 100 
uc.mat["Former","Grid"] = assoc$uc.CR * 100  

uc.mat




