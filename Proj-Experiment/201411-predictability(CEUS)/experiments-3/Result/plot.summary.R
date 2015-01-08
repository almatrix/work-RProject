setwd("D:\\GitRepos\\work\\experiments\\experiments-3\\Result")
library(scales)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(plyr)

result = read.csv("result.csv",header=FALSE)
result[,1]=NULL
colnames(result) = c("t.prep","t.regress","edf","nsunits","rate","memory","cols","rows","ncls","job.id","group.id")
result = result[order(result$job.id),]

# add some columns for plotting
result$nfac = c(c(1:15),rep(5,14),c(1:7))
result$grids = result$cols * result$rows
result$time = (result$t.prep+result$t.regress)/60
result$memory = result$memory / 1024
result$rate = 100*result$rate

subresult1 = result[which(result$group.id==11),] 
subresult1$improve = subresult1$rate-c(29.11,subresult1$rate[1:(nrow(subresult1)-1)])
subresult2 = result[which(result$group.id==21),] 
subresult3 = result[which(result$group.id==31),] 
subresult4 = result[which(result$group.id==41),] 
subresult4$improve = subresult4$rate-c(29.11,subresult4$rate[1:(nrow(subresult4)-1)])
# plot
gg.f1.a = ggplot(subresult1,aes(x=nfac,y=memory)) + 
    geom_line() + geom_point(shape=21,aes(fill=sqrt(memory))) +  
    xlab("(a)")+ylab("Memory [GB]") + 
    scale_fill_gradient2(midpoint = I(sqrt(5)),low="green",high="red")+
    scale_x_continuous(breaks=c(1:15),limits=c(0.5,15.5),
                       labels=paste("M",c(1:15),sep=""))+
    scale_y_continuous(breaks=c(1:5),limits=c(0.5,5),
                       labels=paste("  ",c(1:5),sep=""))+
    ggtitle("Model Predictors (1)\n(40*36 Cells, 20 Ranks)\n")+
    theme(legend.position="none",plot.margin= unit(c(.1, 0,0,0.05), "npc"),
          axis.title = element_text(size=10),
          plot.title = element_text(size=10),
          axis.text = element_text(size=8))
gg.f1.b = ggplot(subresult1,aes(x=nfac,y=time)) + 
    geom_line() + geom_point(shape=21,aes(fill=time)) + 
    xlab("(b)")+ylab("Computing Time [min]") + 
    scale_fill_gradient2(midpoint = 30,low="green",high="red")+
    scale_y_continuous(breaks=c(20,40,60,80),
                       labels=c(20,40,60,80),limits=c(4.5,81))+
    scale_x_continuous(breaks=c(1:15),limits=c(0.5,15.5),
                       labels=paste("M",c(1:15),sep=""))+
    theme(legend.position="none",plot.margin= unit(c(.1, 0,0,0.05), "npc"),
          axis.title = element_text(size=10),
          plot.title = element_text(size=10),
          axis.text = element_text(size=8))
gg.f1.c = ggplot(subresult1,aes(x=nfac,y=rate)) + 
    geom_line() + geom_point(shape=21,aes(fill=rate)) +  
    xlab("(c)")+
    scale_fill_gradient2(midpoint = 45,low="red",high="green")+
    scale_x_continuous(breaks=c(1:15),limits=c(0.5,15.5),
                       labels=paste("M",c(1:15),sep=""))+
    scale_y_continuous(breaks=c(35,40,45,50),
                       labels=c(35,40,45,50),limits=c(35,50))+
    ylab("Prediction Precision [%]")  +
    annotate("rect", xmin = 0.6, xmax = 1.4, ymin = 35, ymax = 50,
             alpha = .6,fill="#31a354") + 
    annotate("rect", xmin = 1.6, xmax = 2.4, ymin = 35, ymax = 50,
             alpha = .5,fill="#31a354") + 
    annotate("rect", xmin = 14.6, xmax = 15.4, ymin = 35, ymax = 50,
             alpha = .4,fill="#31a354") + 
    annotate("rect", xmin = 10.6, xmax = 11.4, ymin = 35, ymax = 50,
             alpha = .3,fill="#31a354") +
    annotate("rect", xmin = 2.6, xmax = 3.4, ymin = 35, ymax = 50,
             alpha = .2,fill="#31a354") + 
    annotate("rect", xmin = 8.6, xmax = 9.4, ymin = 35, ymax = 50,
             alpha = .15,fill="#31a354") +
    annotate("rect", xmin = 5.6, xmax = 6.4, ymin = 35, ymax = 50,
             alpha = .1,fill="#31a354") +
    geom_text(aes(x=I(nfac-0.02), y = I(rate+1.5),
                  label = paste(round(subresult1$improve,digits=2),"↑",sep="")),
              size=2)+
    theme(legend.position="none",plot.margin= unit(c(.1, 0,0,0.05), "npc"),
          axis.title = element_text(size=10),
          plot.title = element_text(size=10),
          axis.text = element_text(size=8))

gg.f1.d = ggplot(data=subresult4,aes(x=nfac,y=memory)) + 
    geom_line() + geom_point(shape=21,aes(fill=sqrt(memory))) + 
    xlab("(d)")+ylab("") + 
    scale_fill_gradient2(name="Memory [GB]  ",midpoint = I(sqrt(5)),low="green",high="red")+
    scale_x_continuous(breaks=c(1:7),limits=c(0.5,7.5),
                       labels=c("M1","M2","M3'","M4'","M5'","M6'","M7'"))+
    scale_y_continuous(breaks=c(1:5),limits=c(0.5,5),
                       labels=paste("  ",c(1:5),sep=""))+
    ggtitle("Model Predictors (2) \n(40*36 Cells, 20 Ranks)\n")+
    theme(legend.position="none",plot.margin= unit(c(.1, 0.1,0,0), "npc"),
          axis.title = element_text(size=10),
          plot.title = element_text(size=10),
          axis.text = element_text(size=8))
gg.f1.e = ggplot(subresult4,aes(x=nfac,y=time)) + 
    geom_line() + geom_point(shape=21,aes(fill=time)) + 
    xlab("(e)")+ylab("") + 
    scale_fill_gradient2(name="Time [min]   ",midpoint = 30,low="green",high="red")+
    scale_x_continuous(breaks=c(1:7),limits=c(0.5,7.5),
                       labels=c("M1","M2","M3'","M4'","M5'","M6'","M7'"))+
    scale_y_continuous(breaks=c(20,40,60,80),
                       labels=c(20,40,60,80),limits=c(4.5,81))+
    theme(legend.position="none",plot.margin= unit(c(.1, 0.1,0,0), "npc"),
          axis.title = element_text(size=10),
          plot.title = element_text(size=10),
          axis.text = element_text(size=8))
gg.f1.f = ggplot(data=subresult4,aes(x=nfac,y=rate)) + 
    geom_line() + geom_point(shape=21,aes(fill=rate)) + 
    xlab("(f)")+
    scale_x_continuous(breaks=c(1:7),limits=c(0.5,7.5),
                       labels=c("M1","M2","M3'","M4'","M5'","M6'","M7'"))+
    scale_y_continuous(breaks=c(35,40,45,50),
                       labels=c(35,40,45,50),limits=c(35,50))+
    ylab("")  +
    scale_fill_gradient2(name="Precision [%]",midpoint = 45,low="red",high="green")+
    geom_text(aes(x=I(nfac-0.02), y = I(rate+1.5),
                  label = paste(round(subresult4$improve,digits=2),"↑",sep="")),
              size=2)+
    theme(legend.position="none",plot.margin= unit(c(.1, 0.1,0,0), "npc"),
          axis.title = element_text(size=10),
          plot.title = element_text(size=10),
          axis.text = element_text(size=8))

png("summary1.png", width =7*300, height = 5*300, res=300,bg = "transparent")
grid.arrange(gg.f1.a,gg.f1.d,
             gg.f1.b,gg.f1.e,
             gg.f1.c,gg.f1.f,
             ncol=2, nrow=3, heights=c(1.29,1,1),widths=c(1.6,1))
dev.off()




gg.f2.a = ggplot(subresult2,aes(x=grids,y=memory)) + 
    geom_line() + geom_point(shape=21,aes(fill=memory)) + 
    xlab("(a)")+ylab("Memory [GB]") +  
    ggtitle("Number of Cells \n(Model M5', 20 Ranks)\n")+
    scale_fill_gradient2(name="Memory [GB]",midpoint = 5,low="green",high="red")+
    scale_x_sqrt(breaks=c(90, 360, 810, 1440, 2250, 3240, 
                          4410, 5760, 7290, 9000,10890,12960,15120,17640,20250),
                 labels =c("10*9","","","40*36","","",
                           "70*63","","","100*90","","",
                           "130*63","",""))+
    scale_y_continuous(limits=c(0.6,22))+
    theme(legend.position="none",
          plot.margin= unit(c(.1, 0,0,0.05), "npc"),
          legend.text=element_text(size=6),legend.title=element_text(size=6), 
          axis.title = element_text(size=10),
          plot.title = element_text(size=10),
          axis.text = element_text(size=8))
gg.f2.b = ggplot(subresult2,aes(x=grids,y=time)) + 
    geom_line() + geom_point(shape=21,aes(fill=time)) + 
    xlab("(b)")+ylab("Computing Time [min]") + 
    ylim(c(15,52))+
    scale_fill_gradient2(name="Time [min]",midpoint = 30,low="green",high="red")+
    scale_x_sqrt(breaks=c(90, 360, 810, 1440, 2250, 3240, 
                          4410, 5760, 7290, 9000,10890,12960,15120,17640,20250),
                 labels =c("10*9","","","40*36","","",
                           "70*63","","","100*90","","",
                           "130*63","",""))+
    theme(legend.position="none",
          plot.margin= unit(c(.1, 0,0,0.05), "npc"),
          legend.text=element_text(size=6),legend.title=element_text(size=6), 
          axis.title = element_text(size=10),
          plot.title = element_text(size=10),
          axis.text = element_text(size=8))
gg.f2.c = ggplot(subresult2,aes(x=grids,y=rate)) + 
    geom_line() + geom_point(shape=21,aes(fill=rate)) + 
    xlab("(c)")+
    ylab("Prediction Precision [%]")  +
    scale_x_sqrt(breaks=c(90, 360, 810, 1440, 2250, 3240, 
                          4410, 5760, 7290, 9000,10890,12960,15120,17640,20250),
                 labels =c("10*9","","","40*36","","",
                           "70*63","","","100*90","","",
                           "130*63","",""))+
    scale_y_continuous(limits=c(37,58))+
    scale_fill_gradient2(name="Precision [%]",midpoint = 45,low="red",high="green")+
    theme(legend.position="none",
          plot.margin= unit(c(.1, 0,0,0.05), "npc"),
          legend.text=element_text(size=6),legend.title=element_text(size=6), 
          axis.title = element_text(size=10),
          plot.title = element_text(size=10),
          axis.text = element_text(size=8))
    

gg.f3.a = ggplot(subresult3,aes(x=ncls,y=memory)) + 
    geom_line() + geom_point(shape=21,aes(fill=sqrt(memory))) + 
    xlab("(d)")+ylab("") + 
    ylim(c(0.6,22))+
    scale_fill_gradient2(midpoint = I(sqrt(5)),low="green",high="red")+
    ggtitle("Number of Ranks \n(Model M5', 40*36 Cells)\n")+
    theme(legend.position="none",plot.margin= unit(c(.1, 0.1,0,0), "npc"),
          legend.text=element_text(size=6),legend.title=element_text(size=8), 
          axis.title = element_text(size=10),
          plot.title = element_text(size=10),
          axis.text = element_text(size=8))
gg.f3.b = ggplot(subresult3,aes(x=ncls,y=time))  + 
    geom_line() + geom_point(shape=21,aes(fill=time)) + 
    xlab("(e)")+ylab("") + 
    ylim(c(15,52))+
    scale_fill_gradient2(name="Time [min]   ",midpoint = 30,low="green",high="red")+
    theme(legend.position="none",plot.margin= unit(c(.1, 0.1,0,0), "npc"),
          legend.text=element_text(size=6),legend.title=element_text(size=8), 
          axis.title = element_text(size=10),
          plot.title = element_text(size=10),
          axis.text = element_text(size=8))
gg.f3.c = ggplot(subresult3,aes(x=ncls,y=rate)) + 
    geom_line() + geom_point(shape=21,aes(fill=rate)) + 
    xlab("(f)")+
    ylab("")  +
    scale_fill_gradient2(name="Precision [%]",midpoint = 45,low="red",high="green")+
    scale_y_continuous(limits=c(37,58))+
    theme(legend.position="none",plot.margin= unit(c(.1, 0.1,0,0), "npc"),
          legend.text=element_text(size=6),legend.title=element_text(size=6), 
          axis.title = element_text(size=10),
          plot.title = element_text(size=10),
          axis.text = element_text(size=8))


png("summary2.png", width =7*300, height = 5*300, res=300,bg = "transparent")
grid.arrange(gg.f2.a,gg.f3.a,
             gg.f2.b,gg.f3.b,
             gg.f2.c,gg.f3.c,
             ncol=2, nrow=3, heights=c(1.29,1,1),widths=c(1.6,1))
dev.off()


