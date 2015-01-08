library(ggplot2)
library(reshape2)
library(gridExtra)
setwd("D:\\GitRepos\\work\\experiments\\experiments-2")

summary1 = read.csv( "job1.summary.txt", header=TRUE, sep=",")
summary2 = read.csv( "job2.summary.csv", header=TRUE, sep=",")
summary3 = read.csv( "job3.summary.csv", header=TRUE, sep=",")

colnames(summary1)[1]="Predictors"
colnames(summary2)[1]="Grids"
colnames(summary3)[1]="Clusters"

gg.f1.1 = ggplot(summary1,aes(x=Predictors,y=Variables)) + 
    geom_line() + geom_point(shape=21) + 
    xlab("(a)")+ylab("Regression\nVariables") + 
    ylim(c(100,550))+
    theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())
gg.f1.2 = ggplot(summary1,aes(x=Predictors,y=Computing)) + 
    geom_line() + geom_point(shape=21) + 
    xlab("(b)")+ylab("Computing\nTime [s]") + 
    ylim(c(360,1400))+
    theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())
gg.f1.3 = ggplot(summary1,aes(x=Predictors,y=Precision)) + 
    geom_line() + geom_point(shape=21) + 
    ylim(c(0.38,0.44))+
    xlab("(c)\n\nNumber of Predictors")+ylab("Prediction\nPrecision")  


gg.f2.1 = ggplot(summary2,aes(x=Grids,y=Variables)) +
    geom_line() + geom_point(shape=21) +
    xlab("(d)")+ ylab("") + ylim(c(100,550))+
    theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())+
    scale_x_sqrt()
gg.f2.2 = ggplot(summary2,aes(x=Grids,y=Computing)) +
    geom_line() + geom_point(shape=21) +  
    xlab("(e)")+ylab("") + ylim(c(360,1400))+
    theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())+
    scale_x_sqrt()
gg.f2.3 = ggplot(summary2,aes(x=Grids,y=Precision)) +
    geom_line() + geom_point(shape=21) + 
    xlab("(f)\n\nNumber of Grids")+ylab("")  +ylim(c(0.38,0.44))+
    scale_x_sqrt()

gg.f3.1 = ggplot(summary3,aes(x=Clusters,y=Variables)) +
    geom_line() + geom_point(shape=21) +  
    xlab("(g)")+ylab("") + ylim(c(100,550))+
    theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())
gg.f3.2 = ggplot(summary3,aes(x=Clusters,y=Computing)) +
    geom_line() + geom_point(shape=21) + 
    xlab("(h)")+ylab("") + ylim(c(360,1400))+
    theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())
gg.f3.3 = ggplot(summary3,aes(x=Clusters,y=Precision)) +
    geom_line() + geom_point(shape=21) + ylim(c(0.38,0.44))+
    xlab("(i)\n\nNumber of Grid Clusters")+ylab("")  


png("summary.png", width =8*300, height = 6*300, res=300,bg = "transparent")
grid.arrange(gg.f1.1,gg.f2.1,gg.f3.1,
             gg.f1.2,gg.f2.2,gg.f3.2,
             gg.f1.3,gg.f2.3,gg.f3.3,
             ncol=3, nrow=3, heights=c(1,1,1),widths=c(1,1,1))
dev.off()

rm(summary1,summary2,summary3)
rm(gg.f1.1,gg.f1.2,gg.f1.3)
rm(gg.f2.1,gg.f2.2,gg.f2.3)
rm(gg.f3.1,gg.f3.2,gg.f3.3)
