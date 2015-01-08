library(ggplot2)
library(gridExtra)

# add some columns for plotting
result$group = c(rep(1,5),rep(2,10),rep(3,5),rep(4,5),rep(1,5))
result$nfac = c(1:5,rep(4,20),6:10)
result$grids = result$cols * result$rows


# plot
gg.f1.a = ggplot(result[which(result$group==1),],aes(x=nfac,y=nsunits)) + 
    geom_line() + geom_point(shape=21) + 
    scale_x_continuous(breaks=c(1:10),labels=(1:10))+
    xlab("(a)")+ylab("Model\nVariables") + 
    ylim(c(10,130))+
    scale_x_continuous(breaks=c(1:10),
                       labels=c("~H","+G","+S","+W",
                                "+(H:G)","+(H:S)","(H:W)",
                                "+(G:S)","+(G:W)","+(S:W)"))+
    ggtitle("Number of Predictors \n(60*54 Grids, 20 Grid Clusters)\n")+
    theme(axis.title = element_text(size=10),
          plot.title = element_text(size=10))
# gg.f1.b = ggplot(result[which(result$group==1),],aes(x=nfac,y=edf)) + 
#     geom_line() + geom_point(shape=21) + 
#     xlab("(b)")+ylab("Effective DF []") + 
#     ylim(c(230,700))+
#     theme(axis.title = element_text(size=10),
#           plot.title = element_text(size=11))
gg.f1.b = ggplot(result[which(result$group==1),],aes(x=nfac,y=t.regress)) + 
    geom_line() + geom_point(shape=21) + 
    xlab("(b)")+ylab("Computing\nTime [s]") + 
    ylim(c(270,3900))+
    scale_x_continuous(breaks=c(1:10),
                       labels=c("~H","+G","+S","+W",
                                "+(H:G)","+(H:S)","(H:W)",
                                "+(G:S)","+(G:W)","+(S:W)"))+
    theme(axis.title = element_text(size=10),
          plot.title = element_text(size=11))
gg.f1.c = ggplot(result[which(result$group==1),],aes(x=nfac,y=rate)) + 
    geom_line() + geom_point(shape=21) + 
    ylim(c(0.36,0.5))+
    xlab("(c)")+
    scale_x_continuous(breaks=c(1:10),
                       labels=c("~H","+G","+S","+W",
                                "+(H:G)","+(H:S)","(H:W)",
                                "+(G:S)","+(G:W)","+(S:W)"))+
    ylab("Prediction\nPrecision")  +
    theme(axis.title = element_text(size=10),
          plot.title = element_text(size=11))

gg.f2.a = ggplot(result[which(result$group==2),],aes(x=grids,y=nsunits)) + 
    geom_line() + geom_point(shape=21) + 
    xlab("(d)")+ylab("") + 
    ylim(c(25,100))+ 
    ggtitle("Number of Grids \n(4 Predictors, 20 Grid Clusters)\n")+
    scale_x_sqrt(breaks=c(90, 360, 810, 1440, 2250, 3240, 
                         4410, 5760, 7290, 9000),
                 labels =c("10*9","","","40*36","","",
                           "70*63","","","100*90"))+
    theme(axis.title = element_text(size=10),
          plot.title = element_text(size=10))
# gg.f2.b = ggplot(result[which(result$group==2),],aes(x=grids,y=edf)) + 
#     geom_line() + geom_point(shape=21) + 
#     xlab("(f)")+ylab("") + 
#     scale_x_sqrt(breaks=c(90, 360, 810, 1440, 2250, 3240, 
#                           4410, 5760, 7290, 9000),
#                  labels =c("10*9","","","40*36","","",
#                            "70*63","","","100*90"))+    ylim(c(230,700))+
#     theme(axis.title = element_text(size=10),
#           plot.title = element_text(size=11))
gg.f2.b = ggplot(result[which(result$group==2),],aes(x=grids,y=t.regress)) + 
    geom_line() + geom_point(shape=21) + 
    xlab("(e)")+ylab("") + 
    ylim(c(750,3200))+ 
    scale_x_sqrt(breaks=c(90, 360, 810, 1440, 2250, 3240, 
                          4410, 5760, 7290, 9000),
                 labels =c("10*9","","","40*36","","",
                           "70*63","","","100*90"))+    
    theme(axis.title = element_text(size=10),
          plot.title = element_text(size=11))
gg.f2.c = ggplot(result[which(result$group==2),],aes(x=grids,y=rate)) + 
    geom_line() + geom_point(shape=21) + 
    ylim(c(0.37,0.5))+
    xlab("(f)")+
    ylab("")  + 
    scale_x_sqrt(breaks=c(90, 360, 810, 1440, 2250, 3240, 
                          4410, 5760, 7290, 9000),
                 labels =c("10*9","","","40*36","","",
                           "70*63","","","100*90"))+
    theme(axis.title = element_text(size=10),
          plot.title = element_text(size=11))
    

gg.f3.a = ggplot(result[which(result$group==3),],aes(x=ncls.grid,y=nsunits)) + 
    geom_line() + geom_point(shape=21) + 
    xlab("(g)")+ylab("") + 
    ylim(c(25,100))+
    ggtitle("Number of Grid Clusters \n(4 Predictors, 40*36 Grids)\n")+
    theme(axis.title = element_text(size=10),
          plot.title = element_text(size=10))
# gg.f3.b = ggplot(result[which(result$group==3),],aes(x=ncls.grid,y=edf)) + 
#     geom_line() + geom_point(shape=21) + 
#     xlab("(j)")+ylab("") + 
#     ylim(c(230,700))+
#     theme(axis.title = element_text(size=10),
#           plot.title = element_text(size=11))
gg.f3.b = ggplot(result[which(result$group==3),],aes(x=ncls.grid,y=t.regress)) + 
    geom_line() + geom_point(shape=21) + 
    xlab("(h)")+ylab("") + 
    ylim(c(750,3200))+
    theme(axis.title = element_text(size=10),
          plot.title = element_text(size=11))
gg.f3.c = ggplot(result[which(result$group==3),],aes(x=ncls.grid,y=rate)) + 
    geom_line() + geom_point(shape=21) + 
    ylim(c(0.37,0.5))+
    xlab("(i)")+
    ylab("")  +
    theme(axis.title = element_text(size=10),
          plot.title = element_text(size=11))

gg.f4.a = ggplot(result[which(result$group==4),],aes(x=ncls.grid,y=nsunits)) + 
    geom_line() + geom_point(shape=21) + 
    xlab("(j)")+ylab("") + 
    ylim(c(25,100))+
    ggtitle("Number of Grid Clusters \n(4 Predictors, 80*72 Grids)\n")+
    theme(axis.title = element_text(size=10),
          plot.title = element_text(size=10))
# gg.f4.b = ggplot(result[which(result$group==4),],aes(x=ncls.grid,y=edf)) + 
#     geom_line() + geom_point(shape=21) + 
#     xlab("(n)")+ylab("") + 
#     ylim(c(230,700))+
#     theme(axis.title = element_text(size=10),
#           plot.title = element_text(size=11))
gg.f4.b = ggplot(result[which(result$group==4),],aes(x=ncls.grid,y=t.regress)) + 
    geom_line() + geom_point(shape=21) + 
    xlab("(k)")+ylab("") + 
    ylim(c(750,3200))+
    theme(axis.title = element_text(size=10),
          plot.title = element_text(size=11))
gg.f4.c = ggplot(result[which(result$group==4),],aes(x=ncls.grid,y=rate)) + 
    geom_line() + geom_point(shape=21) + 
    ylim(c(0.37,0.5))+
    xlab("(l)")+
    ylab("")  +
    theme(axis.title = element_text(size=10),
          plot.title = element_text(size=11))



png("summary.png", width =10*300, height = 6*300, res=300,bg = "transparent")
grid.arrange(gg.f1.a,gg.f2.a,gg.f3.a,gg.f4.a,
             gg.f1.b,gg.f2.b,gg.f3.b,gg.f4.b,
             gg.f1.c,gg.f2.c,gg.f3.c,gg.f4.c,
#              gg.f1.d,gg.f2.d,gg.f3.d,gg.f4.d,
             ncol=4, nrow=3, heights=c(1.29,1,1),widths=c(1.1,1,1,1))
dev.off()


