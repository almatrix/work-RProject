library(ggplot2)
library(reshape2)
library(gridExtra)

# setwd("D:\\GitRepos\\work\\records\\experiments-2\\JOB1d")

basedir = "JOB1d/"
load(paste0(basedir,"tmodel1.Rda"))
load(paste0(basedir,"p1.Rda"))
# source(paste0(basedir,"conf.R"))

# #################### configurations ########################
ncls.hour = 6
ncls.grid = 40
ncls.ia.h.w = 10
ncls.ia.g.l = 20
nparas = (ncls.hour-1) + 2 + (ncls.grid-1) + 9 #+ 
#     (ncls.ia.h.w-1) + (ncls.ia.g.l-1) 

# #############################################################




# invesigate into the model
prediction = apply(tmodel1$fitted.values, 1, FUN=function(i)which(i==max(i)))
real=checkin.active.in.grids$cate_l1
comparison = data.frame("real"=real,"prediction"=levels(real)[prediction])
comparison$correct = ifelse(comparison$prediction==comparison$real,1,0)
rate=sum(comparison$correct)/nrow(comparison)
print(paste(sum(comparison$correct), "correct predictions out of",
            nrow(comparison),". Rate:",rate))


# plot 
image<-ggplot(melt(p1), aes(Var2,Var1)) + 
    geom_tile(aes(fill = sqrt(value)), colour = "white")+
    scale_fill_gradient2(name="p value",midpoint = 0.2, high="red",low="green",
                         breaks=c(0,sqrt(0.05),sqrt(0.1),sqrt(0.25),sqrt(1)),
                         labels=c(0,0.05,0.1,0.25,1))+
    theme(axis.title = element_blank(),legend.position="top",
          axis.text.y=element_text(size=6,color="black"),
          axis.text.x  = element_text(angle=45, vjust=1, hjust=1,size=5,color="black"),
          legend.title = element_text( size=6),legend.text = element_text(angle=45,size = 6),
          #           axis.text=element_blank(),
          panel.background = element_blank(), plot.background = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.margin=unit(c(.05,.05,.05,.05),"npc"))


top <-ggplot(melt(p1))+geom_boxplot(aes(Var2,value),outlier.size=.5,outlier.colour="grey")+
    scale_y_sqrt(breaks=c(0,0.05,0.1,0.25,1),labels=c(0,0.05,0.1,0.25,1))+
    theme(axis.title = element_blank(),
          axis.text.x=element_blank(),axis.text.y=element_text(size=5,color="black"),
          plot.margin=unit(c(.05,.05,.05,.05),"npc"))

right<-ggplot(melt(p1))+geom_boxplot(aes(Var1,value),outlier.size=.5,outlier.colour="grey")+
    scale_y_sqrt( breaks=c(0,0.05,0.1,0.25,1),labels=c(0,0.05,0.1,0.25,1))+
    theme(axis.title = element_blank(),
          axis.text.y=element_blank(),axis.text.x=element_text(size=5,color="black",angle=35, vjust=1,hjust=1),
          plot.margin=unit(c(.05,.05,.05,.05),"npc"))+
    coord_flip()



png(paste0(basedir,"p1.png"), width =33*(nparas+1)+531, height = 1050, res=300,bg = "transparent")
image
dev.off()
png(paste0(basedir,"top.png"),width =33*(nparas+1)+531, height = 350, res=300,bg = "transparent")
top
dev.off()
png(paste0(basedir,"right.png"), width = 325, height = 400, res=300,bg = "transparent")
right
dev.off()




rediduals = tmodel1$residuals
png(paste0(basedir,"residuals.png"), width = 1800, height = 900, res=300,bg = "white")
ggplot(melt(rediduals))+geom_boxplot(aes(Var2,value),outlier.size=.5,outlier.colour="grey")+
    ylab("Residuals")+
    #     scale_y_sqrt(breaks=c(0,0.05,0.1,0.25,1),labels=c(0,0.05,0.1,0.25,1))+
    theme(#axis.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y=element_text(size=6,color="black"),
        axis.text.x  = element_text(angle=30, vjust=1, hjust = 1,size=6,color="black"),
        plot.margin=unit(c(.05,.05,.05,.05),"npc"))
dev.off()

rm(p1,tmodel1,image,top,right,rediduals)
