data=read.csv("time_series_result.csv")

data=data[,c(1,2,3,4,5,10)]
colnames(data)=c("x.var.cate","x.var.time","y.var.cate","y.var.time","MIC","p")
data$x.var = paste(data$x.var.cate,data$x.var.time,sep="_")
data$y.var = paste(data$y.var.cate,data$y.var.time,sep="_")

factorlevels = as.factor(sort(unique(c(data$x.var,data$y.var))))
data$x.var = factor(data$x.var,levels=factorlevels)
data$y.var = factor(data$y.var,levels=factorlevels)
data$r2=(data$p)^2

data$highMIC = ifelse(data$MIC>quantile(data$MIC,0.75),"H",
                      ifelse(data$MIC<quantile(data$MIC,0.25),"L","M"))
data$highp = ifelse(data$p>quantile(data$p,0.75),"H",
                    ifelse(data$p<quantile(data$p,0.25),"L","M"))
data$moran = ifelse((data$highMIC!="M"&data$highp!="M"),
                    paste(data$highMIC,data$highp,sep="-"),
                    "Not significant")

data$moran = factor(data$moran, levels=as.factor(c("H-H","H-L","L-H","L-L","Not significant")))


png("test1.png",width=1500, height=1200,res=300)
ggplot(data)+
    geom_tile(aes(x=x.var, y=y.var,fill = MIC), colour = "white") +
    geom_tile(aes(x=y.var, y=x.var,fill = MIC), colour = "white") +
    scale_fill_gradient(low="white",high="#F3030C")+
    theme(axis.title = element_blank(),
          axis.text.y=element_text(color="black",size=5),
          axis.text.x  = element_text(angle=30, vjust=1, hjust = 1, size=5,color="black"),
          legend.title = element_text(size=5),legend.text = element_text(size = 5),
          panel.background = element_blank(), plot.background = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.margin=unit(c(.05,.05,.05,.05),"npc"))
dev.off()

png("test2.png",width=1500, height=1200,res=300)
ggplot(data) + 
    geom_tile(aes(x=x.var, y=y.var,fill = p), colour = "white") +
    geom_tile(aes(x=y.var, y=x.var,fill = p), colour = "white") +
    scale_fill_gradient(low="white",high="#0071FE")+
    theme(axis.title = element_blank(),
          axis.text.y=element_text(color="black",size=5),
          axis.text.x  = element_text(angle=30, size=5,vjust=1, hjust = 1, color="black"),
          legend.title = element_text(size=5),legend.text = element_text(size = 5),
          panel.background = element_blank(), plot.background = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.margin=unit(c(.05,.05,.05,.05),"npc"))
dev.off()

png("test3.png",width=1600, height=1200,res=300)
ggplot(data) + 
    geom_tile(aes(x=x.var, y=y.var,fill = moran), colour = "white") +
    geom_tile(aes(x=y.var, y=x.var,fill = moran), colour = "white") +
    scale_fill_manual(values=c("#1b7837","#FCBFBE","#71B3FF","#E2E2E2")) + 
    theme(axis.title = element_blank(),
          axis.text.y=element_text(color="black",size=5),
          axis.text.x  = element_text(angle=30,vjust=1, hjust = 1, size=5, color="black"),
          legend.title = element_text(size=5),legend.text = element_text(size = 5),
          panel.background = element_blank(), plot.background = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.margin=unit(c(.05,.05,.05,.05),"npc"))
dev.off()


subdata = data[data$x.var.cate==data$y.var.cate,]
png("WD vs WE.png",width=1500, height=1200,res=300)
ggplot(subdata)+
    geom_point(aes(x=rep(x.var.cate,2),y=c(MIC,p),color=c(rep("MIC",10),rep("p",10))),
               shape=21)+
    theme(axis.title = element_blank(),
          axis.text.y=element_text(color="black",size=5),
          axis.text.x  = element_text(angle=30,vjust=1, hjust = 1, size=5, color="black"),
          legend.title = element_blank(),legend.text = element_text(size = 5),
#           panel.background = element_blank(), plot.background = element_blank(),
#           panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.margin=unit(c(.05,.05,.05,.05),"npc"))
dev.off()


