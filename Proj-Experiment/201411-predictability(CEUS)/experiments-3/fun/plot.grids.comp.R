setwd("D:\\GitRepos\\work\\experiments\\experiments-3")
ppi=300

library(ggplot2)
library(gridExtra)
source("fun/grids.R")
source("fun/regress.job.R")
load("data/checkin_active.Rda")
print("successfully load the data")


cols = 10; rows = 9; ncls.grid = 12;
# new data frame
checkin.in.grids.4 = data.prepration(checkin.active)
tabs.grid = xtabs(data=checkin.in.grids.4,~cate_l1+ugrid.id)
tabs.grid.p = apply(tabs.grid,2,function(x){x/sum(x)})
png("dendrogram.png", width = 20*ppi, height = 10*ppi, res=ppi,bg = "transparent")
heat.grid = heatmap(tabs.grid.p,cexRow=0.8,cexCol=.8,margins = c(5,5),asp=1)
dev.off() 


# some configs#
cols = 10; rows = 9; ncls.grid = 10;
# new data frame
checkin.in.grids.1 = data.prepration(checkin.active)



cols = 40; rows = 36; ncls.grid = 10;
# new data frame
checkin.in.grids.2 = data.prepration(checkin.active)

cols = 40; rows = 36; ncls.grid = 20;
checkin.in.grids.3 = data.prepration(checkin.active)


data=unique(checkin.in.grids.1[,c("col.id","row.id","ugrid.id.cid")])
data$cls = round(as.numeric(data$ugrid.id.cid)/4)
gg1 <- ggplot(data=data, aes(x=col.id,y=row.id)) + 
    geom_tile(aes(fill=as.factor(cls)),color="white")+
#     annotate("text", label = data$ugrid.id.cid,  
#              x = data$col.id, y = data$row.id,
#              size = 2) +
    xlab("") + 
    theme(legend.position="none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), plot.background = element_blank(),
          axis.title.y=element_blank(),axis.title.x = element_text(size=10),
          axis.text=element_blank(),axis.ticks=element_blank())

data=unique(checkin.in.grids.2[,c("col.id","row.id","ugrid.id.cid")])
data$cls = round(as.numeric(data$ugrid.id.cid)/4)
gg2 <- ggplot(data=data, aes(x=col.id,y=row.id)) + 
    geom_tile(aes(fill=as.factor(cls)),color="white")+
#     annotate("text", label = data$ugrid.id.cid,  
#              x = data$col.id, y = data$row.id,
#              size = 2) +
    xlab("") + 
    theme(legend.position="none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), plot.background = element_blank(),
          axis.title.y=element_blank(),axis.title.x = element_text(size=10),
          axis.text=element_blank(),axis.ticks=element_blank())

data=unique(checkin.in.grids.3[,c("col.id","row.id","ugrid.id.cid")])
data$cls = round(as.numeric(data$ugrid.id.cid)/4)
gg3 <- ggplot(data=data, aes(x=col.id,y=row.id)) + 
    geom_tile(aes(fill=as.factor(cls)),color="white")+
#     annotate("text", label = data$ugrid.id.cid,  
#              x = data$col.id, y = data$row.id,
#              size = 2) +
    xlab("") + 
    theme(legend.position="none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), plot.background = element_blank(),
          axis.title.y=element_blank(),axis.title.x = element_text(size=10),
          axis.text=element_blank(),axis.ticks=element_blank())






nycb = readOGR(dsn="D:\\Experiments\\foursquare checkin data\\shapefile\\boundaries", 
               layer="NYC_borough_boundaries_WGS84")
nycb@data$id = rownames(nycb@data)
nycb.points = fortify(nycb, region="id")
nycb.df = join(nycb.points, nycb@data, by="id")
# compose grids
corners = c("left" = min(nycb.points$long), "right" = max(nycb.points$long),
            "top" =  max(nycb.points$lat),  "bottom" = min(nycb.points$lat) )
cols=10;rows=9
grids = compose.grids(corners, cols, rows)
col.size = (corners["right"]-corners["left"])/cols
row.size = (corners["top"]-corners["bottom"])/rows

gg.map<-ggplot() +
    
    geom_path(data=compose.grids.line(corners, cols, rows),
              aes(x=x, y=y,group=group),alpha=.5,size=.3,color="#DDDDDD")+
    
    geom_polygon(data=nycb.df,aes(long,lat,group=group),alpha=.1,color="grey") +
    
    #     geom_path(aes(x=lon.x, y=lat.x),color="#55B1F7",alpha=.8,size=.2)+
    geom_point(data=checkin.active[,c("lon.x","lat.x")],
               aes(x=lon.x, y=lat.x),
               color = "#55B1F7", alpha = 0.3, size=.5)+
    
    theme(legend.position="none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), plot.background = element_blank(),
          axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),
          plot.margin=unit(c(.05,.05,.05,.05),"npc"))
data=unique(checkin.in.grids.4[,c("col.id","row.id","ugrid.id.cid")])
gg4 <-ggplot(data=data, aes(x=col.id,y=row.id)) + 
    geom_tile(aes(fill=as.factor(ugrid.id.cid)),color="white")+
    annotate("text", label = data$ugrid.id.cid,  
             x = data$col.id, y = data$row.id,
             size = 2) +
    xlab("") + 
    scale_x_continuous(breaks=c(1:10),labels=c("001","002","003","004","005",
                                               "006","007","008","009","010"))+
    scale_y_continuous(breaks=c(1:9),labels=c("001","002","003","004","005",
                                               "006","007","008","009"))+
    theme(legend.position="none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), plot.background = element_blank(),
          axis.title.y=element_blank(),axis.title.x = element_text(size=10),
#           axis.text=element_blank(),axis.ticks=element_blank(),
          plot.margin=unit(c(.05,.05,.05,.05),"npc"))

png("compare_grids.png", width = 10*ppi, height = 4.5*ppi, res=ppi,bg = "transparent")
grid.arrange(gg1,gg2,ncol=2,widths=c(1,1))
dev.off()


png("compare_clusters.png", width = 10*ppi, height = 4.5*ppi, res=ppi,bg = "transparent")
grid.arrange(gg2,gg3,ncol=2,widths=c(1,1))
dev.off()

png("reduce-ranks.png", width = 4.5*ppi, height = 4.5*ppi, res=ppi,bg = "transparent")
gg4 
dev.off()
png("reduce-ranks-maps.png", width = 4.5*ppi, height = 4.5*ppi, res=ppi,bg = "transparent")
gg.map 
dev.off()