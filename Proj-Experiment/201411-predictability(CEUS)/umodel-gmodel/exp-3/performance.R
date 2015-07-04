library(plyr)
library(ggplot2)
library(gridExtra) # grid.arrange
library(reshape2) # melt

base.array=c(0.001,0.005,0.01,0.05,0.1)
# all ratio is set as 0.8
result.base <- do.call(rbind,lapply(1:60,function(job.id){
    file.name = paste("../exp-4/result/prediction_job",job.id,".Rda",sep="")
    load(file.name)
    
    prediction$learning.ratio = 0.8
    prediction$weight.base = base.array[ceiling(job.id / 12)]
    
    prediction$alpha = with(prediction, 
                            atan(usermodel/(globalmodel+1e-20))/pi*180)
    
    prediction#[,c(1,2,3,4,5,19,20,21)]
}))
rm(base.array)
# ajusted accuracy
result.base$usermodel.adj=with(result.base,
                               (usermodel*predicting.size+2)/(predicting.size+4))
result.base$globalmodel.adj=with(result.base,
                               (globalmodel*predicting.size+2)/(predicting.size+4))
result.base$alpha.adj=with(result.base, 
                           atan(usermodel.adj/(globalmodel.adj+1e-20))/pi*180)

ratio.array=c(0.9,0.7,0.6,0.5)
# all exponential base is set as 0.001
result.ratio <- do.call(rbind,lapply(1:48,function(job.id){
        file.name = paste("../exp-3/result/prediction_job",job.id,".Rda",sep="")
        load(file.name)
        
        #prediction$learning.ratio = 1-0.1*ceiling(job.id / 12)
        prediction$learning.ratio = ratio.array[ceiling(job.id / 12)]
        prediction$weight.base = 0.001
        
        prediction$alpha = with(prediction, 
                                atan(usermodel/(globalmodel+1e-20))/pi*180)
        
        prediction[,c(1,2,3,4,5,19,20,21)]
}))
rm(ratio.array)
# copy the duplicated part from result.base to result.ratio
result.base.0.8.0.001 = result.base[which(result.base$weight.base==0.001),c(1:8)]
result.ratio <- rbind(result.ratio, result.base.0.8.0.001)
rm(result.base.0.8.0.001)
# ajusted accuracy
result.ratio$usermodel.adj=with(result.ratio,
                               (usermodel*predicting.size+2)/(predicting.size+4))
result.ratio$globalmodel.adj=with(result.ratio,
                                 (globalmodel*predicting.size+2)/(predicting.size+4))
result.ratio$alpha.adj=with(result.ratio, 
                           atan(usermodel.adj/(globalmodel.adj+1e-20))/pi*180)



result.base.ug<-melt(result.base[,c(1,4,7,9,10)],id.vars=c(1,2,3)) 
gg1<-ggplot(result.base.ug[which(result.base.ug$learning.size>30),],
       aes(x=learning.size,y=value,
           group=as.factor(weight.base),color=as.factor(weight.base)))+
    geom_point()+
    geom_smooth(method="lm",se=F,size=1)+
    scale_y_continuous(limit=c(0,1))+
    scale_x_log10()+
    facet_wrap(~variable)+
    theme_bw() %+replace% 
    theme(legend.position="none")  # no lengend

gg2<-ggplot(result.base[which(result.base$learning.size>30),],
       aes(x=learning.size,y=alpha,color=weight.base,group=weight.base))+
    geom_point()+
    geom_smooth(method="lm",se=F,size=1)+
    geom_hline(linetype="dashed",yintercept=45)+
#     scale_y_continuous(limit=c(0,1))+
    scale_x_log10()+
    theme_bw()

grid.arrange(gg1,gg2,nrow=1,widths=c(2,1.3))



result.ratio.ug<-melt(result.ratio[,c(1,4,6,9,10)],id.vars=c(1,2,3)) 
gg3<-ggplot(result.ratio.ug[which(result.ratio.ug$learning.size>30),],
            aes(x=learning.size,y=value,
                group=as.factor(learning.ratio),
                color=as.factor(learning.ratio)))+
    geom_point()+
    geom_smooth(method="lm",se=F,size=1)+
#     scale_y_continuous(limit=c(0,1))+
    scale_x_log10()+
    facet_wrap(~variable)+
    theme_bw() %+replace% 
    theme(legend.position="none")  # no lengend

gg4<-ggplot(result.ratio[which(result.ratio$learning.size>30),],
            aes(x=learning.size,y=alpha.adj,
                color=as.factor(learning.ratio),
                group=as.factor(learning.ratio)))+
#     geom_point()+
    geom_smooth(method="lm",se=F,size=1)+
#     geom_hline(linetype="dashed",yintercept=45)+
    #     scale_y_continuous(limit=c(0,1))+
    scale_x_log10()+
    theme_bw()

grid.arrange(gg3,gg4,nrow=1,widths=c(2,1.3))

scatter.regression.matrix<-function(data,xs,ys,xnames=NA,ynames=NA,weight=NA){
    # 0. deal with NA parameters
    if(is.na(xnames)){xnames=xs}
    if(is.na(ynames)){ynames=ys}
    
    # 1. decide panel color
    data.r <- abs(cor(data[,c(xs,ys)]))[xs,ys]    
    rbPal <- c("#F4BBDD","#FDFFDA","#D2F4F2")
    data.col <- rbPal[as.numeric(cut(as.vector(data.r),breaks = 3))]
    data.col <- matrix(data.col,nrow=length(xs),byrow=F,dimnames=dimnames(data.r))
    
    # 2. shared part of the theme
    theme_shared <- theme_bw(base_size = 12) %+replace% 
        theme(legend.position="none",  # no lengend
              plot.margin =unit(c(0.03,0.03,-0.05,-0.05), "npc"), # margin
              panel.grid= element_blank(), # no grid
              axis.text.y=element_text(angle=90)) # rotate y axis labels
    
    # 3. scatter plot for each x and y column
    regression.matrix <-lapply(1:length(xs),function(i){
        x = xs[i]
        
        lapply(1:length(ys),function(j){
            y = ys[j]
            
            ######## 
            # deal with the theme of each matrix cell
            # add background color based on the strength of correlation
            theme_cell <- theme_shared %+replace%
                theme(panel.background=element_rect(fill=data.col[x,y]))
            # remove y axis for non-first column
            if(i!=1){
                theme_cell <- theme_cell %+replace% 
                    theme(axis.ticks.y=element_blank(),
                          axis.text.y=element_blank())
            }
            # remove x axis for non-last row
            if(j!=length(ys)){
                theme_cell <- theme_cell %+replace% 
                    theme(axis.ticks.x=element_blank(),
                          axis.text.x=element_blank())
            }
            
            
            #######
            # plot each matrix cell
            ggplot(data, aes_string(x=x, y=y, weight=weight)) +
                geom_point(shape="*")+
                geom_smooth(method="lm",color="blue")+
                geom_smooth(method="loess",color="red",se=F)+
                labs(x="",y="")+
                theme_cell
        })
    })
    
    # 4. density for each x and y
    x.density <- lapply(1:length(xs),function(i){
        x=xs[i]
        theme_x <- theme_shared %+replace% 
            theme(panel.background=element_blank(), 
                  axis.ticks.x=element_blank(),
                  axis.text.x=element_blank(),
                  panel.border=element_rect(color="grey50",fill=NA))
        if(i!=1){
            theme_x <- theme_x %+replace% 
                theme(axis.ticks.y=element_blank(),
                      axis.text.y=element_blank())
        }
        ggplot(data, aes_string(x=x)) +
            stat_density(geom = "line")+
            annotation_custom(textGrob(x,gp = gpar(fontsize = 20)), 
                              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)+ 
            labs(x="",y="")+
            theme_x
    })
    
    y.density <- lapply(1:length(ys),function(j){
        y=ys[j]
        theme_y <- theme_shared %+replace% 
            theme(panel.background=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.text.y=element_blank(),
                  panel.border=element_rect(color="grey50",fill=NA),
                  plot.margin =unit(c(0.03,0.03,-0.05,-0.1), "npc"))
        if(j!=length(ys)){
            theme_y <- theme_y %+replace% 
                theme(axis.ticks.x=element_blank(),
                      axis.text.x=element_blank())
        }
        ggplot(data, aes_string(x=y)) +
            stat_density(geom = "line")+
            annotation_custom(textGrob(y,gp = gpar(fontsize = 20)), 
                              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)+ 
            labs(x="",y="")+coord_flip()+
            theme_y 
    })
    
    # 5. placeholder empty plot
    empty.plot <- ggplot(data,aes_string(x=xs[1],y=ys[1]))+geom_blank()+
        labs(x="",y="")+theme_shared %+replace% 
        theme(panel.border=element_blank(),
              axis.ticks=element_blank(),axis.text=element_blank())
    
    list("regression.matrix"=regression.matrix,
         "x.density"=x.density,
         "y.density"=y.density,
         "empty.plot"=empty.plot)
    
}


dta.base <-  result.base[which(result.base$learning.size>30),]
scattermatrix<-scatter.regression.matrix(
    data =dta.base,
    xs = c("learning.size","weight.base"),
    ys = c("globalmodel","usermodel","alpha"),
    weight = "predicting.size")
x.density = scattermatrix$x.density
y.density = scattermatrix$y.density
regression.matrix=scattermatrix$regression.matrix
empty.plot = scattermatrix$empty.plot


# png("test6.png",res=300,width=3000,height=2100)
grid.arrange(x.density[[1]],x.density[[2]],empty.plot,
             regression.matrix[[1]][[1]],regression.matrix[[2]][[1]],y.density[[1]],
             regression.matrix[[1]][[2]],regression.matrix[[2]][[2]],y.density[[2]],
             regression.matrix[[1]][[3]],regression.matrix[[2]][[3]],y.density[[3]])
# dev.off()

dta = melt(result[,c(1,2,3,5)],id.vars=c(3,4))

ggplot(dta,aes(y=value,x=learning.size,group=variable))+
    geom_point(aes(shape=variable))+
    geom_smooth(method="lm",aes(linetype=variable))+
    facet_wrap(~learning.ratio)+
    scale_y_continuous(limit=c(0,1))+
    theme_bw()
    