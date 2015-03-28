#############################################################################
# This document owns several plot functions.
# 1) point.plot: plot the checkin data records with/without a base geographic 
#     map;
# 2) freq.plot: frequency domain analysis
# 3) time.distribution.plot: simple plot of temporal distribution
# 4) time.radial.plot: radial plot of temporal distribution
# 5) transition.plot: plot the hotmap of transition pattern
#
# It further includes several supporting functions for basic statistics.


#############################################################################
library(rgdal) # Bindings for the Geospatial Data Abstraction Library
library(grid) 
library(plotrix) # radial plot
library(reshape2)
library(plyr) # for join
library(animation) # for gif creation



######################################################
## plot the point type data 
point.plot = function(point, x = "lon", y="lat", more.aes=NULL,
                    xlim = NA, ylim = NA, basemap=NA, ...){
    
    # regular the limits of axes
    if(is.na(xlim)) 
      xlim=range(point[,x], finite = TRUE)
    if(is.na(ylim)) 
      ylim=range(point[,y], finite = TRUE)
    
    # base map
    gg.map <- with.basemap(basemap)
    
    # flexible aes
    aes.list = append.aes(aes_string(x=x, y=y),more.aes)

    gg.map + geom_point(data=point,aes.list,...) +
        theme_bw(base_size = 8)  + 
        coord_map(xlim=xlim,ylim=ylim)
    
}

##########################################################
## plot geographical map from shapefile
map.plot = function(mapdir=NA,maplayer=NA,mapdf=NA,basemap=NA,
                    more.aes=NULL,...){
    
    # get map data either from a existing dataframe or a shapefile
    if(!is.na(mapdf)){
        shape.df = mapdf
    } else if(!is.na(mapdir) && !is.na(maplayer)){
        SPDF = readOGR(dsn = mapdir, layer = maplayer)
        shape.df = df.from.spdf(SPDF)
    } else {
        stop("map.plot() failed to find map data.")
    }
    
    # plot the geographic map
    gg.map <- with.basemap(basemap)
    
    # flexible aes
    aes.list = append.aes(aes(long,lat,group=group),more.aes)

    gg.map<- gg.map + 
        geom_polygon(data=shape.df,aes.list,...) + 
        theme_bw()  + 
        coord_map() 
    
}



###############################################################
point.animation.plot = function(point, x = "lon", y="lat", split.attr="hour",
                                basemap=NA, more.aes=NULL, title="",...){
  
  # must be constrained in the same area
  xlim = range(point[,x], finite = TRUE)
  ylim = range(point[,y], finite = TRUE)
  
  # slice the data
  point.slices = split(point,point[,split.attr])
  
  lapply(point.slices,function(i){
      a <- point.plot( i, x=x, y=y, basemap=basemap, more.aes=more.aes, 
                       xlim=xlim, ylim=ylim, ... ) +
          ggtitle(paste(title, i[1,split.attr],": 00")) +
          theme_bw(base_size=20) + 
          xlab("") + ylab("") 
      
      print(a)
  })
}





###############################################################
map.animation.plot= function(point,SPDF,basemap=NA, title="",
                             split.attr="hour",more.aes=NULL,...){
    
    xlim = range(point$lon, finite = TRUE)
    ylim = range(point$lat, finite=TRUE)
    
    lapply(split(point,point[,split.attr]),function(i){# data by hour
        
        # classify the polygons in the SPDF by the overlapped point data 
        SPDF = classify.polygon.by.point(point=i, SPDF, clsfy.attr="cate_l1")
        
        # plot
        mapdf=df.from.spdf(SPDF)
        # mapdf$density=apply(mapdf,1,function(i){i[i["cate.dom"]]})
        # mapdf$density=as.numeric(formatC(mapdf$density,digits=1,format = "f"))
        
        # grid.arrange(
        a <- map.plot(mapdf = mapdf, more.aes = more.aes, 
                      basemap = basemap, ...)+
            ggtitle(paste(title, i[1,split.attr],": 00")) +
            theme_bw(base_size=20) + 
            xlab("") + ylab("") 
        
        print(a)
        
    })
    
}


######################################################
## plot the temporal aspects in the checkin data (frequency domain)
freq.plot = function(checkin, title="",cols=2, rows=5){
    
    stats = lapply(split(checkin, checkin$cate_l1), function(checkin.cate){
        stats_by_date_hour(checkin.cate, category = checkin.cate[1,"cate_l1"])
    })
    
    df = do.call(rbind, stats)

    list.category=split(df,df$cate_l1)

    fre.list = lapply(seq_along(list.category), function(i){
        fre = spec.pgram(list.category[[i]]$prop, plot=FALSE)
        df.fre = data.frame(freq=fre[["freq"]],spec=fre[["spec"]])
        
        df.fre$cate_l1 = list.category[[i]][1,"cate_l1"]
        df.fre
    })

    fre.combined = do.call(rbind,fre.list)
    ggplot(fre.combined) +
        geom_line(aes(x=freq,y=spec)) + 
        facet_wrap(~cate_l1, ncol=cols, nrow=rows) +
        ggtitle(title)+
        scale_y_log10(name="Spectral Density")+
        scale_x_log10(name="Frequency [1/Hour]",
                      breaks = c(1/168,1/24,1/12,1/6),
                      labels = c(expression(1/168),expression(1/24),
                                 expression(1/12),expression(1/6))) + 
        theme(axis.title = element_text(size=10),axis.text=element_text(size=8),
              plot.title = element_text(size=10),#legend.position="none",
              legend.title = element_text(size=8),legend.text = element_text(size = 8))
    

}



######################################################
## plot the temporal aspects in the checkin data
time.distribution.plot =  function(checkin, title="", cols=2, rows=5){
    list.category = lapply(split(checkin,checkin$cate_l1), function(i){
        df = stats_checkin_by_hour(i, category = i[1,"cate_l1"])
    })
    
    df = do.call(rbind,list.category)

    ## plot
    ggplot(df, aes(x=hour,y=I(100*prop))) + 
        geom_bar(stat="identity") +
        facet_wrap(~cate_l1, ncol=cols, nrow=rows) +
        ggtitle(title)+
        coord_cartesian(ylim = c(0,13)) +
#         scale_y_continuous(name="Probability [%]",labels  = percent) +
        scale_y_continuous(name="Probability [%]") +
        scale_x_discrete(name="Hour",breaks=levels(df$hour), 
                         labels=c("0","","","3","","","6","","","9","","","12",
                                  "","","15","","","18","","","21","","23"))+ 
        theme(axis.title = element_text(size=10),axis.text=element_text(size=8),
              plot.title = element_text(size=10),#legend.position="none",
              legend.title = element_text(size=8),legend.text = element_text(size = 8))
}


######################################################
## plot the temporal aspects in the checkin data (radial plot)
# weekday:  plot workday/weekend seperately/together
# mark.peak: mark the peak of each radial line
time.radial.plot =  function(checkin, cols=5, rows=2, 
                             weekday=TRUE, mark.peak=TRUE){
    par(mfrow=c(rows,cols))
    
    plots = lapply(split(checkin,checkin$cate_l1), function(data){
        
        if(weekday){
            
            data$isweekend = as.factor(ifelse( (data$weekday>5 | data$weekday<1), 
                                           "Weekend", "Workday"))
            byweekend = lapply(split(data,data$isweekend),function(i){
                category.by.hour = as.data.frame(table(i$hour))
                # category.by.hour$p = category.by.hour$Freq / sum(category.by.hour$Freq)
                category.by.hour$hpi = as.numeric(category.by.hour$Var1)/12*pi
            
                category.by.hour
            })
            
            datastation = c(byweekend[[1]]$Freq/2,byweekend[[2]]$Freq/5)
            
            p.data = t(matrix(data=datastation, ncol=2))
            
        } 
        
        else{
            not.byweekend = as.data.frame(table(data$hour))
            not.byweekend$hpi = as.numeric(not.byweekend$Var1)/12*pi
            
            datastation = not.byweekend$Freq/7
            
            p.data = t(matrix(data=datastation, ncol=1))

        }
        
        
        radial.lim = pretty(datastation)
        radial.labels = c(rep("",length(radial.lim)-1),
                          tail(radial.lim,1))
        
        # plot the radial lines
        radial.plot(p.data,
                    labels=c("00","","","03","","","06","","","09","","",
                             "12","","","15","","","18","","","21","",""),
                    start=pi/2,clockwise=TRUE,
                    radial.lim = radial.lim, 
                    # radial.labels=radial.labels,
                    radial.labels="",
                    rp.type="p",main=head(data$cate_l1,1),line.col=c("blue","red"))
        
        # mark the peak or not
        if(weekday & mark.peak){
            
            datastation.lab1 = rep("", 24)
            datastation.lab1[which(byweekend[[1]]$Freq==max(byweekend[[1]]$Freq))]=
                round(max(byweekend[[1]]$Freq/2))
            datastation.lab2 = rep("", 24)
            datastation.lab2[which(byweekend[[2]]$Freq==max(byweekend[[2]]$Freq))]=
                round(max(byweekend[[2]]$Freq/5))
            
            # mark the peak
            radial.plot.labels(byweekend[[1]]$Freq/2,units="polar", 
                               start=pi/2,clockwise=TRUE,
                               labels=datastation.lab1,col="blue" )
            radial.plot.labels(byweekend[[2]]$Freq/5,units="polar", 
                               start=pi/2,clockwise=TRUE,
                               labels=datastation.lab2,col="red")
        }
        else if(!weekday & mark.peak){
            
            datastation.lab1 = rep("", 24)
            datastation.lab1[which(not.byweekend$Freq==max(not.byweekend$Freq))]=
                round(max(not.byweekend$Freq/7))
            print(datastation.lab1)
            # mark the peak
            radial.plot.labels(not.byweekend$Freq/7,units="polar", 
                               start=pi/2,clockwise=TRUE,
                               labels=datastation.lab1,col="blue" )
        }
        
    })
}


######################################################
## plot the transition probability in the checkin data 
transition.plot = function(checkin, from="last.cate_l1", to="cate_l1", 
                           scaled=FALSE, title=""){
    ## clean the data. 
    # When we consider transition, the first record of each user cannot be used,
    # because it has no previous record. We also want to exclude the situation 
    # when the user keeps checking in at the same location.
    
    checkin = checkin[checkin$last.gid!=-1,]  # remove each first record
    checkin = checkin[checkin$venue_id!=checkin$last.venue_id,] # remove continuous & same checkin
    checkin$tweight = 0.5^(checkin$time.interval) 
    
    tabs.last = xtabs(data=checkin, 
                      as.formula(paste("tweight~",to,"+",from)))
    
    if(scaled){
        tabs.last.p = apply(tabs.last,1,function(x){x/sum(x)})
        tabs.last.p = apply(tabs.last.p,1,function(x){x/sum(x)})
    } else{
        tabs.last.p = apply(tabs.last,2,function(x){x/sum(x)}) # no scale
    }
    
    
    red=rgb(1,0.2,0.2); white=rgb(1,1,1); green = rgb(0,1,0);
    RtoWrange<-colorRampPalette(c(red, white ) )
    WtoGrange<-colorRampPalette(c( white,green) ) 
    
    df = melt(tabs.last.p)
    colnames(df)=c("cate","last.cate","value")
    ggplot(df, aes(cate,last.cate)) + 
        geom_tile(aes(fill = log10(value)),
                  colour = "white") + 
        scale_fill_gradient2(low=RtoWrange(20),mid=WtoGrange(20),high=green,midpoint=-1,
                             breaks=c(-5,-4,-3,-2,-1),name="Probability",
                             labels=c(expression(10^-5),expression(10^-4),
                                      expression(10^-3),expression(10^-2),
                                      expression(10^-1)))+
        xlab("")+ylab("") + 
        ggtitle(title)+
        theme(axis.title = element_text(size=10),
              plot.title = element_text(size=10),#legend.position="none",
              legend.title = element_text( size=6),legend.text = element_text(size = 6),
              axis.text.y=element_text(size=5,color="black"),
              axis.text.x  = element_text(angle=30, vjust=1, hjust = 1,size=5,color="black"),
              #           axis.text=element_blank(),
              panel.background = element_blank(), plot.background = element_blank(),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              plot.margin=unit(c(0,0,0,0),"npc"))
}


#########################################################
## supporting functions

## FUNCTION 
## description:     generat dataframe describing checkin counts aggregated by hour
## input:           df: a dataframe
## optional input:  weekday: character describing weekday of the dataframe (if only)
## optional input:  category: character describing category of the dataframe (if only)
## output:          a new dataframe describing the statistics aggregated by hour
stats_checkin_by_hour <- function(df, weekday=NA, category=NA){
    
    vec_hour = table(df$hour)
    
    df_h = data.frame(
        count = as.vector(vec_hour),
        hour = names(vec_hour)
    )
    
    df_h$prop = df_h$count/sum(df_h$count)
    
    if(!is.na(weekday)){
        df_h$weekday = weekday
    }
    
    
    if(!is.na(category)){
        df_h$cate_l1 = category
    }
    
    df_h
}


## FUNCTION 
## description:     generate dataframe describing checkin counts by aggregated by date and hour
## input:           df: a dataframe
## optional input:  category: character describing category of the dataframe (if only)
## output:          a new dataframe describing the statistics aggregated by hour
stats_by_date_hour <- function(df, category=NA) {
    
    
    tab_date_hour = table(df$hour,df$yearday)
    vec_date = as.vector(table(df$yearday))
    
    ## make sure that the table is all complete
    hourlist = rownames(tab_date_hour)
    daylist = colnames(tab_date_hour)
    if(length(hourlist)<24){
        ## filling the row
        for (i in 1:24){
            hourname = ifelse( i<=10,  paste0("0", i-1),  as.character(i-1) )
            if( rownames(tab_date_hour)[i]!= hourname | is.na(rownames(tab_date_hour)[i]) ) {
                tab_date_hour = insertrow(tab_date_hour,i,name=hourname)
            }
        }
    } 
    if(length(daylist)< (as.numeric(max(daylist))-as.numeric(min(daylist))+1) ){
        ## filling the column
        for (i in 1:(as.numeric(max(daylist))-as.numeric(min(daylist))+1)){
            
            datename = i + as.numeric(min(daylist)) - 1
            if(datename<10) datename=paste0("00",datename)
            else if(datename>=10 & datename<100) datename=paste0("0",datename)
            else datename=as.character(datename)
            if( colnames(tab_date_hour)[i]!= datename ) {
                tab_date_hour = insertcol(tab_date_hour,i,name=datename)
                vec_date = c(vec_date[1:(i-1)], 0, vec_date[i:length(vec_date)])
                #                  names(vec_date)= c(names(vec_date)[1:(i-1)],datename,
                #                                     names(vec_date)[i:length(vec_date)])
            }
        }
    }
    
    df_date_hour = data.frame(
        count_hour = as.vector(tab_date_hour),   # how many checkins in this hour of this day
        count_daily = rep(vec_date, each = nrow(tab_date_hour)), # total checkins in this day
        hour = rep( rownames(tab_date_hour), ncol(tab_date_hour) ),
        #prop = count_hour/count_daily,
        datetime = strptime(paste(rep( colnames(tab_date_hour), each=nrow(tab_date_hour) ),
                                  rep( rownames(tab_date_hour), ncol(tab_date_hour)      ) ), 
                            format = "%j %H")
        
        
    )
    df_date_hour$prop = df_date_hour$count_hour/(df_date_hour$count_daily + 1E-6)
    df_date_hour$weekday = ifelse(
        (format(df_date_hour$datetime,"%w")>5 | format(df_date_hour$datetime,"%w")<1),
        "Weekend", "Workday")
    
    if(!is.na(category)){
        df_date_hour$cate_l1 = category
    }
    
    df_date_hour
}


insertrow = function(tab, position, vector=NA, name=NA){
    ## how many original rows before insertation
    n_rows = nrow(tab)
    
    ## specify the row content 
    n_cols = ncol(tab)
    if(is.na(vector)){
        vector = rep(0, n_cols)
    }
    
    ## insert the row in the specified position
    ## add row name if specified  
    if(position == 1 || position == "top"|| position == "first"|| position == "head"){
        newtab = rbind(vector, tab)
        if(!is.na(name)){
            rownames(newtab)=c(name,rownames(tab))
        }
    }
    else if(position == (n_rows+1) || position == "tail"|| position == "end"|| position == "last"){
        newtab = rbind(tab, vector)
        if(!is.na(name)){
            rownames(newtab)= c(rownames(tab),name)
        }
    }
    else{
        head1 = tab[1:(position-1),]
        tail1 = tab[position:n_rows,]
        if(n_cols>1) { # works for tables with more than one columns
            newtab = rbind(head1, vector, tail1)
        } else {
            newtab=as.table(matrix(c(head1, rep(0,1), tail1),
                                   ncol=1,byrow=TRUE))
            colnames(newtab)=colnames(tab)
        }
        if(!is.na(name)){
            rownames(newtab)= c(rownames(tab)[1:(position-1)],name,
                                rownames(tab)[position:n_rows])
        }
    }
    newtab
}

insertcol = function(tab, position, vector=NA, name=NA){
    ## how many original columns before insertation   
    n_cols = ncol(tab)
    
    
    ## specify the column content 
    n_rows = nrow(tab)
    if(is.na(vector)){
        vector = rep(0, n_rows)
    }
    
    ## insert the row in the specified position
    ## add row name if specified
    if(position == 1 || position == "first"|| position == "left"){
        newtab = cbind(vector, tab)
        if(!is.na(name)){
            colnames(newtab)=c(name,colnames(tab))
        }
    }
    else if( position == (n_cols+1) || position == "right"|| position == "last"){
        newtab = cbind(tab, vector)
        if(!is.na(name)){
            colnames(newtab)= c(colnames(tab),name)
        }
    }
    else{
        left = tab[,1:(position-1)]
        right = tab[,position:n_cols]
        newtab = cbind(left, vector, right)
        if(!is.na(name)){
            colnames(newtab)= c(colnames(tab)[1:(position-1)],name,
                                colnames(tab)[position:n_cols])
        }
    }
    
    newtab
}

##FUNCTION:
# convert SpatialPolygonDataFrame to a normal dataframe for plotting
##PARAMETERS:
# SDF: an object of SpatilPolygonDataFrame
df.from.spdf=function(SPDF){
    SPDF@data$id = rownames(SPDF@data)
    SPDF.points = fortify(SPDF, region="id")
    join(SPDF.points, SPDF@data, by="id")
}


##FUNCTION:
# common code for plotting something based on a base plot
with.basemap = function(basemap){
    if(is.na(basemap)){
        gg.map <- ggplot()
    }else{
        gg.map <- basemap
    }
    
    gg.map
}


##FUNCTION
# common function for appending aes for plotting
append.aes = function(aes,...){
    aes.list = c(aes,...)
    class(aes.list) <- "uneval"
    
    aes.list
}

##FUNCTION
# the default settings of plotting
plot.settings = function(settings){
    # all the default settings for plotting
    default = list(color="#55B1F7", alpha = 0.3,
                   point.size = 0.5, axis.size=8, 
                   title.size=10, legend.size=8,
                   title="")
    # if you want to reload some settings or add new settings
    for (name in names(settings)){
        # each attribute in the setting parameter
        default[[name]] = settings[[name]]
    }
    
    default
}
