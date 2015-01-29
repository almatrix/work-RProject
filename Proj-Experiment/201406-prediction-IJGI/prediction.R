setwd("Proj-Experiment/201406-prediction-IJGI/")
library(rgeos)
library(scales)
library(reshape2)
library(ggplot2)
library(gridExtra)
# library(parallel)
#library(TSA)
library(ca)

source("../../global/functions/prepare.checkin.R")
source("../../global/functions/basic.stats.plots.R")
source("../../global/functions/spatial.analysis.R")
source("../../global/functions/etc.R")

# global variable
ppi=300

crs.wgs84.str = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
checkin.NY = prepare.checkin("../../global/data/csv-raw/NewYorkCity.csv",
                             is.raw=TRUE, 
                             convert.time=TRUE, add.history=FALSE)

SPDF = readOGR(dsn = "../../global/data/shapefiles", layer = "NYC_zipcode")

get.temporal.impact <- function(dataframe){
    
    hours = formatC(c(0:23),width=2,flag="0")
    
    do.call(rbind, lapply(hours, function(hour){
        data.in.hour = dataframe[dataframe$hour==hour,]
        freq.tab = as.data.frame(table(data.in.hour$cate_l2))
        freq.df = data.frame(t(freq.tab$Freq / sum(freq.tab$Freq) ))
        colnames(freq.df) = freq.tab$Var1
        
        freq.df
    }) )
    
}

get.spatial.impact <- function(point.in.poly, 
                               poly.attr="POSTAL",cate.attr="cate_l2"){
    mat = cate.distr.in.poly(point.in.poly, poly.attr=poly.attr,cate.attr=cate.attr)
    mat[,1:(ncol(mat)-4)]
}

get.denominator <- function(dataframe){
    
    freq.tab = as.data.frame(table(dataframe$cate_l2))
    
    df = data.frame(t(freq.tab$Freq / sum(freq.tab$Freq)))
    colnames(df) = freq.tab$Var1
    
    df
}

checkin.in.poly = na.omit(point.in.poly(checkin.NY, SPDF, copy.attr="POSTAL"))
# order the data
checkin.in.poly = checkin.in.poly[order(checkin.in.poly$timestamps),]

learning.length = length(which(checkin.in.poly$yearday<152))

learning.all.data = checkin.in.poly[1:learning.length,]
experiment.data = checkin.in.poly[(learning.length+1):nrow(checkin.in.poly),]
experiment.data$flag = paste(experiment.data$hour,experiment.data$POSTAL)

pr.t.mat.global = get.temporal.impact(learning.all.data)
pr.s.mat.global = get.spatial.impact(learning.all.data)
pr.d.global = unlist(get.denominator(learning.all.data))+ 1E-6


## parallel computin
counter = 0; counter2=0;
prediction= do.call(rbind,  
                    lapply(
                        split(experiment.data,experiment.data$user_id)[1:50],
                        function(user){
    counter <<- counter + nrow(user); counter2 <<- counter2 +1;
    print(paste(Sys.time(),"User",counter2,"; Finished", counter,"records."))
    # leaning from the historical data of the user
    user.id = user[1,"user_id"]
    # is there learning data for this user?
    learning = learning.all.data[which(learning.all.data$user_id==user.id),]
    learning.length = nrow(learning)
    if(learning.length==0){
        pr.t.mat.indiv = pr.t.mat.global
        pr.s.mat.indiv = pr.s.mat.global
        pr.d.indiv = pr.d.global
    }else{
        pr.t.mat.indiv = get.temporal.impact(learning) 
        pr.s.mat.indiv = get.spatial.impact(learning) 
        pr.d.indiv = unlist(get.denominator(learning)) * 0.99 + 0.01 * pr.d.global
    }
    
    
    do.call(rbind, lapply(split(user, user$flag),function(i){
        # common information 
        hour = i[1,"hour"]
        polygon = i[1,"POSTAL"]
        # real checkin category
        real.list = i$cate_l2
        
        # get the vector
        pr.t.global = unlist(pr.t.mat.global[hour,])
        if(is.na(pr.t.mat.indiv[hour,1])){
            pr.t.indiv = pr.t.global
            flag.t = FALSE
        }else{
            pr.t.indiv = unlist(pr.t.mat.indiv[hour,]) * 0.99 + 0.01 * pr.t.global
            flag.t = TRUE
        }
        # get the vector
        pr.s.global = unlist(pr.s.mat.global[polygon,])
        if(is.na(pr.s.mat.indiv[polygon,1])){
            pr.s.indiv = pr.s.global
            flag.s = FALSE
        }else{
            pr.s.indiv = unlist(pr.s.mat.indiv[polygon,]) * 0.99 + 0.01 * pr.s.global
            flag.s = TRUE
        }
        
        # the average evaluation
        pr.ave.global = pr.t.global * pr.s.global / pr.d.global 
        pr.ave.indiv = pr.t.indiv * pr.s.indiv / pr.d.indiv
        
        # order the probability vector (get first 30)
        pred.d.global = sort(pr.d.global, decreasing = TRUE)
        pred.t.global = sort(pr.t.global, decreasing = TRUE)
        pred.s.global = sort(pr.s.global, decreasing = TRUE)
        pred.st.global = sort(pr.ave.global, decreasing = TRUE)
        
        pred.d.indiv = sort(pr.d.indiv, decreasing = TRUE)
        pred.t.indiv = sort(pr.t.indiv, decreasing = TRUE)
        pred.s.indiv = sort(pr.s.indiv, decreasing = TRUE)
        pred.st.indiv = sort(pr.ave.indiv, decreasing = TRUE)
        
        
        df = do.call(rbind, lapply(real.list,function(real){
            # find the correct position
            p1 =  which(names(pred.d.global)==as.character(real))
            p2 =  which(names(pred.t.global)==as.character(real))
            p3 =  which(names(pred.s.global)==as.character(real))
            p4 =  which(names(pred.st.global)==as.character(real))
            
            p5 =  which(names(pred.d.indiv)==as.character(real))
            p6 =  which(names(pred.t.indiv)==as.character(real))
            p7 =  which(names(pred.s.indiv)==as.character(real))
            p8 =  which(names(pred.st.indiv)==as.character(real))
            
#             df = data.frame("None"=p1,"Temporal"=p2,"Spatial"=p3,
#                             "All"=p5,
#                             "Hour"=hour,"Postal"=polygon,
#                             "Category"=real,
#                             "flag.learning"=flag.learning,
#                             "learning.length"=learning.length,
#                             "flag.t"=flag.t,"flag.s"=flag.s)
#             
            df = data.frame("None.Global"=p1,
                            "None.Individual"=p5,
                            "Temporal.Global"=p2,
                            "Temporal.Individual"=p6,
                            "Spatial.Global"=p3,
                            "Spatial.Individual"=p7,
                            "ST.Global"=p4,
                            "ST.Individual"=p8,
                            "Hour"=hour,
                            "Postal"=polygon,
                            "Real.Category"=real,
                            "Learning.Length"=learning.length,
                            "Learning.T"=flag.t,
                            "Learning.S"=flag.s)
            
            df  
        }))
        
        # additional information 
        df$user = user.id
        df$gid = i$gid
        
        df
    }) )
    
}))