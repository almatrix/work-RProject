######################################################
# description: 
# This document deals with preparing the dataframe from 
# a csv checkin data file or a half-cooked dataframe.
# It includes a main function named "prepare.checkin",
# as well as three supported functions "get.previous.record", 
# "join.weather", and "set.temporal.range".
######################################################


######################################################
# prepare the dataframe from the raw csv data or half done dataframe
prepare.checkin = function(checkin.data, is.raw=FALSE, weather.data=NA, 
                           convert.time=FALSE, add.history=FALSE){
    
    # read the text file if it is a raw csv data; 
    if(is.raw){
        
        print(paste(Sys.time(),
                  ": Preparing data frame from the raw data",
                  checkin.data))
        
        # read the data
        checkin = read.csv( checkin.data, 
                            header=TRUE, sep=",",
                            na.strings = "none",
                            colClasses = c("numeric","numeric","factor",
                                           "factor", "numeric","numeric",
                                           "numeric","character","factor",
                                           "factor")
        )
        
        print(paste(Sys.time(),": The raw checkin data loaded."))
    } 
    # otherwise just load the given data
    else {
        print(paste(Sys.time(),
                ": Preparing data frame from the already existed one."))
        checkin =  checkin.data
        print(paste(Sys.time(),": The base data loaded."))
    }
    
    ## deal with time 
    if(convert.time){
        checkin$datetime = strptime( strtrim(checkin$localtime,19),
                                     format="%Y-%m-%d %H:%M:%S")
        checkin$hour = as.factor(format(checkin$datetime,"%H"))
        checkin$yearday = format(checkin$datetime,"%j")
        checkin$weekday = format(checkin$datetime,"%w")
        checkin$isweekend = as.factor(ifelse(( checkin$weekday>5 | checkin$weekday<1),
                                             "Weekend", "Workday"))
        
        print(paste(Sys.time(),": Time related work done."))
    }
    
    
    ## add record for previous checkin
    if(add.history){
        checkin = get.previous.record(checkin)
        print(paste(Sys.time(),": Previous recording work done."))
    }
    
    ## merge with weather data 
    if(!is.na(weather.data)){
        ## load weather
        weather = read.csv( weatherdata, 
                            header=TRUE, sep=",", na.strings = c("-9999","Unknown"),
                            colClasses = c("numeric","numeric","numeric","character",
                                           "numeric","factor","numeric","numeric",
                                           "numeric","numeric","numeric","numeric",
                                           "numeric","numeric")
        )
        
        weather$fog=as.logical(weather$fog)
        weather$rain=as.logical(weather$rain)
        weather$snow=as.logical(weather$snow)
        weather$thunder=as.logical(weather$thunder)
        weather$tornado=as.logical(weather$tornado)
        
        ## join checkin data with weather data based on timestamps 
        checkin = join.weather(checkin, weather)
        
        checkin = checkin[complete.cases(checkin$conds),]
        
        print(paste(Sys.time(),": Mergeing with weather done."))
    }
    
    checkin
    
}

######################################################
# add the user's previous checkin record to the data
get.previous.record = function(checkin){
    
    # order the dataframe by user and timestamps
    checkin = checkin[order(checkin$user_id,checkin$timestamps),]
    # copy columns
    copied = rbind(c(-1,-1,NA,NA,NA,NA),
                   checkin[(1:(nrow(checkin)-1)),
                           c("gid","timestamps","venue_id",
                             "cate_l1","cate_l2","user_id")])
    colnames(copied)=c("last.gid","last.timestamps","last.venue_id",
                       "last.cate_l1","last.cate_l2","last.user_id")
    checkin = cbind(checkin, copied)
    
    # reset the record that has "last.user_id"!="user_id"
    # which means the two records are created by different users
    wrongmatch = which(checkin$user_id!=checkin$last.user_id)
    checkin[wrongmatch,c("last.gid","last.timestamps")]=-1
    checkin[wrongmatch,c("last.venue_id","last.cate_l1",
                         "last.cate_l2","last.user_id")]=NA
    
    checkin$time.interval = (checkin$timestamps - checkin$last.timestamps)/3600
    
    checkin
}



######################################################
# join the weather information with the checkin data based on time
join.weather = function(checkin, weather){
    # the influence time of each weather record
    weather = set.temporal.range(weather)
    
    checkin.time = checkin$timestamps
    
    checkin$weather.id = sapply(checkin.time, function(time){
        weather[which( weather$influ_ts <= time & weather$influ_te > time ),
                "id"]
    })
    
    df = merge(x=checkin, y=weather, 
               by.x="weather.id", by.y="id", all.X=TRUE)
    
    # remove the unnecessary columns
    df$weather_id = NULL
    df$id = NULL
    df$localtime.x = NULL
    df$localtime.y = NULL
    df$lat.y = NULL
    df$lon.y = NULL
    df$timestamps.y = NULL
    df$influ_ts = NULL
    df$influ_te = NULL
    
    df
}


######################################################
# set the temporal range of influence of each weather record
set.temporal.range = function(weather){
    obs_time = weather$timestamps
    len = length(obs_time)
    nxt_obs_time = c(obs_time[2:len], (obs_time[len]+3600))
    lst_obs_time = c((obs_time[1]-3600), obs_time[1:len-1])
    weather$influ_ts = (obs_time + lst_obs_time )/2
    weather$influ_te = (obs_time + nxt_obs_time )/2
    weather
}