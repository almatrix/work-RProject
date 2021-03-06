## FUNCTION
## description:     statistics for frequency / probability / conditional probability
## input:           data: a dataframe for statistics
##                  obs.col: the name of columns for computing frequency / probability
##                  cond.col: a vector specifying the names of columns of conditions 
##                          if conditional probaility is desired
##                  wgt.col: the name of columns for weighted frequency
##                  p.joint, p.cond, p.marg: type of frequency desired in the output
## output:          a dataframe (similar output as xtabs/table + probability)
xtabs2 = function(data,obs.col,cond.col=NA,wgt.col=NA,
                  p.joint=F,p.cond=F,p.marg=F, p.prior=F){
    
    if(length(cond.col)==1 && is.na(cond.col)){ # no conditional frequency/probability
        if(is.na(wgt.col)){
            freq = as.data.frame(xtabs(data=data,paste("~",obs.col),
                                       drop.unused.levels=T))
        }else{
            freq = as.data.frame(xtabs(data=data,paste(wgt.col,"~",obs.col),
                                       drop.unused.levels=T))
        }
    }else{ 
        if(length(cond.col)>1){# more than one columns are specified for condition
            # make a new column from all the specified condition columns
            data$condition = do.call(paste,lapply(cond.col,function(col){
                data[,col]
            }))
            # build a new column from condition column + observation column
            data$new.col = paste(data[,obs.col],data$condition,sep="@")
        }else{
            # build a new column from condition column + observation column
            data$new.col = paste(data[,obs.col],data[,cond.col],sep="@")
        }
        
        # make statistics based on the new column (save memory)
        if(is.na(wgt.col)){
            freq = as.data.frame(xtabs(data=data,~new.col,
                                       drop.unused.levels=T))
        }else{
            freq = as.data.frame(xtabs(data=data,paste(wgt.col,"~new.col"),
                                       drop.unused.levels=T))
        }
        # column information recovery after frequency statitics
        col.info = data.frame(do.call(rbind,strsplit(as.character(freq$new.col),
                                                     "@",fixed=TRUE)))
        freq = cbind(col.info,freq)
        colnames(freq)=c(obs.col,"condition","obs","Freq")
        
        # marginal frequency
#         marg.freq = ddply(freq,.(condition),function(in.condition){
#             sum(in.condition$Freq)
#         })
#         colnames(marg.freq)[2]="marg.freq"
        marg.freq = do.call(rbind,lapply(split(freq,freq$condition),
            function(in.condition){
                data.frame("condition"=in.condition[1,"condition"],
                           "marg.freq"=sum(in.condition$Freq))
            }))
        
        
        freq = merge(x=freq,y=marg.freq,all.x=T)
    }
    
    # prior frequency
#     prior.freq = ddply(freq, obs.col, function(y){
#         sum(y$Freq)
#     } )
#     colnames(prior.freq)[2]="prior.freq"
    prior.freq = do.call(rbind,lapply(split(freq,freq[,obs.col]),
        function(y){
            df = data.frame(y[1,obs.col],sum(y$Freq))
            colnames(df) = c(obs.col, "prior.freq")
            df
            }))
    
    freq = merge(x=freq,y=prior.freq,all.X=T)
    
    if(p.joint){
        freq$p.joint = with(freq, Freq / sum(Freq))
    }
    
    if(p.cond){
        if(length(cond.col)==1 && is.na(cond.col)){
            stop("you must specify the condition for conditional probability.")}
        freq$p.cond = with(freq, Freq / marg.freq)
    }
    
    if(p.marg){
        if(length(cond.col)==1 && is.na(cond.col)){
            stop("you must specify the condition for marginal probability.")}
        freq$p.marg = with(freq, marg.freq / sum(Freq))
    }
    
    if(p.prior){
        freq$p.prior = with(freq, prior.freq / sum(Freq))
    }
    
    freq
}


# some supporting functions
watch.time.memory <- function(t0=NA,mem0=NA){
    
    if(is.na(mem0)){
        mem.stat = gc(reset=T)
        mem = mem.stat["Ncells",6] + mem.stat["Vcells",6]
    }else{
        mem.stat = gc()
        mem = mem.stat["Ncells",6] + mem.stat["Vcells",6] - mem0
    }
    
    if(is.na(t0)){
        t = proc.time()[3]
    }else{
        t = proc.time()[3] - t0
    }
    
    c(t,mem)
    
}



model.prediction <- function(model, raw){
    #     prediction = levels(raw$cate_l1)[
    #         max.col( model$fitted.values)]
    prediction = predict(model)
    
    list(data.frame("true" = raw$cate_l1, "predicted" = prediction, 
                    "eval" = (raw$cate_l1==prediction)),
         data.frame("accuracy" = sum(raw$cate_l1==prediction)/nrow(raw) ))  
}

significance.code <- function(p){
    ifelse(p>0.1,"",
           ifelse(p>0.05, ".",
                  ifelse(p>0.01, "*",
                         ifelse(p>0.001, "**", "***"))))
}

model.fitness <- function(model){
    deviance = model$deviance
    edf = model$edf
    df = nrow(model$fitted.values) * (ncol(model$fitted.values)-1) - edf
    p = 1 - pchisq(deviance, df)
    significance = significance.code(p)
    AIC = model$AIC
    
    data.frame(AIC,deviance,edf,df,p,significance)
}


model.comparison <- function(model.null, model.fitted){
    deviance.diff = model.null$deviance - model.fitted$deviance
    
    df.null = nrow(model.null$fitted.values) * 
        (ncol(model.null$fitted.values)-1) - model.null$edf
    df.fitted = nrow(model.fitted$fitted.values) * 
        (ncol(model.fitted$fitted.values)-1) - model.fitted$edf
    df.diff = df.null - df.fitted
        
    p = 1 - pchisq(deviance.diff, df.diff)
    significance = significance.code(p)
    
    LLf = model.fitted$deviance / (-2)
    LL0 = model.null$deviance / (-2)
    N = nrow(model.null$fitted.values)
    
    McFadden.R2 = 1 - LLf / LL0
    CoxSnell.R2 = 1 - exp((2/N) * (LL0 - LLf))
    Nagelkerke.R2 = (1 - exp((2/N) * (LL0 - LLf))) / (1 - exp(LL0)^(2/N))
    
    data.frame(deviance.diff, df.diff, p, significance, 
               "McFadden.R2"=McFadden.R2, 
               "CoxSnell.R2"=CoxSnell.R2, 
               "Nagelkerke.R2"=Nagelkerke.R2)
}

counter.print = function(interval){
    if(counter %% interval ==0) print(paste(Sys.time(),counter))
    counter <<- counter+1
}

counter.reset = function(n=1){counter <<- n}