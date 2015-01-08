# function: regress.job
regress.job =  function(df,dataready=FALSE,loc=NA){
    
    #dealing with the dataframe
    ptm <- proc.time()[3]
    if(!dataready) df = data.prepration(df)
    # report time duration
    t.prep = proc.time()[3] - ptm
    ptm <- proc.time()[3] # reset
    
    print(regression.formula)
    # regression 
    tmodel1<-multinom(as.formula(regression.formula),
                      data=df,maxit = 2000,MaxNWts = 2000)
    if(!is.na(loc)) save(tmodel1,file=loc)    
    # report time duration
    t.regress = proc.time()[3] - ptm
    ptm <- proc.time()[3] # reset

    
    # invesigate into the model
    prediction = predict(tmodel1)
    real = df$cate_l1
    comparison = data.frame("real"=real,"prediction"=prediction)
    comparison$correct = ifelse(comparison$prediction==comparison$real,1,0)
    rate=sum(comparison$correct)/nrow(comparison)
    
    c("t.prep"=t.prep, "t.regress"=t.regress,
      "edf"=tmodel1$edf,"nsunits"=tmodel1$nsunits,
      "rate"=rate,"memory"=gc()["Vcells",6])
}




data.prepration = function(df){
    
    # 0. put them in grids
    checkin.active.in.grids = arrange.in.grids(df, cols, rows)
    
    # 1. reduce ranks of "hour"
    if(exists("ncls.hour") && ncls.hour > 0){
        checkin.active.in.grids = reduce.ranks.by.dendrogram(
            checkin.active.in.grids, "cate_l1", "hour", ncls.hour)
    }
    
    # 2. reduce ranks of "grid"
    if(exists("ncls.grid") && ncls.grid > 0){
        checkin.active.in.grids = reduce.ranks.by.dendrogram(
            checkin.active.in.grids, "cate_l1", "ugrid.id", ncls.grid)
    }
    
    # 3. last_c: nothing
    # 4. weekday: nothing
    
    # 5. reduce ranks of joined "hour:grid" if necessary
    if(exists("ncls.ia.h.g") && ncls.ia.h.g > 0){
        checkin.active.in.grids$hour_grid = as.factor(
            paste(checkin.active.in.grids$hour, 
                  checkin.active.in.grids$ugrid.id.cid))  ## note: to save memory
        checkin.active.in.grids = reduce.ranks.by.dendrogram(
            checkin.active.in.grids, "cate_l1", "hour_grid", ncls.ia.h.g)
    }
    
    # 6. reduce ranks of joined "hour:last" if necessary
    if(exists("ncls.ia.h.l") && ncls.ia.h.l > 0){
        checkin.active.in.grids$hour_last = as.factor(
            paste(checkin.active.in.grids$hour, 
                  checkin.active.in.grids$last_cate_l1))
        checkin.active.in.grids = reduce.ranks.by.dendrogram(
            checkin.active.in.grids, "cate_l1", "hour_last", ncls.ia.h.l)
    }
    
    # 7. reduce ranks of joined "hour:isweekend" if necessary
    if(exists("ncls.ia.h.w") && ncls.ia.h.w > 0){
        checkin.active.in.grids$hour_weekday = as.factor(
            paste(checkin.active.in.grids$hour, checkin.active.in.grids$isweekend))
        checkin.active.in.grids = reduce.ranks.by.dendrogram(
            checkin.active.in.grids, "cate_l1", "hour_weekday", ncls.ia.h.w)
    }
    
    # 8. reduce ranks of joined "grid:last" if necessary
    if(exists("ncls.ia.g.l") && ncls.ia.g.l > 0){
        checkin.active.in.grids$grid_last = as.factor(
            paste(checkin.active.in.grids$ugrid.id, 
                  checkin.active.in.grids$last_cate_l1))
        checkin.active.in.grids = reduce.ranks.by.dendrogram(
            checkin.active.in.grids, "cate_l1", "grid_last", ncls.ia.g.l)
    }
    
   
    # 9. reduce ranks of joined "grid:weekday" if necessary
    if(exists("ncls.ia.g.w") && ncls.ia.g.w > 0){
        checkin.active.in.grids$grid_weekday = as.factor(
            paste(checkin.active.in.grids$ugrid.id, 
                  checkin.active.in.grids$isweekend))
        checkin.active.in.grids = reduce.ranks.by.dendrogram(
            checkin.active.in.grids, "cate_l1", "grid_weekday", ncls.ia.g.w )
    }
    
    # 10. reduce ranks of joined "last:weekday" if necessary
    if(exists("ncls.ia.l.w") && ncls.ia.l.w > 0){
        checkin.active.in.grids$last_weekday = as.factor(
            paste(checkin.active.in.grids$last_cate_l1, 
                  checkin.active.in.grids$isweekend))
        checkin.active.in.grids = reduce.ranks.by.dendrogram(
            checkin.active.in.grids, "cate_l1", "last_weekday", ncls.ia.l.w)
    }
    
    
    # 11. reduce ranks of joined "hour:grid:last" if necessary
    if(exists("ncls.ia.h.g.l") && ncls.ia.h.g.l > 0){
        checkin.active.in.grids$hour_grid_last = as.factor(
            paste(checkin.active.in.grids$hour, 
                  checkin.active.in.grids$ugrid.id,
                  checkin.active.in.grids$last_cate_l1))
        checkin.active.in.grids = reduce.ranks.by.dendrogram(
            checkin.active.in.grids, "cate_l1", "hour_grid_last", ncls.ia.h.g.l)
    }
    
    # 12. reduce ranks of joined "hour:grid:isweekend" if necessary
    if(exists("ncls.ia.h.g.w") && ncls.ia.h.g.w > 0){
        checkin.active.in.grids$hour_grid_weekday = as.factor(
            paste(checkin.active.in.grids$hour, 
                  checkin.active.in.grids$ugrid.id.cid,  ## note here! to save memory
                  checkin.active.in.grids$isweekend))
        checkin.active.in.grids = reduce.ranks.by.dendrogram(
            checkin.active.in.grids, "cate_l1", "hour_grid_weekday", ncls.ia.h.g.w)
    }
    
    # 13. reduce ranks of joined "hour:last:isweekend" if necessary
    if(exists("ncls.ia.h.l.w") && ncls.ia.h.l.w > 0){
        checkin.active.in.grids$hour_last_weekday = as.factor(
            paste(checkin.active.in.grids$hour, 
                  checkin.active.in.grids$last_cate_l1,
                  checkin.active.in.grids$isweekend))
        checkin.active.in.grids = reduce.ranks.by.dendrogram(
            checkin.active.in.grids, "cate_l1", "hour_last_weekday", ncls.ia.h.l.w)
    }
    
    
    # 14. reduce ranks of joined "grid:last:weekday" if necessary
    if(exists("ncls.ia.g.l.w") && ncls.ia.g.l.w > 0){
        checkin.active.in.grids$grid_last_weekday = as.factor(
            paste(checkin.active.in.grids$ugrid.id, 
                  checkin.active.in.grids$last_cate_l1,
                  checkin.active.in.grids$isweekend))
        checkin.active.in.grids = reduce.ranks.by.dendrogram(
            checkin.active.in.grids, "cate_l1", "grid_last_weekday", ncls.ia.g.l.w )
    }
    
    # 15. reduce ranks of joined "hour:grid:last:weekday" if necessary
    if(exists("ncls.ia.h.g.l.w") && ncls.ia.h.g.l.w > 0){
        checkin.active.in.grids$hour_grid_last_weekday = as.factor(
            paste(checkin.active.in.grids$hour, 
                  checkin.active.in.grids$ugrid.id,
                  checkin.active.in.grids$last_cate_l1, 
                  checkin.active.in.grids$isweekend))
        checkin.active.in.grids = reduce.ranks.by.dendrogram(
            checkin.active.in.grids, "cate_l1", "hour_grid_last_weekday", ncls.ia.h.g.l.w)
    }
   
    
    
    checkin.active.in.grids 
}




# reduce the ranks of a certain category by clustering the levels 
reduce.ranks.by.dendrogram = function(df, refence.col, target.col, nclusters){
    # tabular
    tabs = xtabs(data=df,as.formula(paste0("~",refence.col,"+",target.col)))
    tabs = apply(tabs,2,function(x){x/sum(x)})
    
    dg = as.dendrogram(hclust(dist(t(tabs))))
    dg
    h = find.h(dg, nclusters)
    clusters.real = cut(dg,h)$lower
    nclusters.real = length(clusters.real)
    
    # cluseters
    cls = data.frame()
    temp = sapply(1:nclusters.real, function(i){
        if(!is.leaf(clusters.real[[i]])) nodes = unlist(clusters.real[[i]])
        else nodes = as.numeric(clusters.real[[i]])
        cls <<- rbind(cls, data.frame(
            levels(df[,target.col])[nodes],
            rep(i,length(nodes)))  )
        NA
    })
    new.col = paste(target.col,"cid",sep=".")
    colnames(cls)=c(target.col, new.col)
    
    # merge into the original dataframe
    df = merge(x=df,y=cls)
    df[,new.col] = as.factor(df[,new.col])
    
    print(paste0("successfully lower the ranks of $",target.col, " into ",
                 nclusters.real, " clusters."))
    
    df
    
}


# find the best cutting height for the dendrogram tree
find.h = function(dg, nclusters){
    h = 0.5
    step = 0.1
    
    if(nclusters>=length(cut(dg,0)$lower)) h=0
    else if(nclusters<=length(cut(dg,1)$lower)) h=1
    else {  
        
        while(step>0.001){
            n.minus = length(cut(dg,h+step)$lower)
            n.real = length(cut(dg,h)$lower)
            n.plus = length(cut(dg,h-step)$lower)
            
            if(n.real == nclusters) break
            else if(n.minus >= nclusters){ # too many clusters than supposed
                h = h + step
            } else if(n.plus <= nclusters){ # too few clusters
                h = h - step
            } else if (n.minus < nclusters && n.plus > nclusters){# boundings found
                step = step / 5
                if(n.real > nclusters){ # too many clusters than supposed
                    h = h + step
                } else if(n.real < nclusters){ # too few clusters
                    h = h - step
                }
            }
        }
    }
    print(paste("The best h for the expected number of clusters is",h))
    
    h
}

