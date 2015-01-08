compose.grids.line = function (corners, cols, rows ){
    
    # horizontal lines: colomn coords
    coords.col = seq(from = corners["left"], to =corners["right"],length= (cols+1) )
    # vertical lines: row corrds
    coords.row = seq(from = corners["bottom"], to =corners["top"],length= (rows+1) )
    
    df1 = data.frame("x" = rep(c(corners["left"], corners["right"]), rows+1 ),
                     "y" = rep(coords.row, each=2),
                     "type" = "h",
                     "id" = rep(c(1:(rows+1)), each=2)) 
    df1$group = paste0(df1$type, df1$id)
    
    df2 = data.frame("x" = rep(coords.col, each=2),
                     "y" = rep(c(corners["bottom"], corners["top"]), cols+1 ),
                     "type" = "v",
                     "id" = rep(c(1:(cols+1)), each=2)
    )
    df2$group = paste0(df2$type, df2$id)
    
    rbind(df1, df2)
    
}

compose.grids = function (corners, cols, rows ){
    
    # horizontal lines: colomn coords
    coords.col = seq(from = corners["left"], to =corners["right"],length= (cols+1) )
    # vertical lines: row corrds
    coords.row = seq(from = corners["bottom"], to =corners["top"],length= (rows+1) )
    
    left = rep(c(coords.col[1:cols]), rows)
    right = rep(c(coords.col[2:(cols+1)]), rows)
    bottom = rep(c(coords.row[1:rows]), each = cols)
    top = rep(c(coords.row[2:(rows+1)]), each = cols)
    
    df = data.frame("left"=left,"right"=right,"bottom"=bottom,"top"=top)
    
    df$row.id = rep(c(1:rows), each = cols)
    df$col.id = rep(c(1:cols), rows)
    
    df$uid = as.factor(paste(formatC(df$row.id,width=4,flag=0), 
                             formatC(df$col.id,width=4,flag=0) ,sep="_") )
    df$uid.seq = as.factor(1:nrow(df))
    
    df
    
}

# add extra colums to the dataframe about the grid
arrange.in.grids = function(df, cols, rows){
    # the bounding box
    corners = c("left" = min(checkin.active$lon.x)-0.001, 
                "right" = max(checkin.active$lon.x)+0.001,
                "top" =  max(checkin.active$lat.x)-0.001,  
                "bottom" = min(checkin.active$lat.x)+0.001 )
    
    # compute the cell size
    col.size = (corners["right"]-corners["left"])/cols
    row.size = (corners["top"]-corners["bottom"])/rows
    
    # get the reference grids ready
    grids = compose.grids(corners, cols, rows)
    
    # merge category.in.grids with checkin.active
    # and add grid id (reference the "grids") to the dataframe
    df$col.id = ceiling( (checkin.active$lon.x-corners["left"]) / col.size )
    df$row.id = ceiling( (checkin.active$lat.x-corners["bottom"]) / row.size )
    df$grid.id = factor(paste(formatC(df$row.id, width = 4, flag = 0), 
                              formatC(df$col.id, width = 4, flag = 0),
                              sep="_" ),
                        levels = levels(grids$uid) )
    df$ugrid.id = factor(df$grid.id)
    
    print(paste("successfully arrange the data in", rows, "*", cols, "grids."))
    
    df   
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
