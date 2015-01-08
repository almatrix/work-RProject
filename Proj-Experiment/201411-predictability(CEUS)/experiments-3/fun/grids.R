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
    
    df$uid = as.factor(paste(formatC(df$row.id,width=3,flag=0), 
                             formatC(df$col.id,width=3,flag=0) ,sep="_") )
    df$uid.seq = as.factor(1:nrow(df))
    
    df
    
}

# add extra colums to the dataframe about the grid
arrange.in.grids = function(df, cols, rows){
    # the bounding box
    corners = c("left" = min(df$lon.x)-0.001, 
                "right" = max(df$lon.x)+0.001,
                "top" =  max(df$lat.x)-0.001,  
                "bottom" = min(df$lat.x)+0.001 )
    
    # compute the cell size
    col.size = (corners["right"]-corners["left"])/cols
    row.size = (corners["top"]-corners["bottom"])/rows
    
    # get the reference grids ready
    grids = compose.grids(corners, cols, rows)
    
    # merge category.in.grids with checkin.active
    # and add grid id (reference the "grids") to the dataframe
    df$col.id = ceiling( (df$lon.x-corners["left"]) / col.size )
    df$row.id = ceiling( (df$lat.x-corners["bottom"]) / row.size )
    df$grid.id = factor(paste(formatC(df$row.id, width = 3, flag = 0), 
                              formatC(df$col.id, width = 3, flag = 0),
                              sep="_" ),
                        levels = levels(grids$uid) )
    df$ugrid.id = factor(df$grid.id)
    
    print(paste("successfully arrange the data in", rows, "*", cols, "grids."))
    
    df   
}

