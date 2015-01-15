#######################################################
# FUNCTION: find spatial clusters from the provided data
# return: a list including:
#   "point"(has been removed): indicating the cluster of each point in the data
#   "centers": indicating the information of cluster centers
#   "point.unique": reserve unique points and their belonging clusters
spatial.clustering = function(data,wss.control=0.1){
    # subsetting 
    subset = data[,c("gid","lat","lon","cate_l1","cate_l2")]
    
    # get coordinates of unique locations
    coords = unique(subset[,c("lat","lon")])
    
    # K-means clustering
    ntotal = nrow(coords)
    # try to find cluster only when the data has at least more than 2 points
    if(ntotal > 2){ 
        # determine number of clusters based on the unique locations
        wss <- (ntotal-1) * sum(apply(coords,2,var))
        for (ncls in 2:(ntotal-1)) {
            wss.i <- sum(kmeans(coords,centers=ncls)$withinss)
            if (wss.i <= wss.control * wss) break
        }
#         print(wss)
#         ncls = length(wss[wss>0.5*wss[1]])+1
        if(ncls > (ntotal-1)) ncls = ntotal-1
        
#         print(paste("Found",ncls,"clusters in the data."))
        
        # K-Means Cluster Analysis
        kmcls <- kmeans(coords,ncls) # "kmcls" is a list of k-means clustering result
#         print(paste("Clustered the points into",ncls,"clusters"))
        wss.final <- sum(kmcls$withinss)
        percentage <- wss.final / wss[1]

        # record the clusters by latitude of cluster center
        centers = data.frame("lat.center"=kmcls$centers[,1],
                             "lon.center"=kmcls$centers[,2],
                             "cid"=c(1:ncls),
                             "size"=kmcls$size)
        # reorder the clusters by lat
        centers = centers[order(centers$lat.center),] # reorder the data frame
        centers$cid.ordered = as.factor(c(1:ncls))  # this is the new cluster id
        
        # record the clusters of each unique location
        coords.cls = data.frame(coords,"cid"=kmcls$cluster) 
        coords.cls = merge(x=coords.cls,
                           y=centers[,c("cid","cid.ordered")], all.X=TRUE)  
#         print("merge finished for unique locations.")
    }
    else{
        centers = data.frame("lat.center"=mean(coords$lat),
                   "lon.center"=mean(coords$lon),
                   "cid"=1,
                   "size"=1)
        coords.cls <- data.frame(as.factor(c(1)),coords, coords, as.factor(c(1))) 
        
        wss.final = 0
        percentage = 0
    }
    colnames(coords.cls) = c("cid","lat.x","lon.x","cid.ordered")
    
#     print("final merging...")
    # join the clustered locations with the original data frame
    list(#"point" = merge(x=subset, y=coords.cls, all.x=TRUE),
         "centers" = centers,
         "point.unique"= coords.cls,
         "wss"=wss.final,
         "pct"=percentage)
}

# assign the checkin record within their belonging spatial polygon
checkin.in.poly = function(checkin, mapdir, maplayer, poly.attr){
    # make SpatialPointDataFrame from the checkin dataset
    checkin.spt = SpatialPointsDataFrame(
        coords=checkin.NY[,c("lon","lat")],
        data=checkin.NY[,c("gid","cate_l1")], 
        proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    
    # make SpatialPolygonDataFrame from the shapefile
    postal.spl = readOGR(dsn=mapdir,layer=maplayer)
    print("Spatial data has been prepared.")
    
    # intersect the two layers to find out which polygon (characterized 
    # e.g. by POSTAL) each checkin point belongs to, and add that information 
    # back to the checkin data
    checkin[,poly.attr] = over(checkin.spt, postal.spl)[,poly.attr]
    print("The information from the polygon has been assigned to the points.")
    
    list("checkin"=checkin,"spl"=postal.spl)
}

# find out the most N dominant categories or probability for each polygon
cate.distr.in.poly = function(checkin,mapdir, maplayer, cate.length=1,
                              poly.attr="POSTAL",cate.attr="cate_l1",
                              probability=FALSE){
    
    # get the belonging points of each category
    in.poly = checkin.in.poly(checkin,mapdir, maplayer, poly.attr)
    checkin = in.poly[["checkin"]]
    
    cate.poly = lapply(split(checkin,checkin[,poly.attr]),function(i){
        # the corresponding points (categories) in that polygon
        categories = as.data.frame(table(i[,cate.attr]))
        
        if(!probability){
            categories = categories[order(categories$Freq,
                                          decreasing = TRUE),]
            df = data.frame(i[1,poly.attr],categories$Var1[1:cate.length])
            colnames(df)=c(poly.attr, paste0("Category_",c(1:cate.length)))
            
        }else{
            # the probability distribution
            df = data.frame(c(i[1,poly.attr],
                            categories$Freq/sum(categories$Freq)))
            colnames(df)=c(poly.attr, categories$Var1)
        }
        
        
        df
          
    })
    
    df = do.call(rbind,cate.poly)
    print("The distribution of category has been assigned to each polygon.")
    
    list("cate.in.poly"=df,"spl"=in.poly[["spl"]])
}

category.by.polygon = function(checkin, poly.attr="POSTAL",...){
    
    # find out the most dominant category for each polygon (POSTAL)
    data = cate.distr.in.poly(checkin, poly.attr=poly.attr,...)
    
    cate.poly = data[["cate.in.poly"]]
    postal.spl = data[["spl"]]
    
    # the information (dominant category) will be mapped to the polygon 
    # data for plotting
    postal.spl@data$id = rownames(postal.spl@data)
    postal.spl@data = merge(postal.spl@data,cate.poly,
                            by=poly.attr,all.x=TRUE) 
    postal.spl.points = fortify(postal.spl, region="id")
    join(postal.spl.points, postal.spl@data, by="id")
}