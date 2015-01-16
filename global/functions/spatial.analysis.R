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



####################################################################
# Spatial Intersection

##FUNCTION:
# copy the information of spatial polygon to the intersecting points
##PARAMETERS:
# checkin: a point type dataframe with position information
# crs.str: specifiy the CRS of the point data. Default is WGS84. 
# SPDF: a SpatialPolygonDataFrame
# poly.attr: the attributes in the polygon that need to be assigned to the point
##OUTPUT:
# the point data with additonal columns from the polygon
point.in.poly = function(point, SPDF, copy.attr,
                         point.position=c("lon","lat"),
                         crs.str=crs.wgs84.str){
    
    # make SpatialPointDataFrame from the point dataset
    spt = SpatialPointsDataFrame(coords=point[,point.position],
                                  data=point[,c("gid","cate_l1")], 
#                                   data=rownames(point),
                                  proj4string=CRS(crs.str))
    print("Spatial data has been prepared.")
    
    # intersect the two layers, and copy the information specified by 
    # "copy.attr" from the intersecting polygon to the point
    point[,copy.attr] = over(spt, SPDF)[,copy.attr]
    print("The information from the polygon has been assigned to the points.")
    
    point
}


##FUNCTION:
# find out the probability distribution of categories for each polygon
# checkin: the point data frame
# poly.attr: the name of the column that indicates polygon
# cate.attr: the name of the column that indicates category
##OUTPUT: 
# a dataframe. Each row represents the probability distribution of all
# category (column) in each polygon.
cate.distr.in.poly = function(checkin, poly.attr="POSTAL",cate.attr="cate_l1"){
    
    # computing for each polygon
    cate.poly = lapply(split(checkin,checkin[,poly.attr]),function(i){
        # the corresponding points (categories) in that polygon
        stats = as.data.frame(table(i[,cate.attr]))
        
        stats$probability = stats$Freq / sum(stats$Freq)
        
        df = data.frame(t(stats$probability))
        colnames(df) = stats$Var1

        categoy.in.order = 
            stats$Var1[order(stats$probability,decreasing = TRUE)]
        df[1,"Category.1st"] = categoy.in.order[1] 
        df[1,"Category.2nd"] = categoy.in.order[2]
        df[1,"Category.3rd"] = categoy.in.order[3] 
        df[1,poly.attr] = i[1,poly.attr]
        
        df
          
    })
    
    print("The distribution of category has been assigned to each polygon.")
    
    do.call(rbind,cate.poly)
}



##FUNCTION:
# attach polygons in a shapefile with the distribution of checkin categories
##PARAMETERS:
# SPDF: a SpatialPolygonDataFrame
# cate.distr: dataframe specifying how each polygon and categories are connected
# poly.attr: name of the column for polygon 
##OUTPUT
# a new SPDF with the category information in its data
poly.with.category = function(SPDF, cate.distr, poly.attr){
    SPDF@data = merge(SPDF@data,cate.distr,
                      by=poly.attr,all.x=TRUE) 
    
    SPDF
}


##FUNCTION:
# classify the polygons in a SPDF by the clsfy.attr in the point data
classify.polygon.by.point = function(point, SPDF, 
                                     poly.attr="POSTAL", clsfy.attr="cate_l1"){
    # assign the attribute from SPDF to the point
    point = point.in.poly(point, SPDF, copy.attr=poly.attr)
    # get the distribution of categories in each postal code region
    cate.distr = cate.distr.in.poly(point,poly.attr=poly.attr,
                                    cate.attr=clsfy.attr)
    # and use that information to implement the SPDF
    SPDF = poly.with.category(SPDF, cate.distr=cate.distr, 
                              poly.attr="POSTAL")
    
    SPDF
}
