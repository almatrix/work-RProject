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
