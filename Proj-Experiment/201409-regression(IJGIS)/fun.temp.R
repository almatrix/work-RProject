# get the experimental dataset ready
usersets.active = usersets[usersets.active.idx]


reduce.levels.ca = function(data,var.ref,var2red,url=NA){
    require(ca)

    # correspondence analysis
    tab = xtabs(as.formula(paste(" ~ ",var.ref, "+", var2red)), 
                data = data)
    fit = ca(tab)
    
    # get coordinates
    coords = fit[["colcoord"]][,1:2]
    # determine number of clusters
    ntotal = nrow(coords)
    wss <- (ntotal-1)*sum(apply(coords,2,var))
    for (i in 2:(ntotal-1)) 
        wss[i] <- sum(kmeans(coords,centers=i)$withinss)
    
    ncls = length(wss[wss>0.01*wss[1]])+1
    
    # K-Means Cluster Analysis
    clusters <- kmeans(coords,ncls) 
    # get cluster means
    aggregate(coords,by=list(clusters$cluster),FUN=mean)
    # append cluster assignment
    coords <- data.frame("Var"=fit[["colnames"]], as.factor(clusters$cluster)) 
    
    if(!is.na(url)){
        png(paste0(url,"_1.png"), width = 4*ppi, height = 3*ppi, res=ppi)
        plot(fit, mass = TRUE, contrib = "absolute", 
               	 map = "rowgreen", arrows = c(FALSE, FALSE)) # asymmetric map
        dev.off()
        
        png(paste0(url,"_2.png"), width = 4*ppi, height = 3*ppi, res=ppi)
        plot(1:(ntotal-1), wss, type="b", xlab="Number of Clusters",
                    			  ylab="Within groups sum of squares") 
        dev.off()
        
        png(paste0(url,"_3.png"), width = 4*ppi, height = 3*ppi, res=ppi)
        ggplot(data=coords, aes(x=X1,y=X2,color=as.factor(clusters.cluster)))+
                geom_point(size=4, shape=21)+
                geom_text(aes(label=Var),hjust=-0.5, vjust=0.5)
        dev.off()
    }
    
    colnames(coords) = c("conds","met.cls")
#     colnames(coords)[4]=paste0(var2red,".cls")
#     user.groups = merge(x=data, y=coords[,c(1,4)], 
#                         by.x=var2red, by.y="Var", all.X=TRUE)
#     
#     user.groups
    coords
}




# log linear analysis for each valid user
loglinear = lapply(usersets.active[1],function(user){
    user$cate_l2 <- factor(user$cate_l2)
    user$conds <- factor(user$conds)
    
    cls.s = spatial.clustering(user)
    cls.t = temporal.clustering(user,interval=3)   
    cls.m = reduce.levels.ca(user,"cate_l2","conds")
    cls = merge(x=cls.s, y=cls.t[,c(1,3)], by.x="id", by.y="id", all.X=TRUE)
    cls = merge(x=cls,y=user[,c("gid","conds")],by.x="id",by.y="gid",all.x=TRUE)
    cls = merge(x=cls, y=cls.m, by.x="conds", by.y="conds", all.X=TRUE)


    # log linear analysis 1: S-T
    cate.st = xtabs(~hour.cls+sp.cls+cate_l2, data=cls)
    freq.cate.st = as.data.frame(cate.st)
    colnames(freq.cate.st)=c("hour","sp","cate_l2","Freq")
    loglin.sat.st = glm(Freq~cate_l2*hour*sp, 
                        data=freq.cate.st, family=poisson)
    #summary(loglin.sat.cate.hour.conds)
#     anova(loglin.sat.cate.hour.conds)

    # log linear analysis 2: S-T-M
    cate.stm = xtabs(~hour.cls+sp.cls+met.cls+cate_l2, data=cls)
    freq.cate.stm = as.data.frame(cate.stm)
    colnames(freq.cate.stm)=c("hour","sp","met","cate_l2","Freq")
    loglin.sat.stm = glm(Freq~cate_l2*hour*sp*met, 
                         data=freq.cate.stm, family=poisson)

    c("st"=loglin.sat.st,"stm"=loglin.sat.stm)
})