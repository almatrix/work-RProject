
#########################################
# supporting functions impact model 
#########################################
# impact.model(): xi --> si
# input: data: raw data frame (observations)
#        x: the condition/predictor
#        y: the dependent variable
#        k, f: the parameters for shrink factor()
# output: the impact of x on y
impact.model <- function(data, x, y, w=NA, Z=30, global.im = NA, ...){

    # for data from more than one single user (global dataset)
    # or data from one single user whose records is large enough
    if(length(unique(data$user_id))>1 |
           (length(unique(data$user_id))==1 && nrow(data)>Z)){

        scaler <- xtabs2(data = data, obs.col = y, wgt.col = w,
                         cond.col = x, p.cond=T, p.prior=T)
        scaler$condition=factor(scaler$condition,levels=levels(data[,x]))
        scaler[,y]=factor(scaler[,y],levels=levels(data[,y]))
        scaler$lambda <- with(scaler, shrink.factor(marg.freq, ...))
        scaler$impact <- with(scaler, weighted.sum(lambda, p.cond, p.prior))
        
        im <- scaler[,c(y,"condition","impact")]
        # long format to wide format
        im <- as.data.frame.table(
            xtabs(data=im,as.formula(paste("impact~condition+",y))))
        im <- dcast(im,as.formula(paste("condition~",y)),value.var="Freq")
        colnames(im)[2:ncol(im)] <- 
            paste(abbreviate(colnames(im))[2:ncol(im)],x,sep="_")

        
        # dealing with empty data 
        if(length(unique(data$user_id))==1 && nrow(data)>Z){
            if(sum(rowSums(im[2:ncol(im)])==0)){
                if(is.na(global.im)){
                    stop("global impact model must be specified for single user with empty context.")
                }
                scaler_2 = xtabs2(data=data,obs.col=y,wgt.col=w,p.prior=T)
                scaler_2[,y]=factor(scaler_2[,y],levels=levels(data[,y]))
                im_2 <- as.data.frame.table(
                    xtabs(data=scaler_2,as.formula(paste("p.prior~",y))))
                
                lambda_2 = shrink.factor(nrow(data),...)
                im <- do.call(rbind,lapply(1:nrow(im),function(i){
                    if(sum(im[i,2:ncol(im)])==0){# the corresponding context is missing
                        global.sij = global.im[which(global.im$condition == im[i,"condition"]),
                                               2:ncol(global.im)]
                        cbind("condition"=im[i,"condition"],
                              weighted.sum(lambda_2,t(im_2$Freq),global.sij))
                    }else{
                        im[i,]
                    }
                }))
            }
            
        }
        

    }else{
        if(is.na(global.im)){
            stop("global impact model must be specified for single user with small records.")
        }
        im <- global.im
    }
    
    im
}

#########################################
# apply impact model
# attach the impact model to the data based on the condition specified by x.name
# input: im: output dataframe from impact.model()
apply.impact.models <- function(data, im, x.name){
    
    data.im <- merge(data, im, by.x=x.name, by.y="condition")
#     colnames(data.im)[c((ncol(data)+1):ncol(data.im))] <- 
#         paste(colnames(data.im)[c((ncol(data)+1):ncol(data.im))], 
#               x.name, sep="_")
    data.im
#     Reduce(merge,lapply(1:length(xs), function(i){
#         x = xs[i]; w = ws[i]
#         im.x <- impact.model(data, x, y, w, ...)
#         im.x <- as.data.frame.table(xtabs(data=im.x,impact~condition+cate_l1))
#         im.x <- dcast(im.x,condition~cate_l1,value.var="Freq")
#         data.im <- merge(data,im.x,by.x=x,by.y="condition")
#         colnames(data.im)[c((ncol(data)+1):ncol(data.im))] <- paste(
#             colnames(data.im)[c((ncol(data)+1):ncol(data.im))],
#             x,sep="_")
#         data.im
#     }) )  
}


#########################################
# shrink.factor(): lambda
# input: n, k, f
# output: lambda
shrink.factor <- function(n, k=15, f=5){
    1 / (1 + exp( -1 * (n - k) / f ))
}

weighted.sum <- function(lambda, p1, p2){
    lambda * p1 + (1-lambda) * p2
}




#########################################
# plot how lamdba varies with k and f
# x <- seq(0,40,length.out = 100)
# k <- c(4, 10, 20)
# f <- c(0.5, 1, 2)
# lambda <- do.call(rbind,lapply(k, function(ki){
#     do.call(rbind,lapply(f, function(fj){
#         lambda = shrink.factor(x, ki, fj)
#         data.frame(x, lambda, k=ki, f=fj)
#     }))
# }))
# lambda$k = as.factor(lambda$k)
# lambda$f = as.factor(lambda$f)
# 
# gg.shrink <- ggplot(lambda, aes(x=x,y=lambda))+
#     geom_line(aes(color=k,linetype=f))+
#     labs(x="n",y=expression(paste(lambda[n])))+
#     theme_bw(base_size = 12) %+replace%
#     theme(legend.position=c(0.8,0.3),legend.box="horizontal")
# png("shrink.factor.png",width=1200,height=800,res=300)
# gg.shrink
# dev.off()