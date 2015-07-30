
#########################################
# supporting functions impact model 
#########################################
# impact.model(): xi --> si
# input: data: raw data frame (observations)
#        x: the condition/predictor
#        y: the dependent variable
#        k, f: the parameters for shrink factor()
# output: the impact of x on y
impact.model <- function(data, x, y, w=NA, k=10, f=5){
    scaler <- xtabs2(data = data, obs.col = y, wgt.col = w,
                     cond.col = x, p.cond=T, p.prior=T)
    scaler[,y]=abbreviate(as.character(scaler[,y]))
    scaler$lambda <- with(scaler, shrink.factor(marg.freq, k, f))
    scaler$impact <- with(scaler, weighted.sum(lambda, p.cond, p.prior))
    
    im <- scaler[,c(y,"condition","impact")]
    #     merge(data, scaler[,c(y,"condition","impact")],
    #           by.x=c(y,x),by.y=c(y,"condition"),all.x=T)
    
    # long format to wide format
    im <- as.data.frame.table(xtabs(data=im,impact~condition+cate_l1))
    dcast(im,condition~cate_l1,value.var="Freq")
}

#########################################
# apply impact model
# transform the dataframe to table format
# input: im: output dataframe from impact.model()
apply.impact.models <- function(data,xs,y,ws,...){

    ans.global = Reduce(merge,lapply(1:length(xs), function(i){
        x = xs[i]; w = ws[i]
        im.x <- impact.model(data, x, y, w, ...)
        im.x <- as.data.frame.table(xtabs(data=im.x,impact~condition+cate_l1))
        im.x <- dcast(im.x,condition~cate_l1,value.var="Freq")
        data.im <- merge(data,im.x,by.x=x,by.y="condition")
        colnames(data.im)[c((ncol(data)+1):ncol(data.im))] <- paste(
            colnames(data.im)[c((ncol(data)+1):ncol(data.im))],
            x,sep="_")
        data.im
    }) )  
}


#########################################
# shrink.factor(): lambda
# input: n, k, f
# output: lambda
shrink.factor <- function(n, k=10, f=5){
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