
#########################################
# supporting functions impact model 
#########################################
# impact.model(): xi --> si
# input: data: raw data frame (observations)
#        x: the condition/predictor
#        y: the dependent variable
#        k, f: the parameters for shrink factor()
# output: the impact of x on y
impact.model <- function(data, x, y, k=10, f=5){
    scaler <- xtabs2(data = data, obs.col = y, 
                     cond.col = x, p.cond=T, p.prior=T)
    scaler[,y]=abbreviate(as.character(scaler[,y]))
    scaler$lambda <- with(scaler, shrink.factor(marg.freq, k, f))
    scaler$impact <- with(scaler, lambda*p.cond + (1-lambda)*p.prior)
    
    scaler[,c(y,"condition","impact")]
    #     merge(data, scaler[,c(y,"condition","impact")],
    #           by.x=c(y,x),by.y=c(y,"condition"),all.x=T)
    
}
#########################################
# shrink.factor(): lambda
# input: n, k, f
# output: lambda
shrink.factor <- function(n, k=10, f=5){
    1 / (1 + exp( -1 * (n - k) / f ))
}

#########################################
# plot how lamdba varies with k and f
x <- seq(1,40,length.out = 100)
k <- c(4, 10, 20)
f <- c(0.5, 1, 2)
lambda <- do.call(rbind,lapply(k, function(ki){
    do.call(rbind,lapply(f, function(fj){
        lambda = shrink.factor(x, ki, fj)
        data.frame(x, lambda, k=ki, f=fj)
    }))
}))
lambda$k = as.factor(lambda$k)
lambda$f = as.factor(lambda$f)

gg.shrink <- ggplot(lambda, aes(x=x,y=lambda))+
    geom_line(aes(color=k,linetype=f))+
    labs(x="n",y=expression(paste(lambda,"(n)")))+
    theme_bw(base_size = 12) %+replace%
    theme(legend.position=c(0.9,0.4))
gg.shrink
