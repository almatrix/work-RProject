#################################################
# This doucment includes the density function,
# cummulative density function, as well as the
# output format of the truncated power law
#################################################


# density function of truncated power law
truncated.plfit.df <- function(x,x0,beta,ka){
    
    if(length(x)==1)
        p = (x+x0)^(-1*beta)*exp(-1*x/ka)
    else 
        p = sapply(x,function(i){(i+x0)^(-1*beta)*exp(-1*i/ka)})
    
    #  print(paste(x0,beta,ka,x,p))
    
    p
}

# CDF of truncated power law
truncated.plfit.cdf <- function(x,x0,beta,ka){
    if(length(x)==  1)
        Fx = sum(truncated.plfit.df(1:x,x0,beta,ka))
    else{
        Fx = sapply(x,function(i){sum(truncated.plfit.df(1:i,x0,beta,ka))})
    }
    Fx
}

# output format of the dentisty function
truncated.plfit.print <- function(x0,beta,ka){
    eq =  substitute(Pr(italic(x)) == (italic(x)+x0) ^ -beta ~ exp(-italic(x)/ka),
                     list(x0 = format(x0, digits = 3), 
                          beta = format(beta, digits = 3), 
                          ka = format(ka, digits = 3)))
    as.character(as.expression(eq))
}

