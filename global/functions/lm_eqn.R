lm_eqn = function(df,x,y){
    m = lm(y ~ x, df);
#     eq <- substitute(atop(italic(y) == a + b ~ italic(x),
#                           italic(r)^2~"="~r2*","~~italic(p)~"="~pvalue), 

#     eq <- substitute(atop(italic(y) == a + b ~ italic(x)*","~~italic(r)^2~"="~r2*","~~italic(p)~"="~pvalue), 
    eq <- substitute(italic(y) == a + b ~ italic(x)*","~~italic(r)^2~"="~r2,
                     list(a = format(coef(m)[1], digits = 2), 
                          b = format(coef(m)[2], digits = 2), 
                          r2 = format(summary(m)$r.squared, digits = 3),
                          pvalue = format(summary(m)[[4]][[8]],scientific=TRUE, digits = 3)))
    as.character(as.expression(eq));                 
}

lm_eqn_log= function(df,x,y){
    m = lm(y ~ log(x), df);
    #     eq <- substitute(atop(italic(y) == a + b ~ italic(x),
    #                           italic(r)^2~"="~r2*","~~italic(p)~"="~pvalue), 
    
    #     eq <- substitute(atop(italic(y) == a + b ~ italic(x)*","~~italic(r)^2~"="~r2*","~~italic(p)~"="~pvalue), 
    eq <- substitute(italic(y) == a + b %.% log ( italic(x) )*","~~italic(r)^2~"="~r2,
                     list(a = format(coef(m)[1], digits = 2), 
                          b = format(coef(m)[2], digits = 2), 
                          r2 = format(summary(m)$r.squared, digits = 3),
                          pvalue = format(summary(m)[[4]][[8]],scientific=TRUE, digits = 3)))
    as.character(as.expression(eq));                 
}

glm_eqn_exp = function(...){
    model = glm(...)
    
    a=model$coefficients[1]
    k=model$coefficients[2]
    
    eq <- substitute(italic(y) == e^a %.% italic(x)^k,
                     list(a = format(gg.personal.stat[[1]]$a, digits = 3), 
                          k = format(gg.personal.stat[[1]]$k, digits = 3)))
    as.character(as.expression(eq))
}

