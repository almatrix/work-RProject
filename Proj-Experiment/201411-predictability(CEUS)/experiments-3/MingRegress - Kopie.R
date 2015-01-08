setwd("D:\\GitRepos\\work\\experiments\\experiments-3")
sink("log.txt", append=FALSE, split=FALSE)

library(nnet)
source("fun/grids.R")
source("fun/regress.job.R")
load("data/checkin_active.Rda")
print("successfully load the data")

result = data.frame()
for(i in c(16:30)){
    base = paste("JOB",i,sep="")
    print(paste("*****************working on", base,"*****************"))
    
    source(paste(base, "/configs.R", sep=""))
    
    result = rbind(result, 
                   c(regress.job(checkin.active, base),
                     
                     cols, rows,
                     
                     ncls.grid, ncls.hour,
                     
                     ncls.ia.h.g,ncls.ia.h.l,ncls.ia.h.w,
                     ncls.ia.g.l,ncls.ia.g.w,ncls.ia.l.w
                     
#                      ncls.ia.h.g.l,ncls.ia.h.g.w,
#                      ncls.ia.h.l.w,ncls.ia.g.l.w,
#                      
#                      ncls.ia.h.g.l.w
                     ) )
    
    # reset configs
    ncls.grid = -1
    ncls.hour = -1
    
    ncls.ia.h.g=-1
    ncls.ia.h.l=-1
    ncls.ia.h.w=-1
    ncls.ia.g.l=-1
    ncls.ia.g.w=-1
    ncls.ia.l.w=-1
    
#     ncls.ia.h.g.l=-1
#     ncls.ia.h.g.w=-1
#     ncls.ia.h.l.w=-1
#     ncls.ia.g.l.w=-1
#     
#     ncls.ia.h.g.l.w = -1

    

}
    
colnames(result)= c("t.prep", "t.regress","edf","nsunits","rate",
                    "cols", "rows", "ncls.grid",
                    "ncls.ia.g.l","ncls.hour",
                    "ncls.ia.h.l","ncls.ia.h.w",
                    "ncls.ia.w.l","ncls.ia.h.g",
                    "ncls.ia.w.g")
        
save(result,file="result3.Rda")




