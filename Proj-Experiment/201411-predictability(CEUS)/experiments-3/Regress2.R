# setwd("D:\\TempM")
setwd("D:\\GitRepos\\work\\experiments\\experiments-3")
sink("log.txt", append=TRUE, split=FALSE)
print("**************************************************************")
Sys.time()

print("Test new configs. Because of memeory limit, the interaction columns")
print("has been precalculated based on 40*36 grids and 20 grid clusters ")


library(nnet)
source("fun/grids.R")
source("fun/regress.job.R")
load("data/checkin.active2.Rda")
print("successfully load the data")


for(i in c(16:25)){
    base = paste("JOB",i,sep="")
    print(paste(Sys.time(),"working on", base,"*************"))
    
    source(paste(base, "/configs.R", sep=""))
    
    result = c(regress.job(checkin.active2), 
               cols, rows, ncls.grid, job.id, job.group.id )
    write.table(t(result), file="result16-26.csv", sep=",", 
                col.names=FALSE,append=TRUE)
}
    
sink()
        





