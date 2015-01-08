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
                   c(regress.job(checkin.active),
                     
                     cols, rows,
                     ncls.grid, 

                      job.id, job.group.id ))

}
    
colnames(result)= c("t.prep", "t.regress","edf","nsunits","rate",
                    "cols", "rows", "ncls.grid","job.id","job.group.id")
        
write.table(result, file="result3.csv", sep=",", col.names=FALSE,append=FALSE)




