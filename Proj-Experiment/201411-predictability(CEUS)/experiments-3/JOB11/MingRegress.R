library(nnet)
source("../fun/grids.R")
source("../fun/regress.job.R")


Sys.time()
load("../data/checkin_active.Rda")
print("successfully load the data")

# reset configs

source("configs.R")
    
result = c(regress.job(checkin.active), 
           cols, rows, ncls.grid, job.id, job.group.id ) 

write.table(t(result), file="../result.csv", sep=",", col.names=FALSE,append=TRUE)

gc()


