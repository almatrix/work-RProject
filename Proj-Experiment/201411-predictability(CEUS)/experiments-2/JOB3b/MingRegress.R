library(nnet)
source("../grids.R")

# #################### configurations ########################
cols = 40
rows = 36
ncls.hour = 6
ncls.grid = 20
regression.formula = paste("cate_l1 ~", "hour.cid", 
                           "+ ugrid.id.cid",
                           "+ isweekend",
                           "+ last.cate_l1")
# #############################################################



# #############################################################
load("../checkin_active.Rda")
print("successfully load the data")


# dealing with the dataframe
ptm <- proc.time()
checkin.active.in.grids = arrange.in.grids(checkin.active, cols, rows)

checkin.active.in.grids = reduce.ranks.by.dendrogram(
    checkin.active.in.grids, "cate_l1", "hour", ncls.hour)

checkin.active.in.grids = reduce.ranks.by.dendrogram(
    checkin.active.in.grids, "cate_l1", "ugrid.id", ncls.grid)




# report time duration
print(paste("Data preparation finished after",proc.time()- ptm,"seconds"))
ptm <- proc.time()


# regression 1
# freq = as.data.frame(xtabs(~cate_l2+hour.cid+grid.cid+isweekend+last.cate_l1, 
#                                     data=checkin.active.in.grids))
# loglin.sat.cate.hour = glm(Freq~cate_l2*hour.cid*grid.cid*isweekend*last.cate_l1, 
#                           data=freq, family=poisson)
# summary(loglin.sat.cate.hour)
# anova(loglin.sat.cate.hour)


# regression 
tmodel1<-multinom(as.formula(regression.formula),
        data=checkin.active.in.grids,maxit = 2000,MaxNWts = 2000)
save(tmodel1,file="tmodel1.Rda")
# report time duration
print(paste("Regression model converged in",proc.time()- ptm,"seconds"))
ptm <- proc.time()


# investigate into the coefficient
tsummary1 = summary(tmodel1)
z1 = tsummary1$coefficients/tsummary1$standard.errors
p1 = (1 - pnorm(abs(z1), 0, 1)) * 2
save(p1,file="p1.Rda")
# report time duration
print(paste("Model summary finished after",proc.time()- ptm,"seconds"))
ptm <- proc.time()


# invesigate into the model
prediction = apply(tmodel1$fitted.values, 1, FUN=function(i)which(i==max(i)))
real=checkin.active.in.grids$cate_l1
comparison = data.frame("real"=real,"prediction"=levels(real)[prediction])
comparison$correct = ifelse(comparison$prediction==comparison$real,1,0)
rate=sum(comparison$correct)/nrow(comparison)
print(paste(sum(comparison$correct), "correct predictions out of",
            nrow(comparison),". Rate:",rate))

