#########################################
# settings & configurations
configs = read.csv("configs.txt",header = F)

city.index = configs[1,1]
city.sp.attr = c("ZIP","Zip_Num","POSTAL")[city.index]
city.name = c("Chi","LA","NYC")[city.index]
k.threshold = configs[1,2]


learning.ratio = configs[1,3]
weight.base = configs[1,4]
node.interval = configs[1,5]
node.id = configs[1,6] 

rm(configs)

node = c("id" = node.id, 
        "start" = (node.id - 1)* node.interval + 1,
        "end" = node.id * node.interval)

sink("logs.txt")

# libraries
library(nnet)

# load the function
source("../../functions.R")

print(Sys.time())
tm.init = watch.time.memory()

#########################################
# data preparation
#load("../data/checkin.city.st.Rda")
load(paste("../../data/checkin.",city.name,".st_",k.threshold,".Rda",sep=""))
checkin.city.st = checkin.city.st[order(checkin.city.st$timestamps),]
checkin.city.st$cate_l1 = as.character(checkin.city.st$cate_l1)
# split the dataset
learning.length = as.integer(nrow(checkin.city.st) * learning.ratio)
checkin.city.st.learning = checkin.city.st[c(1:learning.length),]
checkin.city.st.predicting = checkin.city.st[c((learning.length+1):nrow(checkin.city.st)),]

predicting.list = split(checkin.city.st.predicting, checkin.city.st.predicting$user_id)
# print the general statistics of the data
print(c("sample.size"=nrow(checkin.city.st),
        "learning.size"=nrow(checkin.city.st.learning),
        "predicting.size"=nrow(checkin.city.st.predicting),
        "predicting.user"=length(predicting.list),
        "learning.user (approx)"=with(as.data.frame(xtabs(~user_id,data=checkin.city.st.learning)),sum(Freq>30)))
)
# representing each user with his/her visiting vector
user.vectors <- xtabs2(data=checkin.city.st.learning, obs.col="cate_l1",
                       cond.col="user_id", p.cond=T)
user.vectors <- reshape(user.vectors[,c("condition","cate_l1","p.cond")], 
                        v.names = "p.cond", idvar = "condition", 
                        timevar = "cate_l1", direction = "wide")
user.vectors[is.na(user.vectors)] <- 0 


# global model
gmodel <- multinom(cate_l1 ~ cluster.hour + cluster.ZIP, maxit = 1000, 
                   data = checkin.city.st.learning)

counter.reset()
# prediction based on global/personalized model
prediction <- do.call(rbind,lapply(predicting.list[
    node["start"]:min(node["end"],length(predicting.list))], 
    function(target.user){

    counter.print(1)
    # build model for this target user 
    uid = target.user[1,"user_id"]
    predicting.size = nrow(target.user)
    
    # do we have enough historial data for this user?
    # if yes: adding weights based on user similarity
    # if no: use all learning data with equal weights
    learning.size = sum(checkin.city.st.learning$user_id == uid)
    if(learning.size>30){
        ulearning <- checkin.city.st.learning
        target.vv <- user.vectors[
            which(user.vectors$condition == uid),2:11]
        
        weight <- do.call(rbind,lapply(split(ulearning,ulearning$user_id),
            function(user){
                user.vv <- user.vectors[
                    which(user.vectors$condition == user[1,"user_id"]),2:11]
                distance = dist(rbind(target.vv,user.vv))[1]
                data.frame("user_id"=user[1,"user_id"],
                           "weights"=weight.base^distance)
                }))
        
        ulearning = merge(x=ulearning, y=weight, by="user_id")
        
        # simplify the learning data
        ulearning <- ulearning[which(ulearning$weights>mean(ulearning$weights)),]
        
        umodel <- multinom(cate_l1 ~ cluster.hour + cluster.ZIP,
                           data = ulearning,  
                           weights = weights, maxit = 1000)
    } else {
        umodel <- gmodel
    }
    
    # make prediction based on the model
    cbind(
        data.frame(
            "user_id"=uid,
            "city"=city.name,
            "k.threshold"=k.threshold,
            "learning.ratio"=learning.ratio,
            "weight.base"=weight.base,
            "usermodel"=sum(target.user$cate_l1==predict(umodel, target.user))/predicting.size,
            "globalmodel"=sum(target.user$cate_l1==predict(gmodel, target.user))/predicting.size,
            "learning.size"=learning.size,
            "predicting.size"=predicting.size),
        model.fitness(umodel),
        model.comparison(gmodel,umodel))

}))

watch.time.memory(tm.init[1],tm.init[2])

save(prediction,file=paste("../result/prediction_",
                           tail(strsplit(getwd(),"/")[[1]],n=1),
                           ".Rda",sep=""))
