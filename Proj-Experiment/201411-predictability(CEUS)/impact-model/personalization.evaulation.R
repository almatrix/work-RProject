pers.eva = function(data=dta,...){
    # global context coding for three types of context
    im.hour = impact.model(data=dta,x="hour2",y="cate_l1",...)
    im.zip = impact.model(data=dta,x="ZIP",y="cate_l1",...)
    im.last = impact.model(data=dta,x="last.cate_l1",y="cate_l1",w="wgt",...)
    # apply the impact models
    dta.im = apply.impact.models(dta,im.hour,"hour2")
    dta.im = apply.impact.models(dta.im,im.zip,"ZIP")
    dta.im = apply.impact.models(dta.im,im.last,"last.cate_l1")
    # global model
    gmodel = multinom(data=dta.im[,c(7,14:43)], cate_l1~., 
                      maxit=2000,trace=FALSE)
    print(paste(Sys.time(),": full model finished."))
    
    # personalized result
    do.call(rbind,lapply(split(dta,dta$user_id),function(user){
        #         print(user[1,"user_id"])
        uim.hour = impact.model(data=user,x="hour2",y="cate_l1",
                                global.im = im.hour,...)
        uim.zip = impact.model(data=user,x="ZIP",y="cate_l1",
                               global.im = im.zip,...)
        uim.last = impact.model(data=user,x="last.cate_l1",y="cate_l1",
                                w="wgt",global.im = im.last,...)
        user.im = apply.impact.models(user,im.hour,"hour2")
        user.im = apply.impact.models(user.im,im.zip,"ZIP")
        user.im = apply.impact.models(user.im,im.last,"last.cate_l1")
        
        # entropy
        stat = as.data.frame(xtabs(~cate_l1,data=user))
        stat$prob = stat$Freq / sum(stat$Freq)
        stat$prob.adj = (stat$Freq+2) / (sum(stat$Freq)+4)
        entropy = -1 * sum(stat$prob * log2(stat$prob),na.rm=T)
        entropy.adj = -1 * sum(stat$prob.adj * log2(stat$prob.adj),na.rm=T)
        
        # model
        if(length(unique(user$cate_l1))>1){
            model = multinom(data=user.im[,c(7,14:43)], cate_l1~., 
                             maxit=2000,trace=FALSE)
        }else{
            model = gmodel
        }
        
        pred.g = predict(gmodel,type="class",newdata=user.im)
        pred.p = predict(model,type="class",newdata=user.im)
        
        data.frame("user_id"=user[1,"user_id"],
                   "cate_l1"=user.im$cate_l1,
                   "sample.size"=nrow(user),
                   "entropy"= entropy,
                   "entropy.adj"= entropy.adj,
                   "pred.g"=pred.g,
                   "rate.g"=sum(levels(pred.g)[pred.g]==user.im$cate_l1)/nrow(user.im),
                   "pred.p"=pred.p,
                   "rate.p"=sum(levels(pred.p)[pred.p]==user.im$cate_l1)/nrow(user.im),
                   "candidate.leng"=length(unique(user$cate_l1)))
    }))
}