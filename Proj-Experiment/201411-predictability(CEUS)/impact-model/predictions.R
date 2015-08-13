data.preparation <- function(data.regress,ratio=0.8){
    
    # some operations on spatial and temporal context
    data.regress$ZIP = factor(data.regress$ZIP)
    data.regress$weekday = as.factor(data.regress$weekday)
    data.regress$hour2 = with(data.regress,as.factor(paste(hour,weekday)))
    # some additional operations for sequential context
    # add weight based on time interval
    data.regress$wgt = 2 ^ ( -1 * data.regress$time.interval + 1)
    # remove continuous check-ins by the same user (by changing the weight to 0)
    data.regress$wgt = with(data.regress,ifelse(venue_id==last.venue_id, 0, wgt))
    # deal with the empty values
    data.regress$wgt = with(data.regress, ifelse(is.na(last.cate_l1)|time.interval>12,
                                                 0.0005,wgt))
    data.regress$last.cate_l1 = with(data.regress,
                                     as.factor(ifelse(is.na(last.cate_l1)|time.interval>12,
                                                      "Unknown",as.character(last.cate_l1))))

    # order by timestamps
    data.regress = data.regress[order(data.regress$timestamps),]
    # use 80% of the entire dataset as training data
    training.length = ceiling(nrow(data.regress)*ratio) 
    training.data = data.regress[1:training.length,]
    reference.data = data.regress[(training.length+1):nrow(data.regress),]
    
    list("training.data"=training.data,
         "reference.data"=reference.data)
}

prediction.eva <- function(training.data, reference.data, Z1=30, Z2=5){
    
    # evaluation of the global data itself (diversity of user behavior)
    gstat = as.data.frame(xtabs(~cate_l1,data=training.data))
    gentropy = get.entropy(with(gstat, Freq / sum(Freq) ))
    
    # global impact model and global regression model
    gim.hour = impact.model(data=training.data,x="hour2",y="cate_l1")
    gim.zip = impact.model(data=training.data,x="ZIP",y="cate_l1")
    gim.last = impact.model(data=training.data,x="last.cate_l1",y="cate_l1",w="wgt")
    training.data.gim = apply.impact.models(training.data, gim.hour, "hour2")
    training.data.gim = apply.impact.models(training.data.gim, gim.zip, "ZIP")
    training.data.gim = apply.impact.models(training.data.gim, gim.last, "last.cate_l1")
    gmodel = multinom(data=training.data.gim[,c(7,14:43)], 
                      cate_l1~., maxit=2000)
    
    # personalized result
    counter.reset()
    lapply(split(reference.data,reference.data$user_id),function(user.reference.data){
        uid = user.reference.data[1,"user_id"]
        counter.print(20)
        
        # how many data records exist in the training data for this user?
        user.training.data = training.data[which(training.data$user_id==uid),]
        user.training.length = nrow(user.training.data)
        # just for comparison: how many are there in the reference list?
        user.reference.length = nrow(user.reference.data)
        
        # personalized regression
        # if there is any training data for this user
        if(user.training.length > 0){
            
            # evaluation of the data itself (diversity of user behavior)
            ustat = as.data.frame(xtabs(~cate_l1,data=user.training.data))
            entropy = get.entropy(with(ustat, Freq / sum(Freq) ))
            
            # personalized impact model
            # attention: Z=30 means if training data length is smaller than 30,
            # the global impact model will actually be applied
            uim.hour = impact.model(data=user.training.data, x="hour2", y="cate_l1",
                                    Z=Z1, global.im = gim.hour)
            uim.zip = impact.model(data=user.training.data, x="ZIP", y="cate_l1",
                                   Z=Z1, global.im = gim.zip)
            uim.last = impact.model(data=user.training.data, x="last.cate_l1", y="cate_l1",
                                    Z=Z1, w="wgt", global.im = gim.last)
            
            # apply personal impact model
            user.training.im = apply.impact.models(user.training.data,uim.hour,"hour2")
            user.training.im = apply.impact.models(user.training.im,uim.zip,"ZIP")
            user.training.im = apply.impact.models(user.training.im,uim.last,"last.cate_l1")
            
            # apply personal impact model to reference data (for prediction)
            user.reference.im = apply.impact.models(user.reference.data,uim.hour,"hour2")
            user.reference.im = apply.impact.models(user.reference.im,uim.zip,"ZIP")
            user.reference.im = apply.impact.models(user.reference.im,uim.last,"last.cate_l1")
            
            # evaluate the global regression model (check the fitness/CCR and prediction/CPR)
            # using the transformed personalized impact model data 
            fit.g = predict(gmodel,type="class",newdata=user.training.im)
            pred.g = predict(gmodel,type="class",newdata=user.reference.im)
            CCR.g = sum(levels(fit.g)[fit.g]==user.training.im$cate_l1)/nrow(user.training.im)
            CPR.g = sum(levels(pred.g)[pred.g]==user.reference.im$cate_l1)/nrow(user.reference.im)
            
            # evaluate the personal regression model
            # attention: when there is only one type of response, multinom cannot be applied
            if(length(unique(user.training.im$cate_l1))>1){
                umodel = multinom(data=user.training.im[,c(7,14:43)], cate_l1~., 
                                  maxit=2000,trace=FALSE)
                # evaluation of the model 
                fit.p = predict(umodel,type="class",newdata=user.training.im)
                pred.p = predict(umodel,type="class",newdata=user.reference.im)
                CCR.p = sum(levels(fit.p)[fit.p]==user.training.im$cate_l1)/nrow(user.training.im)
                CPR.p = sum(levels(pred.p)[pred.p]==user.reference.im$cate_l1)/nrow(user.reference.im)
                note = "1"
            }
            else{
                # when there is only one type of response
                # if the number of training records exceed the theshod(3): we trust the data
                # otherwise it could just an occasion-> global model is applied
                if(user.training.length > Z2){
                    fit.p = user.training.im$cate_l1
                    pred.p = rep(unique(user.training.im$cate_l1),nrow(user.reference.im))
                    
                    CCR.p = sum(fit.p==user.training.im$cate_l1)/nrow(user.training.im) # should be 1
                    CPR.p = sum(pred.p==user.reference.im$cate_l1)/nrow(user.reference.im)
                    note = "2"
                }else{
                    umodel = gmodel 
                    # evaluation of the model (inner Correct Classification Rate)
                    fit.p = fit.g
                    pred.p = pred.g
                    CCR.p = CCR.g
                    CPR.p = CCR.g
                    note = "3"
                } 
            }
            
        }
        else{  
            # evaluation of the data itself (diversity of user behavior)
            # since there are no training data: here it should be the global value
            entropy = gentropy
            
            # personalized impact model: the same as global impact model
            # apply only be applied to reference data (for prediction)
            user.training.im = data.frame("cate_l1"=NA)
            user.reference.im = apply.impact.models(user.reference.data,gim.hour,"hour2")
            user.reference.im = apply.impact.models(user.reference.im,gim.zip,"ZIP")
            user.reference.im = apply.impact.models(user.reference.im,gim.last,"last.cate_l1")
            
            # evaluate the global regression model (check the fitness/CCR and prediction/CPR)
            # using the transformed personalized impact model data 
            fit.g = NA
            CCR.g = NA
            pred.g = predict(gmodel,type="class",newdata=user.reference.im)
            CPR.g = sum(levels(pred.g)[pred.g]==user.reference.im$cate_l1)/nrow(user.reference.im)
            
            # evaluate the personal regression model(actually the same as global model)
            fit.p = fit.g
            CCR.p = CCR.g
            pred.p = pred.g
            CPR.p = CPR.g
            
            note = "4"
        }
        
        
        
        list("inner"=data.frame("user_id"=uid,
                                "user.training.length"=user.training.length,
                                "user.reference.length"=user.reference.length,
                                "TP.ratio"=user.training.length/user.reference.length,
                                "entropy"= entropy,
                                "cate_l1"=user.training.im$cate_l1,
                                "fit.g"=fit.g,
                                "fit.p"=fit.p,
                                "CCR.g"=CCR.g,
                                "CCR.p"=CCR.p,
                                "note"=note),
             "outer"=data.frame("user_id"=uid,
                                "user.training.length"=user.training.length,
                                "user.reference.length"=user.reference.length,
                                "TP.ratio"=user.training.length/user.reference.length,
                                "entropy"= entropy,
                                "cate_l1"=user.reference.im$cate_l1,
                                "pred.g"=pred.g,
                                "pred.p"=pred.p,
                                "CPR.g"=CPR.g,
                                "CPR.p"=CPR.p,
                                "note"=note))
        
    })
                             
}