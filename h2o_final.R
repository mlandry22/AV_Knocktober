setwd("/users/mark/Documents/AV-knocktober-2016/models/")
library(data.table)
library(h2o)
h2o.init(nthreads=-1)

train<-fread("../input/Train.csv",stringsAsFactors = T)
train[,set:="train"]
test<-fread("../input/Test.csv",stringsAsFactors = T)
test[,set:="test"]
all<-rbind(train,test)
all[,Registration_Date:=as.Date(Registration_Date,format="%d-%b-%y")]

camps<-fread("../input/Health_Camp_Detail.csv",stringsAsFactors = T)
camps[,Camp_Start_Date:=as.Date(Camp_Start_Date,format="%d-%b-%y")]
camps[,Camp_End_Date:=as.Date(Camp_End_Date,format="%d-%b-%y")]
camps[,timeOrder:=.I]
camps[,.(timeOrder,current=timeOrder%%4,newOrder=round((timeOrder+5)/11))]
camps[,trainGroup:=timeOrder%%4]
camps[,campGroup:= ifelse(Category1=="Third","Third"
                          ,ifelse(Category1=="Second","Second"
                                  ,ifelse(Category2=="C","FirstC"
                                          ,ifelse(paste0(Category2,Category3) %in% c("E2","F1","F2","B2"),"First","Other"
                                          ))))]

patients<-fread("../input/Patient_Profile.csv",stringsAsFactors = T)
patients[,Income:=as.numeric(as.character(Income))]
patients[,Education_Score:=as.numeric(as.character(Education_Score))]
patients[,Age:=as.numeric(as.character(Age))]
patients[,First_Interaction:=as.Date(First_Interaction,format="%d-%b-%y")]

fha<-fread("../input/First_Health_Camp_Attended.csv")
sha<-fread("../input/Second_Health_Camp_Attended.csv")
tha<-fread("../input/Third_Health_Camp_Attended.csv")


all1<-merge(all,fha[,.(Patient_ID,Health_Camp_ID
                       ,F1_Donation=Donation,F1_Health_Score=Health_Score,F1_V5=V5)]
            ,by=c("Patient_ID","Health_Camp_ID"),all.x=T)
all12<-merge(all1,sha[,.(Patient_ID,Health_Camp_ID,F2_Health_Score=`Health Score`)]
             ,by=c("Patient_ID","Health_Camp_ID"),all.x=T)
all123<-merge(all12,tha,by=c("Patient_ID","Health_Camp_ID"),all.x=T)
all123[1:3,]
all123[,y:=0]
all123[!is.na(F1_Health_Score),y:=1]
all123[!is.na(F2_Health_Score),y:=1]
all123[Number_of_stall_visited>0,y:=1]
all123[set=="train",.N,y]

#patients
full<-merge(all123,patients,by="Patient_ID",all.x=T)
full<-merge(full,camps,by="Health_Camp_ID")
full[,nCamp:=.N,Health_Camp_ID]

patStart<-Sys.time()  ## getting a little long; consider using ifs instead of ifelse
patientFeatures<-full[
  set=="train",
  .(campsAttended=.N
    ,yTtl=sum(y)
    ,ttlDonated=sum(as.numeric(F1_Donation),na.rm=T)
    ,ttlF1Score=sum(ifelse(is.na(F1_Health_Score),0.0,F1_Health_Score*1.0))
    ,cntF1Score=sum(ifelse(is.na(F1_Health_Score),0.0,1))
    ,ttlF2Score=sum(ifelse(is.na(F2_Health_Score),0.0,F2_Health_Score*1.0))
    ,cntF2Score=sum(ifelse(is.na(F2_Health_Score),0.0,1.0))
    ,ttlF3Stalls=sum(ifelse(is.na(Number_of_stall_visited),0.0,Number_of_stall_visited*1.0))
    ,cntF3Sstalls=sum(ifelse(is.na(Number_of_stall_visited),0.0,1.0))
    ,y60=sum(ifelse(campGroup=="Third",y*1.0,0.0))
    ,n60=sum(ifelse(campGroup=="Third",1.0,0.0))
    ,y50=sum(ifelse(campGroup=="Second",y*1.0,0.0))
    ,n50=sum(ifelse(campGroup=="Second",1.0,0.0))
    ,y30=sum(ifelse(campGroup=="FirstC",y*1.0,0.0))
    ,n30=sum(ifelse(campGroup=="FirstC",1.0,0.0))
    ,y10=sum(ifelse(campGroup=="First",y*1.0,0.0))
    ,n10=sum(ifelse(campGroup=="First",1.0,0.0))
  ),Patient_ID]
print(paste("patientFeatures query took:",Sys.time()-patStart))

allPatientFeatures<-full[,.(allCampsAttended=.N),Patient_ID][order(allCampsAttended)]
allPatientFeatures[,patCountId:=.I]

full<-merge(full,patientFeatures,by="Patient_ID",all.x=T)
full<-merge(full,allPatientFeatures,by="Patient_ID",all.x=T)
full[,allCampsAttended:=as.integer(allCampsAttended-1)]
full[set=="train",campsAttended:=as.integer(campsAttended-1)]
full[set=="train",yTtl:=yTtl-y]
full[set=="train",ttlDonated:=ttlDonated-ifelse(ttlDonated>0,ttlDonated,0.0)]
full[is.na(campsAttended),campsAttended:=0]
full[is.na(yTtl),yTtl:=0]
full[,priorRate:=as.numeric(NA)]
full[campsAttended>0,priorRate:=round(yTtl/campsAttended,2)]
full[cntF1Score>0,F1Rate:=round(
  (ttlF1Score-ifelse(is.na(F1_Health_Score),0.0,1.0*F1_Health_Score))
  /(cntF1Score-ifelse(is.na(F1_Health_Score),0,1)),2)]
full[cntF2Score>0,F2Rate:=round(
  (ttlF2Score-ifelse(is.na(F2_Health_Score),0.0,1.0*F2_Health_Score))
  /(cntF2Score-ifelse(is.na(F2_Health_Score),0,1)),2)]
full[cntF3Sstalls>0,F3Stalls:=round(
  (ttlF3Stalls-ifelse(is.na(Number_of_stall_visited),0.0,1.0*Number_of_stall_visited))
  /(cntF3Sstalls-ifelse(is.na(Number_of_stall_visited),0,1)),2)]

full[n60>0,rt60:=round(
  (y60-ifelse(set=="train" & campGroup=="Third",y*1.0,0.0))
  /(n60-ifelse(set=="train" & campGroup=="Third",1.0,0.0)),2)]
full[n50>0,rt50:=round(
  (y50-ifelse(set=="train" & campGroup=="Second",y*1.0,0.0))
  /(n50-ifelse(set=="train" & campGroup=="Second",1.0,0.0)),2)]
full[n30>0,rt30:=round(
  (y30-ifelse(set=="train" & campGroup=="FirstC",y*1.0,0.0))
  /(n30-ifelse(set=="train" & campGroup=="FirstC",1.0,0.0)),2)]
full[n10>0,rt10:=round(
  (y10-ifelse(set=="train" & campGroup=="First",y*1.0,0.0))
  /(n10-ifelse(set=="train" & campGroup=="First",1.0,0.0)),2)]
full[,rtThis:=
       ifelse(campGroup=="Third",rt60/0.6
              ,ifelse(campGroup=="Second",rt50/0.5
                      ,ifelse(campGroup=="FirstC",rt30/0.3
                              ,ifelse(campGroup=="First",rt10/0.1
                                      ,NA))))]


full[,sinceFirst:=as.numeric(Camp_Start_Date-First_Interaction)]
full[,firstToRegister:=as.numeric(Registration_Date-First_Interaction)]
full[,campLength:=as.numeric(Camp_End_Date-Camp_Start_Date)]
full[,registrationLeadDays:=as.numeric(Camp_Start_Date-Registration_Date)]
full[,registrationLeadDaysAlt:=as.numeric(Camp_End_Date-Registration_Date)]
full[,allVarsTtl:=Var1+Var2+Var3+Var4+Var5]
full[,anythingShare:=pmin(1,Online_Follower+LinkedIn_Shared+Twitter_Shared+Facebook_Shared)]
full[,ttlShare:=Online_Follower+LinkedIn_Shared+Twitter_Shared+Facebook_Shared]

patientPivot<-dcast(data=full[,.(Patient_ID,Health_Camp_ID,y=1)]
                    ,formula=Patient_ID~Health_Camp_ID
                    ,fun.aggregate=sum(y)
                    ,fill=0
                    ,value.var="y")
patientPivot[,patientCampHistory:=paste0(`6523`,`6524`,`6525`,`6526`,`6527`,`6528`,`6529`,`6530`,`6531`,`6532`,`6533`,`6534`
                                         ,`6535`,`6536`,`6537`,`6538`,`6539`,`6540`,`6541`,`6542`,`6543`,`6544`,`6545`,`6546`
                                         ,`6547`,`6548`,`6549`,`6550`,`6551`,`6552`,`6553`,`6554`,`6555`,`6556`,`6557`,`6558`
                                         ,`6559`,`6560`,`6561`,`6562`,`6563`,`6564`,`6565`,`6566`,`6567`,`6568`,`6569`,`6570`
                                         ,`6571`,`6572`,`6573`,`6574`,`6575`,`6576`,`6577`,`6578`,`6579`,`6580`,`6581`,`6582`
                                         ,`6583`,`6584`,`6585`,`6586`,`6587`)]
#full[,patientCampHistory:=NULL]  ## quick reset
full<-merge(full,patientPivot[,.(Patient_ID,patientCampHistory)],by="Patient_ID",all.x=T)

full[,rand1:=runif(nrow(full),0,1)]
full[,rand2:=runif(nrow(full),0,1)]


#####################################################################################
## Ideas
# ** done: diff btw registration and camp
# ** done: account for test camp membership (currently not counted whatsoever)
# ** encode ID numbers by camp involvement to stay compliant (paste)
# ** done: encode people's rates per style of camp (3 separate features)
# ** done: try a single feature that shows propensity to be above the baseline as well, taking into account type of camp
# ** done: think about ensuring NaN look like NA; otherwise leaking
# rolling participation and success rates, by patient
# analyze residuals
# need better abstraction of patients
# could average together leave-one-camp-out models
# blending? RF,GLM,DL are not great. Even different GBMs are not great. How else to get variance?
###### Final
# will need to reproduce the final models from scratch in one merged script
# set seeds, just in case
###### Observations
## are these related:
# 490196, 513633 ,  7/28  --yes
# 493366, 499917 ,  8/24  -- no
# 494149, 509122 , 15/24  --yes

#####################################################################################

######################################
### [Re]load data into H2O starting here
######################################

allHex<-as.h2o(full,destination_frame = "all.hex")
allHex$patientCampHistory<-as.factor(allHex$patientCampHistory)

trainHex<-allHex[allHex$set=="train",]
testHex<-allHex[allHex$set=="test",]
trainHex$y<-as.factor(trainHex$y)

predictors<-colnames(trainHex)[!colnames(trainHex) %in% 
                                 c( ## compliance fields
                                   "Patient_ID","Health_Camp_ID"
                                   ## invalid fields
                                   ,"F2_Health_Score","F1_Donation","F1_Health_Score","F1_V5","y"
                                   ,"Last_Stall_Visited_Number","Number_of_stall_visited","timeOrder","Camp_Start_Date"
                                   ,"Registration_Date","set","Camp_End_Date"
                                   ,"cntF1Score","ttlF1Score","cntF2Score","ttlF2Score","cntF3Sstalls","ttlF3Stalls"
                                   ,"y60","n60","y50","n50","y30","n30","y10","n10"
                                   ## ineffective predictors
                                   ,"ttlDonated","ttlShare","anythingShare","rand1","rand2","patientCampHistory"
                                   ,"campsAttended" ## replaced to include test; kept for rate calculation only
                                 )]
gbmName<-"g32-final"
g<-h2o.gbm(training_frame = trainHex[trainHex$timeOrder,],x=predictors,y="y" #,fold_column="trainGroup"
           ,ntrees=413,learn_rate = 0.01,sample_rate = 0.6,col_sample_rate_per_tree = 0.6   ##
           ,max_depth = 4,score_tree_interval = 50 #,stopping_rounds = 4,stopping_tolerance = 0
           ,pred_noise_bandwidth = 0.5
           ,model_id = gbmName)

p<-as.data.frame(h2o.predict(h2o.getModel(gbmName),testHex))
submission<-full[set=="test",.(Health_Camp_ID,Patient_ID,Outcome=y)]
submission[,Outcome:=(p$p1)]
summary(submission)
write.csv(submission,paste0(gbmName,".csv"),row.names=F)

###########################################################################
###########################################################################
###########################################################################

gbmName<-"g22-final"
g<-h2o.gbm(training_frame = trainHex[trainHex$timeOrder,],x=predictors,y="y" #,fold_column="trainGroup"
           ,ntrees=158,learn_rate = 0.03,sample_rate = 0.7,col_sample_rate_per_tree = 0.7   ##
           ,max_depth = 4,score_tree_interval = 50 #,stopping_rounds = 4,stopping_tolerance = 0
           ,pred_noise_bandwidth = 0.2
           ,model_id = gbmName)

p<-as.data.frame(h2o.predict(h2o.getModel(gbmName),testHex))
submission<-full[set=="test",.(Health_Camp_ID,Patient_ID,Outcome=y)]
submission[,Outcome:=(p$p1)]
summary(submission)
write.csv(submission,paste0(gbmName,".csv"),row.names=F)

###########################################################################
###########################################################################
###########################################################################

gbmName<-"segFirst-Final"
g<-h2o.gbm(training_frame = trainHex[trainHex$campGroup=="First",],x=predictors,y="y" #,fold_column="trainGroup"
           ,ntrees=275,learn_rate = 0.01,sample_rate = 0.7,col_sample_rate_per_tree = 0.7
           ,score_tree_interval = 50,max_depth = 4#,stopping_rounds = 4,stopping_tolerance = 0
           ,model_id = gbmName)

p<-as.data.frame(h2o.predict(h2o.getModel(gbmName),testHex[testHex$campGroup=="First",]))
subFirst<-full[set=="test" & campGroup=="First",.(Health_Camp_ID,Patient_ID,Outcome=y)]
subFirst[,Outcome:=p$p1]

gbmName<-"segNotFirst-Final"
g<-h2o.gbm(training_frame = trainHex[trainHex$campGroup!="First",],x=predictors,y="y" #,fold_column="trainGroup"
           ,ntrees=337,learn_rate = 0.01,sample_rate = 0.7,col_sample_rate_per_tree = 0.7
           ,score_tree_interval = 50,max_depth = 4 #,stopping_rounds = 4,stopping_tolerance = 0
           ,model_id = gbmName)

p<-as.data.frame(h2o.predict(h2o.getModel(gbmName),testHex[testHex$campGroup!="First",]))
subNotFirst<-full[set=="test" & campGroup!="First",.(Health_Camp_ID,Patient_ID,Outcome=y)]
subNotFirst[,Outcome:=p$p1]

segName<-"segmented2-Final"
submission<-rbind(subFirst,subNotFirst)
summary(submission); nrow(submission)
write.csv(submission,paste0(segName,".csv"),row.names=F)


###########################################################################
###########################################################################
###########################################################################

rfName<-"rf1-final"
rf<-h2o.randomForest(training_frame = trainHex,x=predictors,y="y" #,fold_column="trainGroup"
                     ,ntrees=200 #,  sample_rate = 0.5,col_sample_rate_per_tree = 0.7, col_sample_rate=0.7
                     ,score_tree_interval = 10
                     ,histogram_type = "QuantilesGlobal"
                     ,model_id = rfName)

p<-as.data.frame(h2o.predict(h2o.getModel(rfName),testHex))
submission<-full[set=="test",.(Health_Camp_ID,Patient_ID,Outcome=y)]
submission[,Outcome:=p$p1]
summary(submission)
write.csv(submission,paste0(rfName,".csv"),row.names=F)

###########################################################################
###########################################################################
###########################################################################

predictors34<-colnames(trainHex)[!colnames(trainHex) %in% 
                                 c( ## compliance fields
                                   "Patient_ID","Health_Camp_ID"
                                   ## invalid fields
                                   ,"F2_Health_Score","F1_Donation","F1_Health_Score","F1_V5","y"
                                   ,"Last_Stall_Visited_Number","Number_of_stall_visited","timeOrder","Camp_Start_Date"
                                   ,"Registration_Date","set","Camp_End_Date"
                                   ,"cntF1Score","ttlF1Score","cntF2Score","ttlF2Score","cntF3Sstalls","ttlF3Stalls"
                                   ,"y60","n60","y50","n50","y30","n30","y10","n10"
                                   ## ineffective predictors
                                   ,"ttlDonated","ttlShare","anythingShare","rand1","rand2"
                                   #,"patientCampHistory"
                                   ,"patCountId"  ## flipping this and patientCampHistory for this submission
                                   ,"campsAttended" ## replaced to include test; kept for rate calculation only
                                 )]
gbmName<-"g34-final"    ## 0.83435308036
g<-h2o.gbm(training_frame = trainHex[trainHex$timeOrder,],x=predictors34,y="y" #,fold_column="trainGroup"
           ,ntrees=450,learn_rate = 0.01,sample_rate = 0.6,col_sample_rate_per_tree = 0.6   ##
           ,max_depth = 4,score_tree_interval = 50 #,stopping_rounds = 4,stopping_tolerance = 0
           ,pred_noise_bandwidth = 0.5
           ,model_id = gbmName)

p<-as.data.frame(h2o.predict(h2o.getModel(gbmName),testHex))
submission<-full[set=="test",.(Health_Camp_ID,Patient_ID,Outcome=y)]
submission[,Outcome:=(p$p1)]
summary(submission)
write.csv(submission,paste0(gbmName,".csv"),row.names=F)


###########################################################################
###########################################################################
###########################################################################

## this function was missing from the original version pushed
## thanks, @roger
processFile<-function(fileName){
  a<-fread(fileName)[order(Outcome)]
  a[,rk:=.I/nrow(a)]
  a<-a[order(Patient_ID,Health_Camp_ID)]
  print(nrow(a))
  return(a)
}


blendFileLocations<-c("g32-final.csv"
                      ,"g22-final.csv"
                      ,"segmented2-final.csv"
                      ,"rf1-final.csv"
                      ,"g34-final.csv")
blendWeights<-c(7,1,1,1,2)
blendWeights<-blendWeights*(1/sum(blendWeights))
print(sum(blendWeights))
blendList<-list()
for(i in 1:length(blendFileLocations)){
  blendList[[i]]<-processFile(blendFileLocations[i])
}
blend<-blendList[[1]]
blend[,Outcome:=0]
for(i in 1:length(blendFileLocations)){
  blend[,Outcome:=Outcome+blendList[[i]][,rk]*blendWeights[i]]
#  blend[,Outcome:=Outcome+blendList[[i]][,Outcome]*blendWeights[i]]
}
summary(blend)
blendName<-"blend-Final"
write.csv(blend[,.(Health_Camp_ID,Patient_ID,Outcome)],paste0(blendName,".csv"),row.names=F)
