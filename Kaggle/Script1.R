rm(list=ls())
library(randomForest)
library(caret)
library(doMC)
library(e1071)
library(caTools)
library(mice)
library(caret)
library(nnet)
library(caretEnsemble)
registerDoMC(4)
setwd("/Users/krishna/MOOC/Edge/Kaggle/Imputes")

traindf = read.csv("imputeTrain.csv",na.strings=c("","NA"),sep='|')
testdf =  read.csv("imputeTest.csv",na.strings=c("","NA"),sep='|')

# Fix Education Level
levelsE=c("Current K-12", "High School Diploma", "Current Undergraduate","Associate's Degree", "Bachelor's Degree","Master's Degree", "Doctoral Degree")
traindf$EducationLevel = ordered(traindf$EducationLevel, levels = levelsE)
testdf$EducationLevel = ordered(testdf$EducationLevel, levels = levelsE)

# Fix Income
levelsI = c("under $25,000","$25,001 - $50,000","$50,000 - $74,999","$75,000 - $100,000","$100,001 - $150,000","over $150,000")
traindf$Income = ordered(traindf$Income, levels= levelsI)
testdf$Income = ordered(testdf$Income, levels= levelsI)

# Fix YOB as factor
traindf$YOB = (traindf$YOB)
testdf$YOB = (testdf$YOB)
traindf$YOB = as.factor((traindf$YOB))
testdf$YOB = as.factor((testdf$YOB))

table(traindf$HouseholdStatus)
features =c('YOB','Gender','Income','HouseholdStatus','EducationLevel','Party'
            ,'Q98197','Q109244','Q113181','Q115611','Q119851',
            'Q120379','Q120472','Q120978','Q121011','Q98869')

trainC = traindf[, (names(traindf) %in% features)]
testC = testdf[, (names(testdf) %in% features)]


Tcontrol <- trainControl(method="cv", number=5)
testmodel = train(Party ~ . + EducationLevel:Income ,
                  trControl=Tcontrol, data = traindf, method=c('gbm'))
print(testmodel)


op= predict(testmodel,testdf)
op =cbind.data.frame(testdf$USER_ID  ,op)
colnames(op) <-  c("USER_ID","Predictions")
write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)




#sapply(testC, function(x) sum(is.na(x)))

model_list <- caretList(
  Party ~ . , data=trainC,
  methodList= c('pda','widekernelpls','glmnet','rpart','knn'),
  trControl=trainControl(savePredictions="final",verboseIter = T,
                         classProbs=TRUE)
)

modelCor(resamples(model_list))


gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:30)*50,
                        n.minobsinnode = 10)

glm_ensemble <- caretStack(
  model_list,
  method='gbm',
  tuneGrid = gbmGrid
)

op= predict(glm_ensemble,testC)
names(op) = c('a','b','c')
head(op)
Predictions  = apply(as.data.frame(op), 1, max)
length(Predictions)
op =cbind.data.frame(testdf$USER_ID  ,op$c)
colnames(op) <-  c("USER_ID","Predictions")
write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)









predictTest = as.data.frame(predict(model_list,testC))
head(predictTest)
Predictions  = apply(as.data.frame(predictTest), 1, mean)
Predictions
pred<- ifelse(round(Predictions) == 0, 'Democrat', 'Republican')
op =cbind.data.frame(testdf$USER_ID  ,pred)
colnames(op) <-  c("USER_ID","Predictions")
write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)







plot(testmodel)








predictdf = predict(model5,testC)
head(predictdf)
Predictions  = apply(as.data.frame(predictdf), 1, mean)
pred<- ifelse(round(Predictions) == 1, 'Democrat', 'Republican')


op =cbind.data.frame(testdf$USER_ID  , predict(model_list,testC))
colnames(op) <-  c("USER_ID","Predictions")
write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)


head(predictdf) 
print(testmodel)
model_list <- caretList(
  Party ~ . , data=trainC,
  #methodList= c('glmnet','knn','gbm','rf','svmPoly','bagFDA'),
  methodList= c('gbm'),
  trControl=trainControl(savePredictions="final")
)

modelCor(resamples(model_list))

predictTest = predict(model_list, testC)
head(predictTest)


pred  = apply(predictTest, 1, max)




print(predictTest)




