#TO DO
#Random Forest Feature Importance
#Correlation with target

rm(list=ls())
library(randomForest)
library(caret)
library(doMC)
library(e1071)
library(caTools)
library(mice)
library(caret)
library(caretEnsemble)
registerDoMC(4)
setwd("/Users/krishna/MOOC/Edge/Kaggle")
train = read.csv("train2016.csv",na.strings=c("","NA"))
test = read.csv("test2016.csv",na.strings=c("","NA"))
train$YOB[train$YOB == 2039] = NA

str(train)

imputedata2 <- function(train,test) {
  AddCol = c('Q98197','Q109244','Q113181','Q115611','Q119851',
             'Q120379','Q120472','Q120978','Q121011')
  train =train[,names(train) %in% AddCol]
  test =test[,names(test) %in% AddCol]
  train$Tag = "Train"
  test$Tag = "Test"
  completeData = rbind(train,test)
  cols.num = colnames(completeData)
  completeData[,cols.num] = lapply(completeData[,cols.num] , factor)
  completeData[cols.num] <- sapply(completeData[cols.num], as.numeric)
  completeData[cols.num] <- apply(completeData[cols.num], 2, function(x) {x[is.na(x)] <- 0; x})
  completeData[,cols.num] = lapply(completeData[,cols.num] , factor)
  completeData[cols.num] <- apply(completeData[cols.num], 2, function(x) {x[x == 1] <- -1; x})
  completeData[cols.num] <- apply(completeData[cols.num], 2, function(x) {x[x == 2] <- 1; x})
  completeData[,cols.num] = lapply(completeData[,cols.num] , factor)
  return(completeData)
}

imputedata1 <- function(train,test) {
  imputeCols = c('Gender','Income','HouseholdStatus','EducationLevel','YOB','USER_ID')
  library(mice)
  train = train[, (colnames(train) %in% imputeCols)]
  test =  test[, (colnames(test) %in% imputeCols)]
  completeData = rbind(train,test)
  imputed = complete(mice(completeData,meth = "rf", ntree = 20),5)
  return(imputed)
}

imputedata2 <- function(train,test) {
  removeCols = c('Gender','Income','HouseholdStatus','EducationLevel','YOB','USER_ID','Party')
  train =train[,!names(train) %in% removeCols]
  test =test[,!names(test) %in% removeCols]
  train$Tag = "Train"
  test$Tag = "Test"
  completeData = rbind(train,test)
  cols.num = colnames(completeData)
  completeData[,cols.num] = lapply(completeData[,cols.num] , factor)
  completeData[cols.num] <- sapply(completeData[cols.num], as.numeric)
  completeData[cols.num] <- apply(completeData[cols.num], 2, function(x) {x[is.na(x)] <- 0; x})
  completeData[,cols.num] = lapply(completeData[,cols.num] , factor)
  completeData[cols.num] <- apply(completeData[cols.num], 2, function(x) {x[x == 1] <- -1; x})
  completeData[cols.num] <- apply(completeData[cols.num], 2, function(x) {x[x == 2] <- 1; x})
  return(completeData)
}

imputedB = imputedata1(train,test)
imputedA = imputedata2(train,test)
imputedC = cbind(imputedB,imputedA)

trainC <- subset(imputedC, Tag== 1)
testC <- subset(imputedC, Tag == -1)

# Remove Tag
rmTag ="Tag"
trainC = trainC[, !(colnames(trainC) %in% rmTag)]
testC =  testC[, !(colnames(testC) %in% rmTag)]
dataC = cbind(trainC,train[,c('Party')])


write.table(dataC,"imputeTrain.csv", row.names=FALSE,sep="|",quote=F)
write.table(testC,"imputeTest.csv", row.names=FALSE,sep="|",quote=F)



imputedB = imputedata2(train,test)
str(imputedB$Tag)

trainImp <- subset(imputedB, Tag== 1)
testImp <- subset(imputedB, Tag == -1)

# Generate Complete DataSet
ColsAdd = c('Gender','Income','HouseholdStatus','EducationLevel','YOB','Party')
trainC = cbind(train[,ColsAdd],trainImp)
ColsAdd1 = c('Gender','Income','HouseholdStatus','EducationLevel','YOB')
testC = cbind(test[,ColsAdd1],testImp)

rmTag ="Tag"
trainC = trainC[, !(colnames(trainC) %in% rmTag)]
testC =  testC[, !(colnames(testC) %in% rmTag)]


Tcontrol <- trainControl(method="cv", number=5)
testmodel = train(Party ~ . , trControl=Tcontrol, data=trainC, method="gbm")
print(testmodel)


model_list <- caretList(
  Party ~ . , data=trainC,
  #methodList= c('glmnet','knn','gbm','rf','svmPoly','bagFDA'),
  methodList= c('glmnet'),
  trControl=trainControl(savePredictions="final")
)

modelCor(resamples(model_list))
predictTest = predict(testmodel, testC)

pred  = apply(predictTest, 1, max)
op =cbind.data.frame(test$USER_ID,pred)
colnames(op) <-  c("USER_ID","Predictions")
head(op)
write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)
