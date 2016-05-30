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

imputedA = imputedata1(train,test)


imputedB = imputedata2(train,test)
imputed = cbind(imputedA,imputedB)

trainImp <- subset(imputed, Tag== "Train")
testImp <- subset(imputed, Tag == "Test")
rmTag ="Tag"
trainImp = trainImp[, !(names(trainImp) %in% rmTag)]
testImp =  testImp[, !(colnames(testImp) %in% rmTag)]
#str(trainImp,list.len = 108)
str(trainImp)

trainMissing =train[,!names(train) %in% names(trainImp)]
testMissing =test[,!names(test) %in% names(testImp)]
traindf = cbind(trainMissing,trainImp)
testdf = cbind(testMissing,testImp)

#str(testdf)
# Fix Education Level
levelsE=c("Current K-12", "High School Diploma", "Current Undergraduate","Associate's Degree", "Bachelor's Degree","Master's Degree", "Doctoral Degree")
traindf$EducationLevel = ordered(traindf$EducationLevel, levels = levelsE)
testdf$EducationLevel = ordered(testdf$EducationLevel, levels = levelsE)

# Fix Income
levelsI = c("under $25,000","$25,001 - $50,000","$50,000 - $74,999","$75,000 - $100,000","$100,001 - $150,000","over $150,000")
traindf$Income = ordered(traindf$Income, levels= levelsI)
testdf$Income = ordered(testdf$Income, levels= levelsI)

# Fix YOB as factor
traindf$YOB = as.factor(traindf$YOB)
testdf$YOB = as.factor(testdf$YOB)

#traindf2 = traindf[, !(colnames(traindf) %in% c("USER_ID"))]
#train_control <- trainControl(method="cv", number=5)

cols.num = colnames(traindf2)
str(traindf2)
str(lapply(traindf2[,cols.num] , factor),list.len=ncol(df))

str(traindf2)


# Figure Out Correlation
traindf2$Party = ifelse(traindf2$Party=='Democrat', 1, -1)
str(traindf2$Party)



model_list <- caretList(
  Party ~ . , data=traindf2,
  #methodList= c('glmnet','knn','gbm','rf','svmPoly','bagFDA'),
  methodList= c('glmnet','knn','LogitBoost','rpart'),
  trControl=trainControl(savePredictions="final")
)


model1 = randomForest(Party ~ .,traindf2)

modelCor(resamples(model_list))
predictTest = predict(model_list, testdf)


pred  = apply(predictTest, 1, max)

op =cbind.data.frame(test$USER_ID,pred)
colnames(op) <-  c("USER_ID","Predictions")
head(op)
write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)



testdf2 = testdf2[, !(colnames(testdf2) %in% c("Tag.1"))]

predictTest1 = predict(model1, testdf2)
predictTest2 = predict(model2, testdf2)
predictTest3 = predict(model3, testdf2)

ensem =cbind.data.frame(predictTest1,predictTest2,predictTest3)
pred  = apply(ensem, 1, max)

library(corrplot)
corrplot(cor(DF))

# TO Remove features based on hand coded

