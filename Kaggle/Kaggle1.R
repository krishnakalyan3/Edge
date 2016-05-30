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

# String as factor (Done)
# Fix Educartion Level (Done)
# Fix Income Level (Done)
# Convert YOB to Factor (Done)
# chi-square test measure assoiciation and convert to numeric 
# Relace NA questions with 0
# Figure out xgboost
# bagged FDA and GLMnet

#dataRaw = read.csv("train2016.csv",stringsAsFactors=FALSE,na.strings=c("","NA"))
#test = read.csv("test2016.csv",stringsAsFactors=FALSE,na.strings=c("","NA"))

dataRaw = read.csv("train2016.csv",stringsAsFactors=FALSE)
test = read.csv("test2016.csv",stringsAsFactors=FALSE)


# Fix Education Level
levelsE=c("Current K-12", "High School Diploma", "Current Undergraduate","Associate's Degree", "Bachelor's Degree","Master's Degree", "Doctoral Degree")
dataRaw$EducationLevel = ordered(dataRaw$EducationLevel, levels = levelsE)
test$EducationLevel = ordered(test$EducationLevel, levels = levelsE)

# Fix Income
levelsI = c("under $25,000","$25,001 - $50,000","$50,000 - $74,999","$75,000 - $100,000","$100,001 - $150,000","over $150,000")
dataRaw$Income = ordered(dataRaw$Income, levels= levelsI)
test$Income = ordered(test$Income, levels= levelsI)

# Fix YOB as factor
dataRaw$YOB = as.factor(dataRaw$YOB)
test$YOB = as.factor(test$YOB)
dataRaw$YOB[is.na(dataRaw$YOB)] = 1980
test$YOB[is.na(test$YOB)] = 1980

usualAlgo = c('svmLinear','svmPoly','svmRadial','svmRadialCost','svmRadialWeights','rpart')

myAlgo=c('xgbLinear','glmnet','rf','svmRadial','bagFDA','LogitBoost','PenalizedLDA','gbm')
myAlgo =c('knn','nb')

logFile = paste("filelog10_" ,Sys.Date(),sep = "")
ModelFile = paste("filemodel10_" ,Sys.Date(),sep = "")

for (algos in myAlgo){
  ptm = proc.time()
  Tcontrol = trainControl(method="cv", number=5)
  print(algos)
  testmodel = train(Party ~ . , trControl=Tcontrol, data=dataRaw, method=algos)
  cat(print(testmodel), file=logFile, append=TRUE, sep = "\n")
  time = proc.time() - ptm
  data = c(testmodel$method,max(testmodel$results$Accuracy),time)
  cat(paste(data,collapse=" "), file=ModelFile, append=TRUE, sep = "\n")
}




# Ensemble
model_list <- caretList(
  Party ~ . , data=dataRaw,
  #methodList= c('glmnet','knn','gbm','rf','svmPoly','bagFDA'),
  methodList= c('rf','glmnet','svmPoly','gbm','knn','rpart','xgbLinear'),
  trControl=trainControl(savePredictions="final")
)

modelCor(resamples(model_list))
dim(test)
predictdf = predict(model_list,test)
pred  = apply(predictdf, 1, max) 
str(test)

op =cbind.data.frame(test$USER_ID,pred)
colnames(op) <-  c("USER_ID","Predictions")
write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)

