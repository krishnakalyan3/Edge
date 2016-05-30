rm(list=ls())
library(randomForest)
library(caret)
library(e1071)
library(caTools)
library(mice)
library(caret)
library(caretEnsemble)
registerDoMC(4)
setwd("/Users/krishna/MOOC/Edge/Kaggle")

datat = read.csv("train2016.csv")
test = read.csv("test2016.csv")
levels=c("Current K-12", "High School Diploma", "Current Undergraduate","Associate's Degree", "Bachelor's Degree","Master's Degree", "Doctoral Degree")

datat$YOB[is.na(datat$YOB)] = 1980
test$YOB[is.na(test$YOB)] = 1980
datat$YOB = as.factor(datat$YOB)
test$YOB = as.factor(test$YOB)

myAlgo=c('svmLinear','svmPoly','svmRadial','svmRadialCost','svmRadialWeights','lssvmLinear','lssvmPoly','lssvmRadial')


logFile = paste("filelog5_" ,Sys.Date(),sep = "")
ModelFile = paste("filemodel5_" ,Sys.Date(),sep = "")

for (algos in Algo1){
  Tcontrol <- trainControl(method="cv", number=5)
  print(algos)
  testmodel = train(Party ~ . , trControl=Tcontrol, data=datat, method=algos)
  cat(print(testmodel), file=logFile, append=TRUE, sep = "\n")
  data = c(testmodel$method,max(testmodel$results$Accuracy))
  cat(paste(data,collapse=" "), file=ModelFile, append=TRUE, sep = "\n")
}

# String as factor
# Fix Educartion Level
# Fix Income Level
# chi-square test measure assoiciation and convert to numeric
# Relace NA questions with 0
# Figure out xgboost
# bagged FDA and GLMnet