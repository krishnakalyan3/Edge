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

# Load Data
data = read.csv("train2016.csv",na.strings=c("","NA"))
test = read.csv("test2016.csv",na.strings=c("","NA"))


data$Tag = "Train"
test$Tag = "Test"
removecols = c("Party")
data1 = data[, !(colnames(data) %in% removecols)]
completeData = rbind(data1,test)
str(completeData)

# Impute Missing values
imputed = complete(mice(completeData),5)
dim(imputed)

# Split data to train and test
train1 <- subset(imputed, Tag== "Train")
dim(train1)
test1 <- subset(imputed, Tag == "Test")
rmTag <- "Tag"
train1 = train1[, !(names(train1) %in% rmTag)]
test1 =  test1[, !(colnames(test1) %in% rmTag)]
train1$Party = data$Party

usualAlgo = c('rf','glmnet','gbm','glm','glmnet','neuralnet','nnet','LogitBoost','PenalizedLDA','svmLinear','svmPoly','svmRadial','svmRadialCost','svmRadialWeights','rpart')
myAlgo=c('svmLinear','svmPoly','svmRadial','svmRadialCost','svmRadialWeights','lssvmLinear','lssvmPoly','lssvmRadial')

Algo1=c('treebag','bagFDA','logicBag','bagEarth','bag','bayesglm','brnn','ada','gamboost','glmboost','bstLs')
Algo2=c('LogitBoost','blackboost','bstTree','J48','C5.0','rpart','rpart2','cforest','ctree','ctree2','C5.0Cost')
Algo3=c('rpartCost','cubist','enet','elm','RFlda','fda','gaussprLinear','gaussprPoly','gaussprRadial','gamLoess','gam')
Algo4=c('gamSpline','glm','glmStepAIC','gpls','glmnet','protoclass','hda','hdda','icr','kknn','knn','lvq','lars')
Algo5=c('lars2','lssvmLinear','lssvmPoly','lssvmRadial','lda','lda2','stepLDA','lm','leapBackward','leapForward')
Algo6=c('leapSeq','lmStepAIC','logreg','LMT','Mlda','mda','avNNet','M5Rules','M5','mlp','mlpWeightDecay','earth') 
Algo7=c('gcvEarth','nb','pam','neuralnet','nnet','pcaNNet','ORFlog','ORFpls','ORFridge','ORFsvm','oblique.tree','parRF')
Algo8=c('partDSA','kernelpls','pls','simpls','widekernelpls','pda','pda2','PenalizedLDA','penalized','plr','multinom')
Algo9=c('krlsPoly','pcr','ppr','qda','stepQDA','qrf','qrnn','krlsRadial','rbf','rbfDDA','rFerns','rf','extraTrees')
Algo10=c('Boruta','rknn','rknnBel','rda','RRF','RRFglobal','relaxo','rvmLinear','rvmPoly','rvmRadial','ridge')
Algo11=c('foba','Linda','rlm','QdaCov','rrlda','RSimca','rocc','JRip','PART','bdk','xyf','sda','CSimca','C5.0Rules')
Algo12=c('C5.0Tree','OneR','sparseLDA','smda','spls','slda','dnn','sddaQDA','gbm','superpc','svmRadialWeights')
Algo13=c('svmLinear','svmPoly','svmRadial','svmRadialCost','lasso','evtree','nodeHarvest','vbmpRadial')

logFile = paste("filelog7_" ,Sys.Date(),sep = "")
ModelFile = paste("filemodel7_" ,Sys.Date(),sep = "")


for (algos in usualAlgo){
  Tcontrol <- trainControl(method="cv", number=5)
  print(algos)
  testmodel = train(Party ~ . - USER_ID , trControl=Tcontrol, data = train1, method=algos)
  cat(print(testmodel), file=logFile, append=TRUE, sep = "\n")
  data = c(testmodel$method,max(testmodel$results$Accuracy))
  cat(paste(data,collapse=" "), file=ModelFile, append=TRUE, sep = "\n")
}

gAlgo = c('glmnet','LogitBoost','PenalizedLDA')

Tcontrol <- trainControl(method="cv", number=5,classProbs=TRUE)
model2 = train(Party ~ . - USER_ID, data = train1,trControl=Tcontrol, methodList=gAlgo)
print(model2)

(predictTest)
predictTest = predict(model2, test1)
op =cbind.data.frame(test$USER_ID,predictTest)
colnames(op) <-  c("USER_ID","Predictions")
summary(op)
head(op)
write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)

str(train1)

model_list <- caretList(
  Party ~ . , data=train1,
  trControl=Tcontrol,
  methodList= c('glmnet','LogitBoost','PenalizedLDA')
)

modelCor(resamples(model_list))
greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)

predictTest = predict(greedy_ensemble,test1)
op =cbind.data.frame(test1$USER_ID,predictTest)
colnames(op) <-  c("USER_ID","Predictions")
write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)

