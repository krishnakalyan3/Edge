rm(list=ls())
library(randomForest)
library(caret)
library(e1071)
library(caTools)
library(mice)
library(caret)
setwd("/Users/krishna/MOOC/Edge/Kaggle")
data = read.csv("train2016.csv",na.strings=c("","NA"))
test = read.csv("test2016.csv",na.strings=c("","NA"))

# Try Corrlation
str(train1)
cor(train1[,7:107], method = "pearson")



summary(data)
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


# Add class label to training
train1$Party = data$Party
library(caret)
library(caretEnsemble)
library(doMC)
registerDoMC(4)
Tcontrol <- trainControl(method="cv", number=5)
algorithmList <- c('xgboost','rf','nnet','svm','rpart','glm','gbm','ridge')
model1 = train(Party ~ . , data = train1, methodList='gbm')

#model2 = randomForest(Party ~ ., data = train1)
print(model1)
plot(model1)
max(model1$results$Accuracy)

algolistbig = c('treebag','bagFDA','logicBag','bagEarth','bag','bayesglm','brnn','ada','gamboost','glmboost','bstLs','LogitBoost','bstS','blackboost','bstTree','J48','C5.0','rpart','rpart2','cforest','ctree','ctree2','C5.0Cost','rpartCost','cubist','enet','elm','RFlda','fda','gaussprLinear','gaussprPoly','gaussprRadial','gamLoess','gam','gamSpline','glm','glmStepAIC','gpls','glmnet','protoclass','hda','hdda','icr','kknn','knn','lvq','lars','lars2','lssvmLinear','lssvmPoly','lssvmRadial','lda','lda2','stepLDA','lm','leapBackward','leapForward','leapSeq','lmStepAIC','logreg','LMT','Mlda','mda','avNNet','M5Rules','M5','mlp','mlpWeightDecay','earth','gcvEarth','nb','pam','neuralnet','nnet','pcaNNet','ORFlog','ORFpls','ORFridge','ORFsvm','oblique.tree','parRF','partDSA','kernelpls','pls','simpls','widekernelpls','pda','pda2','PenalizedLDA','penalized','plr','multinom','krlsPoly','pcr','ppr','qda','stepQDA','qrf','qrnn','krlsRadial','rbf','rbfDDA','rFerns','rf','extraTrees','Boruta','rknn','rknnBel','rda','RRF','RRFglobal','relaxo','rvmLinear','rvmPoly','rvmRadial','ridge','foba','Linda','rlm','QdaCov','rrlda','RSimca','rocc','JRip','PART','bdk','xyf','sda','CSimca','C5.0Rules','C5.0Tree','OneR','sparseLDA','smda','spls','slda','dnn','sddaQDA','gbm','superpc','svmRadialWeights','svmLinear','svmPoly','svmRadial','svmRadialCost','lasso','evtree','nodeHarvest','vbmpRadial')
algorithmListT=c('svm','glmnet','rf','gbm')

logFile = paste("filelog" ,Sys.Date(),sep = "")
ModelFile = paste("filemodel" ,Sys.Date(),sep = "")

for (algos in algorithmListT){
  Tcontrol <- trainControl(method="cv", number=5)
  model1 = train(Party ~ . , trControl=Tcontrol, data = train1, methodList=algos)
  cat(print(model1), file=logFile, append=TRUE, sep = "\n")
  data = c(model1$method,max(model1$results$Accuracy))
  cat(paste(data,collapse=" "), file=ModelFile, append=TRUE, sep = "\n")
}

predictTest = predict(model1,test1)
op =cbind.data.frame(test$USER_ID,predictTest)
colnames(op) <-  c("USER_ID","Predictions")
write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)


