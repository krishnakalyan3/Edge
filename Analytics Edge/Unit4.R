getwd()
install.packages("caTools")

setwd("/Users/krishna/MOOC/Edge/Data/")


stevens = read.csv("stevens.csv")
str(stevens)

library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse,SplitRatio = .7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

stevensTree = rpart(Reverse ~Circuit + Issue + 
                    Petitioner + Respondent + LowerCourt + Unconst,
                    Train,method = "class",minbucket=100)
prp(stevensTree)
stevensTree

predictCART =predict(stevensTree,Test,type="class")
table(Test$Reverse,predictCART)
(41+71)/(41+36+22+71)
library(ROCR)
predictROC = predict(stevensTree,newdata=Test)
pred=prediction(predictROC[,2],Test$Reverse)
pref=performance(pred,"tpr","fpr")
plot(pref)

as.numeric(performance(pred, "auc")@y.values)

## Random Forest
ip <- installed.packages()
pkgs.to.remove <- ip[!(ip[,"Priority"] %in% c("base", "recommended")), 1]
sapply(pkgs.to.remove, remove.packages)

install.packages("randomForest")
library(randomForest)

stevenForest = randomForest(as.factor(Reverse) ~Circuit + Issue + 
                              Petitioner + Respondent 
                            + LowerCourt + Unconst,
                            Train, nodesize = 25, ntree = 200 )
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)


set.seed(200)
stevenForest = randomForest(Reverse ~Circuit + Issue + 
                              Petitioner + Respondent 
                            + LowerCourt + Unconst,
                            Train, nodesize = 25, ntree = 200 )

predictForest = predict(stevenForest,newdata = Test)
table(Test$Reverse,predictForest)
(44 +76)/(44+33 + 17 + 76)
?randomForest
install.packages("e1071")
library(caret)
library(e1071)
numfolds = trainControl(method="cv",number=10)


numFolds = trainControl(method = "cv", number = 10) #cv for cross validation, and 10 folds
cpGrid = expand.grid(.cp = seq(0.01,0.5, 0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
      data=Train, method="rpart" , trControl = numFolds, tuneGrid= cpGrid)

StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                      data=Train, method="class", cp=0.19)
PredictCV = predict(StevensTreeCV, newdata = Test, type="class")
cm = table(Test$Reverse,PredictCV)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy


