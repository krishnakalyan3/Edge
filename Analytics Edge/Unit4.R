getwd()
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
install.packages("randomForest")
stevenForest = randomForest(Reverse ~Circuit + Issue + 
                               Petitioner + Respondent + LowerCourt + Unconst,
                             Train )
