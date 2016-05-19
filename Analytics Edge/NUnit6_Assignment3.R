getwd()
setwd("/Users/krishna/MOOC/Edge/Data")
stocks = read.csv("StocksCluster.csv")


# Problem 1.1 - Exploring the Dataset
dim(stocks)

# > dim(stocks)
# [1] 11580    12

# Problem 1.2 - Exploring the Dataset
str(stocks)
table(stocks$PositiveDec)

6324/(5256+6324)

# > 6324/(5256+6324)
# [1] 0.546114

# Problem 1.3 - Exploring the Dataset
cor(stocks)

# 0.19167279x

# Problem 1.4 - Exploring the Dataset
summary(stocks)

# April
# September

# Problem 2.1 - Initial Logistic Regression Model
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec~.,stocksTrain,family=binomial)
op =table(stocksTrain$PositiveDec,predict(StocksModel,stocksTrain,type="response")>=0.5)
(990 + 3640) /sum(op)
op

# [1] 0.5711818

# Problem 2.2 - Initial Logistic Regression Model
op =table(stocksTest$PositiveDec,predict(StocksModel,stocksTest,type="response")>=0.5)
(417 + 1553)/sum(op)

# [1] 0.5670697

# Problem 2.3 - Initial Logistic Regression Model
table(stocksTest$PositiveDec)

1897/sum(table(stocksTest$PositiveDec))
#[1] 0.5460564

# Problem 3.1 - Clustering Stocks
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#  Needing to know the dependent variable value to assign an observation to a cluster defeats the purpose of the methodology Needing to know the dependent variable value to assign an observation to a cluster defeats the purpose of the methodology - correct

# Problem 3.2 - Clustering Stocks
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

summary(normTrain)
summary(normTest)

# Problem 3.3 - Clustering Stocks
#  The distribution of the ReturnJan variable is different in the training and testing set 

# Problem 3.4 - Clustering Stocks
set.seed(144)
km = kmeans(normTrain,centers = 3)
table(km$cluster)

# Problem 3.5 - Clustering Stocks
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

# 1    2    3 
# 1298 2080   96 

# Problem 4.1 - Cluster-Specific Predictions

stocksTrain1 =subset(stocksTrain,km$cluster==1)
stocksTrain2 =subset(stocksTrain,km$cluster==2)
stocksTrain3 =subset(stocksTrain,km$cluster==3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)
stocksTest1 =subset(stocksTest,clusterTest==1)
stocksTest2 =subset(stocksTest,clusterTest==2)
stocksTest3 =subset(stocksTest,clusterTest==3)

# Problem 4.2 - Cluster-Specific Predictions
StocksModel1 = glm(PositiveDec ~ . , data = stocksTrain1 , family = "binomial")
StocksModel2 = glm(PositiveDec ~ . , data = stocksTrain2 , family = "binomial")
StocksModel3 = glm(PositiveDec ~ . , data = stocksTrain3 , family = "binomial")


summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)


# Problem 4.3 - Cluster-Specific Predictions
PredictTest1 = predict(StocksModel1,stocksTest1,type="response")
PredictTest2 = predict(StocksModel2,stocksTest2,type="response")
PredictTest3 = predict(StocksModel3,stocksTest3,type="response")

cm1 =table(stocksTest1$PositiveDec,PredictTest1>0.5)
cm1 =table(stocksTest2$PositiveDec,PredictTest2>0.5)
cm1 =table(stocksTest3$PositiveDec,PredictTest3>0.5)



TN = cm1[1,1]
TP = cm1[2,2]
FN = cm1[2,1]
FP = cm1[1,2]
Acc = (TP + TN)/sum(cm1) 
Acc

# Problem 4.4 - Cluster-Specific Predictions
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
cm1 = table(AllOutcomes,AllPredictions> 0.5)

TN = cm1[1,1]
TP = cm1[2,2]
FN = cm1[2,1]
FP = cm1[1,2]
Acc = (TP + TN)/sum(cm1) 
Acc
