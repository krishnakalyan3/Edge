install.packages("caTools")
library("caTools")
# Quiz
op = -1.5 + 3 + 5 * -0.5
op
k = exp(op)
x = 1/ (1+exp(-op))
x

getwd()
setwd("/Users/krishna/MOOC/Edge/Data/")
quality = read.csv("quality.csv")
str(quality)
table(quality$PoorCare)

# Base Line
98/131 
# 0.74 is our accuracy
set.seed(88)
split = sample.split(quality$PoorCare,SplitRatio = 0.75)

qualityTrain = subset(quality,split == TRUE)
qualityTest = subset(quality,split == FALSE)
nrow(qualityTest)
nrow(qualityTrain)

QualityLog = glm(PoorCare ~OfficeVisits + Narcotics , qualityTrain,family=binomial())
summary(QualityLog)

predictTrain = predict(QualityLog,type="response")
summary(predictTrain)

tapply(predictTrain,qualityTrain$PoorCare,mean)
?tapply

#Quick Question

model1 =glm(PoorCare ~StartedOnCombination + ProviderCount ,
            qualityTrain,family=binomial())
summary(model1)
table(qualityTrain$PoorCare,predictTrain >0.2)

# 0.2
54/(20+54)
16/(9+16)

# 0.7
8/25
73/74

# 0.5
10/(10+15)
70/(70 +4)

## Quick Quiz
##
20/25
15/25
##
15/25
20/25

## ROC Curve in R
install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(predictTrain,qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize = T, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))


# Quick Question
QualityLog = glm(PoorCare ~OfficeVisits + Narcotics , qualityTrain,family=binomial())
summary(QualityLog)
predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

framingham = read.csv("framingham.csv")
str(framingham)
library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD,SplitRatio = 0.65)
train = subset(framingham,split==T)
test = subset(framingham,split==F)

framinghamLog =glm(TenYearCHD~. , data=train,family = binomial() )
summary(framinghamLog)
predictTest = predict(framinghamLog,type="response",newdata=test)
table(test$TenYearCHD,predictTest>.5)

# Accuracy
(1069+11)/(1069+11+6+187)

# Baseline
(1069+8)/(1069+11+6+187)

# AUC
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred,"auc")@y.values)

#True positive Rate  or Recall   <-    Sensitivity =  TP/(TP+FN)
#True negative Rate  or Preision <-    Specificity =  TN/(TN + FP)

#Sensitivity 
# TP / P 
11/(11+187)

# Specificity
# TN / P
1069/(1069+6)

## Recitation
polling =read.csv("PollingData.csv")
str(polling)
table(polling$Year)
summary(polling)

# Check out the NAs
install.packages("mice")
library(mice)
simple = polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)

# Complex baseline
train = subset(polling, Year ==2004 | Year ==2008)
test = subset(polling , Year ==2012)
table(train$Republican)
table(sign(train$Rasmussen))
table(train$Republican,sign(train$Rasmussen))

cor(train)
str(train)
cor(train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])
# Ras and SurUSA have high corr count

model1 = glm(Republican ~ PropR,train,family=binomial())
summary(model1)

# AIC 19.8
pred1 = predict(model1,type="response")
table(train$Republican,pred1>=0.5)

model2 = glm(Republican ~SurveyUSA +DiffCount , data = train, family="binomial")
pred2 = predict(model2,type="response")
summary(model2)
table(train$Republican,pred2>=0.5)

table(test$Republican, sign(test$Rasmussen))
testpred = predict(model2,newdata=test,type="response")
table(test$Republican,testpred>=0.5)
subset(test,testpred>=0.5 & Republican ==0)
