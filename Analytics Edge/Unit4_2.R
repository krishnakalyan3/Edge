getwd()
setwd("/Users/krishna/MOOC/Edge/Data")

Claims = read.csv("ClaimsData.csv")
str(Claims)
table(Claims$bucket2009)/nrow(Claims)

library("caTools")
set.seed(88)

spl = sample.split(Claims$bucket2009,SplitRatio = 0.6)
ClaimsTrain = subset(Claims, spl==TRUE)
ClaimsTest = subset(Claims,spl==FALSE)
mean(ClaimsTrain$age)
sum(ClaimsTest$diabetes)/nrow(ClaimsTest)

## Baseline method
table(ClaimsTest$bucket2009,ClaimsTest$bucket2008)
# Accuracy
sum(diag(tbl))/nrow(ClaimsTest)
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow=TRUE,nrow=5)
PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009,ClaimsTest$bucket2008))
    * PenaltyMatrix)/nrow(ClaimsTest)
(table(ClaimsTest$bucket2009,ClaimsTest$bucket2008))* PenaltyMatrix
PenaltyMatrix

ClaimsTest$newbaseline = 1
op = table(ClaimsTest$bucket2009,ClaimsTest$newbaseline)
op
#####QuickQuestion#####
ClaimsTest$newbaseline = 1
tbl2 = table(ClaimsTest$bucket2009)
tbl2
sum(diag(tbl2))/nrow(ClaimsTest) #Accuracy
#Multiply baseline with Penalty Matrix(or vector)
as.matrix(tbl2)*c(0,2,4,6,8)
sum(as.matrix(tbl2)*c(0,2,4,6,8))/nrow(ClaimsTest)

library(rpart)
library(rpart.plot)
str(ClaimsTest)
ClaimsTree = rpart(bucket2009~ age + arthritis + alzheimers+ cancer+
                     copd+ depression+diabetes +  heart.failure+ ihd
                   +kidney +  osteoporosis +stroke + bucket2008 + reimbursement2008
                   ,data = ClaimsTrain,method="class",cp=0.00005)

prp(ClaimsTree)
predictTest = predict(ClaimsTree,ClaimsTest,type="class")
op = table(ClaimsTest$bucket2009,predictTest)
sum(diag(op))/nrow(ClaimsTest)
sum(as.matrix(op)*PenaltyMatrix)/nrow(ClaimsTest)

## Adding Penalty
ClaimsTree = rpart(bucket2009~ age + arthritis + alzheimers+ cancer+
                     copd+ depression+diabetes +  heart.failure+ ihd
                   +kidney +  osteoporosis +stroke + bucket2008 + reimbursement2008
                   ,data = ClaimsTrain,method="class",cp=0.00005
                   ,parms=list(loss=PenaltyMatrix))

predictTest = predict(ClaimsTree,ClaimsTest,type="class")
op = table(ClaimsTest$bucket2009,predictTest)
sum(diag(op))/nrow(ClaimsTest)
sum(as.matrix(op)*PenaltyMatrix)/nrow(ClaimsTest)
