getwd()
setwd("/Users/krishna/MOOC/Edge/Data")
loans = read.csv("loans.csv")
str(loans)
summary(loans)

# Problem 1.1 - Preparing the Dataset
table(loans$not.fully.paid)
1533/(sum(table(loans$not.fully.paid)))

# > 1533/(sum(table(loans$not.fully.paid)))
# [1] 0.1600543

# Problem 1.2 - Preparing the Dataset
summary(loans)

# Problem 1.3 - Preparing the Dataset
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
# From nrow(missing), we see that only 62 of 9578 loans have missing data; removing this small number of observations would not lead to overfitting. From table(missing$not.fully.paid), we see that 12 of 62 loans with missing data were not fully paid, or 19.35%. This rate is similar to the 16.01% across all loans, so the form of biasing described is not an issue. However, to predict risk for loans with missing data we need to fill in the missing values instead of removing the observations.

# Problem 1.4 - Preparing the Dataset
install.packages("mice") 
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
summary(loans)
?mice
# We predicted missing variable values using the available independent variables for each observation. 

# Problem 2.1 - Prediction Models
loans_impute = read.csv("loans_imputed.csv")
set.seed(144)
library(caTools)
split = sample.split(loans_impute$not.fully.paid, SplitRatio = 0.7)
train = subset(loans_impute, split == TRUE)
test = subset(loans_impute, split == FALSE)
model1 = lm(not.fully.paid ~. , train)
summary(model1)

# Problem 2.2 - Prediction Models
-9.317e-03 *(700 - 710)
# [1] 0.09317

exp(0.09317)

# Problem 2.3 - Prediction Models
model1 = glm(not.fully.paid ~ . , data=train, family=binomial)
predictTest = predict(model1,test,type="response")
cm  =table(test$not.fully.paid,predictTest>=0.5)
cm
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Sensitivity =  TP/(TP+FN)     #TP / Positives
Specificity =  TN/(TN + FP)   #TN / Negatives
Acc = (TP + TN)/sum(cm) 
Sensitivity
Specificity
Acc
5632/sum(table(train$not.fully.paid))
# 0.8364079
# [1] 0.8399702

# Problem 2.4 - Prediction Models
library(ROCR)
ROCRpred = prediction(predictTest, test$not.fully.paid)
as.numeric(performance(ROCRpred,"auc")@y.values)

# 0.6720995

# Problem 3.1 - A "Smart Baseline"
model2 = glm(not.fully.paid ~  int.rate , data=train, family=binomial)
summary(model2)

# Problem 3.2 - A "Smart Baseline"
predictTestb = predict(model2,test,type="response")
max(predictTestb)
cm  =table(test$not.fully.paid,predictTestb>=0.5)
cm

# 0.426624
# FALSE
# 0  2413
# 1   460 

# Problem 3.3 - A "Smart Baseline"
library(ROCR)
ROCRpred = prediction(predictTestb, test$not.fully.paid)
as.numeric(performance(ROCRpred,"auc")@y.values)

# Problem 4.1 - Computing the Profitability of an Investment
10 * exp(.06 * 3)

# 11.97217

# Problem 4.2 - Computing the Profitability of an Investment
# c * exp(rt) - c c * exp(rt) - c 

# Problem 4.3 - Computing the Profitability of an Investment
# -c

# Problem 5.1 - A Simple Investment Strategy
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(10 * test$profit)

# Problem 6.1 - An Investment Strategy Based on Risk
highrt =  subset(test , test$int.rate >= .15)  
str(highrt)

highrt$profit = exp(highrt$int.rate*3) - 1
highrt$profit[highrt$not.fully.paid == 1] = -1
mean(highrt$profit)

tb =table(highrt$not.fully.paid)
110/(sum(tb))

# 0.2251015
# 0.2517162

# Problem 6.2 - An Investment Strategy Based on Risk
highrt$predicted.risk = predict(model1,highrt,type="response")
cutoff = sort(highrt$predicted.risk, decreasing=FALSE)[100]
cutoff
selectedLoans = subset(highrt,predicted.risk <=cutoff )
nrow(selectedLoans)
nrow(highrt)
selectedLoans$profit = exp(selectedLoans$int.rate*3) - 1
selectedLoans$profit[selectedLoans$not.fully.paid == 1] = -1
sum(selectedLoans$profit)
table(selectedLoans$profit)
