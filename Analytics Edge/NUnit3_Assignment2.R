# Problem 1.1 - Loading the Dataset
parole = read.csv("parole.csv")
nrow(parole)

# > nrow(parole)
# [1] 675

# Problem 1.2 - Loading the Dataset
sum(parole$violator)

# > sum(parole$violator)
# [1] 78

# Problem 2.1 - Preparing the Dataset
str(parole)

# Problem 2.2 - Preparing the Dataset
parole$crime = as.factor(parole$crime)
parole$state = as.factor(parole$state)
str(parole)

# Problem 3.1 - Splitting into a Training and Testing Set
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# Problem 3.2 - Splitting into a Training and Testing Set

# Problem 4.1 - Building a Logistic Regression Model
model1 = glm(violator ~ . , data=train, family=binomial)
summary(model1)

# Problem 4.2 - Building a Logistic Regression Model
#  Our model predicts that a parolee who committed multiple offenses has 5.01 times higher odds of being a violator than a parolee who did not commit multiple offenses but is otherwise identical. Our model predicts that a parolee who committed multiple offenses has 5.01 times higher odds of being a violator than a parolee who did not commit multiple offenses but is otherwise identical. - correct

# Problem 4.3 - Building a Logistic Regression Model
test = rbind(test,c(1,1,50,1,3,12,0,2,0))
odd =-4.2411574 +  0.3869904 +  0.8867192 -0.0001756 * 50  + 3 *-0.1238867 + 
  0.0802954 * 12 + 1 * 0.6837143
oddrato = exp(odd)
oddrato
# -1.700629
# 0.1825687

# Problem 5.1 - Evaluating the Model on the Testing Set
predictTest = predict(model1,test,type="response")
max(predictTest)

# Problem 5.2 - Evaluating the Model on the Testing Set
cm  =table(test$violator,predictTest>=0.5)
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


# Problem 5.3 - Evaluating the Model on the Testing Set
table(test$violator)
180/(180+23)

# Problem 5.4 - Evaluating the Model on the Testing Set
# The board assigns more cost to a false negative than a false positive, and should therefore use a logistic regression cutoff less than 0.5. The board assigns more cost to a false negative than a false positive, and should therefore use a logistic regression cutoff less than 0.5. - correct

# Problem 5.5 - Evaluating the Model on the Testing Set
# The model is likely of value to the board, and using a different logistic regression cutoff is likely to improve the model's value.
library(ROCR)
ROCRpred = prediction(predictTest, test$violator)
as.numeric(performance(ROCRpred,"auc")@y.values)

# The probability the model can correctly differentiate between a randomly selected parole violator and a randomly selected parole non-violator. 

# Problem 6.1 - Identifying Bias in Observational Data

