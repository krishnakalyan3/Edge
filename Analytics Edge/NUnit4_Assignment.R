getwd()
setwd("/Users/krishna/MOOC/Edge/Data")
gerber = read.csv("gerber.csv")
str(gerber)

# Problem 1.1 - Exploration and Logistic Regression
sum(gerber$voting)/nrow(gerber)

# > sum(gerber$voting)/nrow(gerber)
# [1] 0.3158996

# Problem 1.2 - Exploration and Logistic Regression
table(gerber$civicduty,gerber$voting)
table(gerber$hawthorne,gerber$voting)
table(gerber$self,gerber$voting)
table(gerber$neighbors,gerber$voting)
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

# Problem 1.3 - Exploration and Logistic Regression
model1 = glm(voting ~ hawthorne + civicduty + neighbors + self, gerber, family=binomial() )
summary(model1)

# Problem 1.4 - Exploration and Logistic Regression
trainPredict = predict(model1 , type="response")
op  = table(gerber$voting,trainPredict >0.3)


# FALSE   TRUE
# 0 134513 100875
# 1  56730  51966

# Problem 1.5 - Exploration and Logistic Regression
op2 = table(gerber$voting,trainPredict >0.5)
op2
235388/sum(op2)

# [1] 0.6841004
# > table(gerber$voting)

# Problem 1.6 - Exploration and Logistic Regression
op3  = table(gerber$voting)
235388/sum(op3)

# > 235388/sum(op3)
# [1] 0.6841004

library(ROCR)
ROCRpred = prediction(trainPredict, gerber$voting)
as.numeric(performance(ROCRpred,"auc")@y.values)

# > as.numeric(performance(ROCRpred,"auc")@y.values)
# [1] 0.5308461

#  Even though all of the variables are significant, this is a weak predictive model. Even though all of the variables are significant, this is a weak predictive model. - correct

# Problem 2.1 - Trees
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
rpart.plot(CARTmodel)

# Problem 2.2 - Trees
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
rpart.plot(CARTmodel2)

#  Neighbor is the first split, civic duty is the last.

# Problem 2.3 - Trees

# 0.31

# Problem 2.4 - Trees
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
rpart.plot(CARTmodel3)

# Men

# Problem 3.1 - Interaction Terms
CARTmodel4 = rpart(voting ~  control, data=gerber, cp=0.0)
CARTmodel5 = rpart(voting ~  control + sex, data=gerber, cp=0.0)
rpart.plot(CARTmodel4)
prp(CARTmodel4,digits = 6)
rpart.plot(CARTmodel5)

abs(0.296638 - 0.34)

# > abs(0.296638 - 0.34)
# [1] 0.043362

# Problem 3.2 - Interaction Terms
rpart.plot(CARTmodel5)

# Problem 3.3 - Interaction Terms
model6 = glm(voting ~ control + sex, gerber, family=binomial() )
summary(model6)

# Coefficient is negative, reflecting that women are less likely to vote 

