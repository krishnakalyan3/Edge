getwd()
FluTrain = read.csv("FluTrain.csv")
table(FluTrain$ILI,FluTrain$Week)

# Problem 1.1
FluTrain[which.max(FluTrain$ILI),]
FluTrain[which.max(FluTrain$Queries),]

# Week      ILI Queries
# 303 2009-10-18 - 2009-10-24 7.618892       1

# Problem 1.2
hist(FluTrain$ILI)

# Plot 1.3
plot(log(FluTrain$ILI),FluTrain$Queries)
# Positive Linear Relatinship

# Problem 2.1
# log(ILI) = intercept + coefficient x Queries, where the coefficient is positive log(ILI) = intercept + coefficient x Queries, where the coefficient is positive - correct

# Problem 2.2
model1 = lm(log(ILI)~Queries, FluTrain)
summary(model1)

# Problem 3.1 - Performance on the Test Set
setwd("/Users/krishna/MOOC/Edge/Data")
FluTest = read.csv("FluTest.csv")
PredTest1 = predict(model1, newdata=FluTest)
PredTest1 = exp(predict(model1, newdata=FluTest))
?which
which( FluTest$Week  == "2012-03-11 - 2012-03-17")
PredTest1[11]
PredTest1[11]

# 11 
# 2.187378 

# Problem 3.2 - Performance on the Test Set
RE = ( FluTest$ILI[11] -PredTest1[11] )/FluTest$ILI[11]

# 11 
# 0.04623827

# Problem 3.3 - Performance on the Test Set
SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))

# [1] 0.7490645
# Problem 4.2 - Training a Time Series Model
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain$ILILag2)
plot(log(FluTrain$ILILag2),log(FluTrain$ILI))
#  There is a strong positive relationship between log(ILILag2) and log(ILI). 


# Problem 4.3 - Training a Time Series Model
FluTrend2 = lm(log(ILI)~Queries + log(ILILag2), FluTrain)
summary(FluTrend2)


# Problem 5.1 - Evaluating the Time Series Model in the Test Set
ILILag2Test = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2Test)
summary(FluTest$ILILag2)

# Problem 5.3 - Evaluating the Time Series Model in the Test Set

FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]
FluTest$ILILag2[1]
FluTest$ILILag2[2]

# > FluTest$ILILag2[1]
# [1] 1.852736
# > FluTest$ILILag2[2]
# [1] 2.12413

# Problem 5.4 - Evaluating the Time Series Model in the Test Set
PredTest2 = exp(predict(FluTrend2,FluTest))
SSE = sum((PredTest2 - FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))



