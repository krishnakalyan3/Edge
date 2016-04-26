pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

# Problem 1.1 - Dataset size
dim(pisaTrain)[1]
# [1] 3663

# Problem 1.2 - Summarizing the dataset
tapply(pisaTest$readingScore,pisaTest$male,mean)
# 0        1 
# 509.2992 485.3103 

# Problem 1.3 - Locating missing values
summary(pisaTrain)
colnames(pisaTrain)[colSums(is.na(pisaTrain)) > 0]

# Problem 1.4 - Removing missing values
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
dim(pisaTrain)[1]
dim(pisaTest)[1]

# Problem 2.1 - Factor variables
summary(pisaTrain)


# Problem 3.1 - Building a model
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

summary(pisaTrain)
lmScore = lm(readingScore ~ .,pisaTrain)
summary(lmScore)

# Problem 3.2
SSE = sum((predict(lmScore,pisaTrain) - pisaTrain$readingScore)^2)
RMSE = sqrt(SSE/nrow(pisaTrain))

SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE / nrow(pisaTrain))
# [1] 73.36555

# Problem 3.3 - Comparing predictions for similar students
# 59.09 59.09 - correct

# Problem 3.4 - Interpreting model coefficients
# Predicted difference in the reading score between an Asian student and a white student who is otherwise identical Predicted difference in the reading score between an Asian student and a white student who is otherwise identical - correct

# Problem 3.5 - Identifying variables lacking statistical significance
# all significant variables

# Problem 4.1 - Predicting on unseen data
prediction = predict(lmScore,pisaTest)
summary(prediction)[1] - summary(prediction)[6] 

# Problem 4.2 - Test set SSE and RMSE
SSE = sum((predict(lmScore,pisaTest) - pisaTest$readingScore)^2)
# 5762082
RMSE = sqrt(SSE / nrow(pisaTest))
RMSE
# [1] 76.29079

# Problem 4.3 - Baseline prediction and test-set SSE
baseline = mean(pisaTrain$readingScore)
SST = sum((baseline-pisaTest$readingScore)^2)
# [1] 7802354
R2 = 1 - SSE/SST
# [1] 0.2614944