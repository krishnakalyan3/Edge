getwd()
library(randomForest)
rm(list=ls())

library(caret)
library(e1071)
install.packages("VIM")
setwd("/Users/krishna/MOOC/Edge/Kaggle")
train1 = read.csv("train2016.csv")
test = read.csv("test2016.csv")

traincol = c("YOB","Gender","Income","HouseholdStatus","EducationLevel")
trainY = train1[traincol]
testY = test[traincol]

dataY = rbind(trainY,testY)
dim(dataY)
dataYtrain = na.omit(dataY)

modelYOB = randomForest(as.factor(YOB) ~ ., dataYtrain)
predictYOBtrain = predict(modelYOB, train1)
predictYOBtest = predict(modelYOB, test)

train1$predYOB = predictYOBtrain
test$predYOB = predictYOBtest

train1$YOB[is.na(trainY$YOB)] = train1$predYOB[is.na(trainY$YOB)]
test$YOB[is.na(test$YOB)] = test$predYOB[is.na(test$YOB)]

summary(train1)

removecols = c("predYOB")
train1 = train1[, !(colnames(train1) %in% removecols)]
test = test[, !(colnames(test) %in% removecols)]


# Fix YOB
train1$YOB[is.na(train1$YOB)] = 1980
test$YOB[is.na(test$YOB)] = 1980


#YOB 
# Mean 1980
# Median 1983

#train1[,7:100] =sapply(train1[,7:100], as.numeric)
#train1$YOB = as.factor(train1$YOB)
#test$YOB = as.factor(test$YOB)
#train1 = train1[, !(colnames(train1) %in% c("USER_ID"))]


# Data Pre=Processig
#library(corrplot)
#summary(train1)
#str(train1)
# Divide into train , validation , test set
set.seed(144)
library(caTools)
split = sample.split(train1$Party, SplitRatio = 0.7)
train = subset(train1, split == TRUE)
validation = subset(train1, split == FALSE)

#str(train)
#summary(train)

#dim(train)
#dim(validation)

# NAs in my dataset
sum(is.na(test))

#md.pattern(train)
summary(train)
alldata = rbind(train,validation)

train_control <- trainControl(method="cv", number=5)
model1 = train(Party ~ ., alldata,trControl=train_control, method="rf")
print(model1)
#summary(model1)
#plot(model1, log="y")
#varImpPlot(model1)
# Accuracy and F1 Score
predictval = predict(model1, validation)
cm = table(validation$Party,predictval)
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Acc = (TP + TN)/sum(cm) 
Acc

#test1 = test[, !(colnames(test) %in% c("USER_ID"))]
# Writing Output
predictTest = predict(model1, test)
op =cbind.data.frame(test$USER_ID,predictTest)
colnames(op) <-  c("USER_ID","Predictions")
summary(op)
head(op)
write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)
?write.table



# Models Tried
model1 <- randomForest(Party ~ . -USER_ID , data = trainimp,  keep.forest=TRUE,
                       importance=TRUE)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:15))
metric <- "Accuracy"
rf_gridsearch <- train(Party~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

