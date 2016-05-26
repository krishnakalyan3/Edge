rm(list=ls())
library(randomForest)
library(caret)
library(e1071)
library(caTools)
library(mice)
library(caret)
setwd("/Users/krishna/MOOC/Edge/Kaggle")
data = read.csv("train2016.csv",na.strings=c("","NA"))
test = read.csv("test2016.csv",na.strings=c("","NA"))

# Try Corrlation
# cor(your.data, method = "pearson")

summary(data)
data$Tag = "Train"
test$Tag = "Test"

removecols = c("Party")
data1 = data[, !(colnames(data) %in% removecols)]
completeData = rbind(data1,test)
str(completeData)

# Impute Missing values
imputed = complete(mice(completeData),5)
dim(imputed)
# Split data to train and test
train1 <- subset(imputed, Tag== "Train")
dim(train1)
test1 <- subset(imputed, Tag == "Test")
rmTag <- "Tag"
train1 = train1[, !(names(train1) %in% rmTag)]
test1 =  test1[, !(colnames(test1) %in% rmTag)]


# Add class label to training
train1$Party = data$Party
library(caret)
library(caretEnsemble)
library(doMC)
registerDoMC(4)
Tcontrol <- trainControl(method="cv", number=5)
algorithmList <- c('xgboost')
model1 = train(Party ~ ., data = train1,trControl=Tcontrol, methodList=algorithmList)

#model2 = randomForest(Party ~ ., data = train1)
print(model1)
plot(model1)


predictTest = predict(model1,test1)
op =cbind.data.frame(test$USER_ID,predictTest)
colnames(op) <-  c("USER_ID","Predictions")
write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)



gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = 2000,
                        shrinkage = 0.1,
                        n.minobsinnode = 10)

rfGrid <- expand.grid(.mtry=c(1:30))

Tcontrol <- trainControl(method="cv", number=5)
model1 = train(Party ~ ., data = train1,trControl=Tcontrol, method="rf"
               ,tuneGrid = rfGrid)

?train
