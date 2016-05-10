census = read.csv("census.csv")
str(census)

# Problem 1.1 - A Logistic Regression Model
set.seed(2000)
spl = sample.split(census$over50k,SplitRatio = .6)
train = subset(census, spl == TRUE)
test = subset(census, spl == FALSE)
model10 = glm(over50k ~ . , data = train, family = "binomial")
summary(model10)

# Problem 1.2 - A Logistic Regression Model
predTest = predict(fitGLM, test )
acc =table(test$over50k,predTest>0.5)
acc
sum(diag(acc))/ sum(acc)

# > sum(diag(acc))/ sum(acc)
# [1] 0.8487218

# Problem 1.3 - A Logistic Regression Model
base = table(test$over50k)
9713/ sum(base)

# Problem 1.4 - A Logistic Regression Model
library(ROCR)
ROCRpred = prediction(predTest, test$over50k)
as.numeric(performance(ROCRpred,"auc")@y.values)

#[1] 0.9077589

# Problem 2.1 - A CART Model
library(rpart.plot)
model11 = rpart(over50k ~ ., train , method="class")
prp(model11)

# 4

# Problem 2.4 - A CART Model
testPred = predict(model11, test)
cm  =table(test$over50k,testPred)
sum(diag(cm))/sum(cm)


t = prediction(testPred[,2], test$over50k)
perf = performance(t, "tpr", "fpr") 
plot(perf) plot(perf, colorize = TRUE) 
plot(perf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.2,1.7))

#The probabilities from the CART model take only a handful of values (five, one for each end bucket/leaf of the tree); the changes in the ROC curve correspond to setting the threshold to one of those values. The probabilities from the CART model take only a handful of values (five, one for each end bucket/leaf of the tree); the changes in the ROC curve correspond to setting the threshold to one of those values. - correct

# Problem 2.6 - A CART Model
as.numeric(performance(t,"auc")@y.values)

# > as.numeric(performance(t,"auc")@y.values)
# [1] 0.8443179

# Problem 3.1 - A Random Forest Model
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
set.seed(1)
model12 = randomForest(over50k ~ ., trainSmall)
predTestR = predict(model12, test)
cm  =table(test$over50k,predTestR)
sum(diag(cm))/sum(cm)

# > sum(diag(cm))/sum(cm)
# [1] 0.8446564

# Problem 3.2 - A Random Forest Model
vu = varUsed(model12, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(model12$forest$xlevels[vusorted$ix]))

# Age

# Problem 3.3 - A Random Forest Model
varImpPlot(model12)
# occupation

# Problem 4.1 - Selecting cp by Cross-Validation
library(caret)
set.seed(2)
fitControl = trainControl(method ="cv",number =10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
cp =train(over50k~., data=train, method="rpart", 
      trControl=fitControl, tuneGrid=cartGrid)

# 0.002

# Problem 4.2 - Selecting cp by Cross-Validation
model13 = rpart(over50k ~ ., train , parms =0.002, method="class")





