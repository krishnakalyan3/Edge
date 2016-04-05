getwd()
setwd("/Users/krishna/MOOC/Edge/Data")
boston = read.csv("boston.csv")
str(boston)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],
       col="blue",pch=19)
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],
       col="red",pch=19)
summary(boston$NOX)
points(boston$LON[boston$NOX>=0.55],boston$LAT[boston$NOX>=0.55],
       col="green",pch=19)
summary(boston$MEDV)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],
       col="red",pch=19)
plot(boston$LAT,boston$MEDV)
plot(boston$LON,boston$MEDV)
latlonlm = lm(MEDV ~ LAT + LON , boston)
summary(latlonlm)
points(boston$LON[latlonlm$fitted.values>=21.2],boston$LAT[latlonlm$fitted.values>21.2],col="blue",pch='$')
library(rpart)
library(rpart.plot)
latlontree = rpart(MEDV ~ LAT + LON , boston)
prp(latlontree)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],
       col="red",pch=19)

fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues>=21.2],boston$LAT[fittedvalues>21.2],col="blue",pch='$')

latlontree = rpart(MEDV ~ LAT + LON , boston,minbucket =50)
plot(latlontree)
text(latlontree)

plot(boston$LON,boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],
       col="red",pch=19)

library(caTools)
set.seed(123)
split =sample.split(boston$MEDV,SplitRatio = 0.7)
train = subset(boston,split==T)
test = subset(boston,split==F)
str(train)
linreg = lm(MEDV ~ LAT + LON + CRIM + ZN +INDUS + CHAS + NOX + RM + AGE + DIS +RAD + TAX +PTRATIO,train)
summary(linreg)
linreg.pred = predict(linreg,test)
linreg.sse = sum((linreg.pred - test$MEDV )^2)
linreg.sse
tree = rpart(MEDV ~ LAT + LON + CRIM + ZN +INDUS + CHAS + NOX + RM + AGE + DIS +RAD + TAX +PTRATIO,train)
prp(tree)
tree.pred = predict(tree,test)
tree.sse = sum((tree.pred - test$MEDV )^2)
tree.sse

library(caret)
install.packages("Rcpp")
install.packages("MatrixModels", dependencies = c("Depends"))
library(e1071)
tr.contrl = trainControl(method ="cv", number =10)
cp.grid = expand.grid(.cp = (1:10)*0.001)
tr = train(MEDV ~ LAT + LON + CRIM + ZN +
             INDUS + CHAS + NOX + RM + AGE + DIS +RAD
           + TAX +PTRATIO, data = train, 
           method ="rpart", trControl = tr.contrl, tuneGrid = cp.grid)

best.tree = tr$finalModel
prp(best.tree)
best.tree.pred = predict(best.tree, newdata = test)
best.tree.sse = sum((best.tree.pred-test$MEDV)^2)
best.tree.sse
