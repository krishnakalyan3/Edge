data(state)
statedata = data.frame(state.x77)
str(statedata)
attach(statedata)
model1 = lm(Life.Exp ~ Population+Income+Illiteracy+Murder + HS.Grad + Frost+
              Area)
summary(model1)
pred = predict(model1,statedata)
sse = sum((pred-statedata$Life.Exp)^2)
sse


model2 = lm( Life.Exp ~Population +Murder+ Frost+ HS.Grad )
summary(model2)
sse2 = sum(model2$residuals^2)
sse2
# Cart Model
model3 = rpart(Life.Exp  ~ Population+Income+ Illiteracy+ Murder+
                 HS.Grad+ Frost+ Area)
prp(model3)
pre3 =predict(model3,statedata)
sse3 = sum((pre3-statedata$Life.Exp)^2)
sse3

model4 = model3 = rpart(Life.Exp  ~ ., statedata,minbucket = 5)
prp(model4)
# Larger why? 2.4
pre4 =predict(model4,statedata)
sse4 = sum((pre4-statedata$Life.Exp)^2)
sse4

model5 = rpart(Life.Exp ~ Area,minbucket =1)
prp(model5)
sse5 = sum((predict(model5,statedata)-statedata$Life.Exp)^2)
sse5

library(caret)
library(e1071)
set.seed(111)
cartGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01) )
cp.grid
tr.contrl = trainControl(method ="cv", number =10)
tr = train(Life.Exp ~ ., statedata,
           method ="rpart", trControl = tr.contrl, tuneGrid = cartGrid)
best.tree = tr$finalModel
best.tree

# 3.2
model6 = model3 = rpart(Life.Exp  ~ ., statedata,minbucket = 5,cp = 0.12)
prp(model6)
model6
sse6 = sum((predict(model6,statedata)-statedata$Life.Exp)^2)
sse6

set.seed(111)
train(Life.Exp ~ Area, data=statedata, method="rpart", trControl = tr.contrl, tuneGrid = cartGrid )
#  cp = 0.02
model7 = rpart(Life.Exp ~ Area, data=statedata, cp=0.02)
model7
prp(model7)
sse7 = sum((predict(model7,statedata)-statedata$Life.Exp)^2)
sse7

set.seed(111)
CARTmodel5 = rpart(Life.Exp ~ Area, data=statedata, cp=0.02)
prp(CARTmodel5)
