getwd()
setwd("/Users/krishna/MOOC/Edge/Data")
wine = read.csv("wine.csv")
str(wine)
summary(wine)
model1 =lm(Price ~ AGST,wine)
summary(model1)
model1$residuals
SSE = sum(model1$residuals^2)
SSE

# Adjusted R square goes down if the independent variable added
# Does not contribute to the model
model2 =lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)

# Inc in Adj R sq .. Better features

SSE = sum(model2$residuals^2)
SSE
model3 = lm(Price ~ AGST + HarvestRain + WinterRain+ Age + FrancePop 
            ,data = wine)
summary(model3)
SSE =sum(model3$residuals^2)
SSE

model4 = lm(Price ~ HarvestRain + WinterRain,wine)
summary(model4)

model4 = lm(Price ~ AGST+HarvestRain + WinterRain+Age,wine)
summary(model4)

## Computing Corr
cor(wine$WinterRain,wine$Price)
cor(wine$Age,wine$FrancePop)
cor(wine)
cor(wine$HarvestRain,wine$WinterRain)

# Correlation b/w Indepent and dependent variable is a good thing
# Multi Colinearity - when 2 independent varibales are correlated - bad
# remove the least significant highly correlated variable

model5 = lm(Price ~ AGST+HarvestRain + WinterRain,wine)
summary(model5)

# Test - Out of sample accuracy

wineTest = read.csv("wine_test.csv")
str(wineTest)
predictTest=predict(model4,newdata=wineTest)
predictTest
SSE = sum((wineTest$Price -predictTest)^2)
SST = sum((wineTest$Price -mean(wine$Price))^2)
1 - SSE/SST
