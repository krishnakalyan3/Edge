getwd()
setwd("/Users/krishna/MOOC/Edge/Data")

climate = read.csv("climate_change.csv")
str(climate)
train = subset(climate ,  Year <= 2006 )
test = subset(climate ,  Year > 2006 )

# Problem 1.1 - Creating Our First Model
# 
# (2 points possible)
# We are interested in how changes in these variables affect future temperatures, as well as how well these variables explain temperature changes so far. To do this, first read the dataset climate_change.csv into R.
# 
# Then, split the data into a training set, consisting of all the observations up to and including 2006, and a testing set consisting of the remaining years (hint: use subset). A training set refers to the data that will be used to build the model (this is the data we give to the lm() function), and a testing set refers to the data we will use to test our predictive ability.
# 
# Next, build a linear regression model to predict the dependent variable Temp, using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, and Aerosols as independent variables (Year and Month should NOT be used in the model). Use the training set to build the model.
# 
# Enter the model R2 (the "Multiple R-squared" value):
  
  
model1 = lm(Temp ~ MEI+ CO2+ CH4+ N2O+ CFC.11+ CFC.12+ TSI+ Aerosols,train)
summary(model1)

# 
# Problem 2.2 - Understanding the Model
# 
# (2/2 points)
# Compute the correlations between all the variables in the training set. Which of the following independent variables is N2O highly correlated with (absolute correlation greater than 0.7)? Select all that apply.
cor(train)

# 
# Problem 3 - Simplifying the Model
# 
# (2 points possible)
# Given that the correlations are so high, let us focus on the N2O variable and build a model with only MEI, TSI, Aerosols and N2O as independent variables. Remember to use the training set to build the model.
# 
# Enter the coefficient of N2O in this reduced model:

model2 = lm(Temp ~ MEI+ TSI +  Aerosols+ N2O,train)
summary(model2)

# 
# Problem 4 - Automatically Building the Model
# 
# (4 points possible)
# We have many variables in this problem, and as we have seen above, dropping some from the model does not decrease model quality. R provides a function, step, that will automate the procedure of trying different combinations of variables to find a good compromise of model simplicity and R2. This trade-off is formalized by the Akaike information criterion (AIC) - it can be informally thought of as the quality of the model with a penalty for the number of variables in the model.

model3 = step(model1)
summary(model3)

sse  =sum((test$Temp-predict(model3,test))^2)
sst = sum(( mean(train$Temp) - test$Temp )^2)
rsq =1 -sse/sst 
rsq

