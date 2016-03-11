getwd()
setwd("/Users/krishna/MOOC/Edge/Data")
statedata = read.csv("statedata.csv")
str(statedata)

plot(statedata$x,statedata$y)
?tapply
tapply(statedata$HS.Grad,statedata$state.region, mean)
?boxplot
boxplot(statedata$Murder~statedata$state.region)

ne = subset(statedata, Murder>= 10 & state.region == "Northeast")
ne$state.name
names(ne)
model1 = lm(Life.Exp~Population +Income + Illiteracy+ Murder
            + HS.Grad + Frost + Area,statedata)
summary(model1)

plot(statedata$Income, statedata$Life.Exp,pch=19,col="blue")

model2 = lm(Life.Exp~Population +Income + Illiteracy+ Murder
            + HS.Grad + Frost ,statedata)
summary(model2)

model3 = lm(Life.Exp~Population +Income + Murder
            + HS.Grad + Frost ,statedata)
summary(model3)

model4 = lm(Life.Exp~Population  + Murder
            + HS.Grad + Frost ,statedata)
summary(model4)

op =(predict(model4,statedata))
sort(op)
head(statedata)

statedata[with(statedata,order("predict")),]


statedata$state.name[statedata$Life.Exp<68]
which.min(statedata$Life.Exp)
statedata$state.name[40] 

which.max(statedata$predict)
statedata$state.name[47]

statedata$state.name[which.max(statedata$Life.Exp)]

statedata$state.name[which.min(model4$residuals)]

statedata$state.name[which.max(abs(model4$residuals))]

model4$residuals


SSE1 = sum(statedata$residuals^2)
