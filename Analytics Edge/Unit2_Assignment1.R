getwd()
setwd("/Users/krishna/MOOC/Edge/Data")
elantra = read.csv("elantra.csv")
head(elantra)
str(elantra)
ElantraTrain = elantra[elantra$Year<=2012,]
ElantraTest = elantra[elantra$Year>2012,]

model1 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = ElantraTrain)
summary(model1)

model2 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data = ElantraTrain)
summary(model2)

110.69 * (3 - 1) 
110.69 * (5 - 1) 

model3 = lm(ElantraSales ~ as.factor(Month) + Unemployment + CPI_all + CPI_energy + Queries, data = ElantraTrain)
summary(model3)
cor(ElantraTrain)

ElantraTrain.factor = cbind(ElantraTrain,as.factor(ElantraTrain$Year))
head(ElantraTrain.factor)

model4 = lm(ElantraSales ~ as.factor(Month) + Unemployment + CPI_all + CPI_energy + Queries, data = ElantraTrain)
summary(model4)

model5 = lm(ElantraSales ~ as.factor(Month) + Unemployment + CPI_all + CPI_energy , data = ElantraTrain)
summary(model5)


# 190757747
mean(ElantraTrain$ElantraSales)
predict(model5,ElantraTest)

#You can compute the SST as the sum of the squared differences 
#between ElantraSales in the testing set and the mean of ElantraSales 
SST = sum((ElantraTest$ElantraSales  - mean(ElantraTrain$ElantraSales))^2)
SSE= sum((ElantraTest$ElantraSales - predict(model5,ElantraTest))^2)

Rsq = 1- SSE/SST
Rsq

abserror = abs(ElantraTest$ElantraSales - predict(model5,ElantraTest))
indexmax =match(max(abserror),abserror)

ElantraTest[indexmax,]