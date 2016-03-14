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
