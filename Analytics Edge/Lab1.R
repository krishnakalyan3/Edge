# Question 1
sd(c(5,8,12))

# Q 2
which.min(c(4,8,6))

# Q 3 
Sys.setlocale("LC_ALL", "C")

8*6
2^16
sqrt(2)
abs(-16)
SR2 =sqrt(2)
SR2
HY =365*24
ls()
Country =c("India","Mexico","USA")
LifeExpentancy = c(74,75,76)
Country
LifeExpentancy
Country[1]
LifeExpentancy[3]
seq(0,100,2)
CountryData = data.frame(Country,LifeExpentancy)
CountryData
CountryData$Pop =c(123123,23345,123542)
CountryData
Country = c("Austrialia","Greece")
LifeExpentancy = c(82,81)
Pop = c(23050,11125)
NewCountryData = data.frame(Country,LifeExpentancy,Pop)
NewCountryData
AllCountryData = rbind(CountryData,NewCountryData)
AllCountryData

getwd()

WHO =read.csv("WHO.csv")

str(WHO)
summary(WHO)
WHO_Europe = subset(WHO,Region=="Europe")
str(WHO_Europe)
write.csv(WHO_Europe,"WHO_Europe.csv")
ls()
rm(WHO_Europe)
ls()

WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
which.min(WHO$Under15)
WHO$Country[86]
which.max(WHO$Under15)
WHO$Country[124]
plot(WHO$GNI,WHO$FertilityRate)
