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
Outliers = subset(WHO,GNI>10000 & FertilityRate >2.5)
nrow(Outliers)
Outliers[c("Country","GNI","FertilityRate")]

mean(WHO$Over60)
which.min(WHO$Over60)
WHO$Country[183]
WHO$Country[which.max(WHO$LiteracyRate)]
hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region,xlab="",ylab="Life Expetancy",main="Life Expetancy of Countries by Region")
?boxplot
table(WHO$Region)
tapply(WHO$Over60, WHO$Region, mean)
?tapply
tapply(WHO$LiteracyRate, WHO$Region, min,na.rm=T)
tapply(WHO$ChildMortality, WHO$Region, mean)


## Recitation
getwd()
USDA = read.csv("../Data/USDA.csv")
str(USDA)
summary(USDA)
USDA$Sodium
which.max(USDA$Sodium)
names(USDA)
USDA$Description[which.max(USDA$Sodium)]
HighSodium = subset(USDA,Sodium >10000)
nrow(HighSodium)
HighSodium$Description
match("CAVIAR",USDA$Description)
USDA$Sodium[4154]
USDA$Sodium[match("CAVIAR",USDA$Description)]
summary(USDA$Sodium)
sd(USDA$Sodium,na.rm=T)
plot(USDA$Protein,USDA$TotalFat,ylab="Fat",xlab="Protein",main="Protein vs Fat",col="red",pch=19)
hist(USDA$VitaminC,xlab =  "Vitamin C in mg", main ="Histogram of Vit C levels",xlim=c(0,100),breaks=2000)
boxplot(USDA$Sugar, main = "Boxplot of Sugar Levels" ,ylab="Sugar in gm")

USDA$Sodium[1] > mean(USDA$Sodium,na.rm=T)
USDA$Sodium[50] > mean(USDA$Sodium,na.rm=T)
HighSodium =  USDA$Sodium > mean(USDA$Sodium,na.rm=T)
str(HighSodium)
HighSodium =  as.numeric(USDA$Sodium > mean(USDA$Sodium,na.rm=T))
USDA$HighSodium =  as.numeric(USDA$Sodium > mean(USDA$Sodium,na.rm=T))
str(USDA)
USDA$HighProtein =  as.numeric(USDA$Protein > mean(USDA$Protein,na.rm=T))
USDA$HighFat =  as.numeric(USDA$TotalFat > mean(USDA$TotalFat,na.rm=T))
USDA$HighCarbs =  as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate,na.rm=T))
str(USDA)
table(USDA$HighSodium,USDA$HighFat)
tapply(USDA$Iron, USDA$HighProtein, mean,na.rm=T)
tapply(USDA$VitaminC, USDA$HighCarbs, max,na.rm=T)
tapply(USDA$VitaminC, USDA$HighCarbs, summary,na.rm=T)
