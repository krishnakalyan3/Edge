getwd()
setwd("/Users/krishna/MOOC/Edge/Data")
mvt = read.csv("mvtWeek1.csv")
dim(mvt)
str(mvt)
max(mvt$ID)
min(mvt$Beat)
sum(mvt$Arrest)
alley = mvt$LocationDescription == 'ALLEY'
sum(alley)
mvt$Date
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
str(mvt)
mvt$Date
mvt$Date = DateConvert
str(mvt)
min(table(mvt$Month))
max(table(mvt$Weekday))

max(table(mvt$Month,mvt$Arrest))

hist(mvt$Date, breaks=100)
boxplot(mvt$Date~mvt$Arrest)
?boxplot
table(mvt$Arrest,mvt$Year)

sort(table(mvt$LocationDescription))
Top5 = subset(mvt , mvt$LocationDescription == "STREET"|
              mvt$LocationDescription =="PARKING LOT/GARAGE(NON.RESID.)"|
              mvt$LocationDescription =="ALLEY"|
              mvt$LocationDescription =="GAS STATION"|
              mvt$LocationDescription =="DRIVEWAY - RESIDENTIAL"
              )
dim(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5)
table(Top5$LocationDescription,Top5$Arrest) /                                 
((table(Top5$LocationDescription,Top5$Arrest)[,1]) + (table(Top5$LocationDescription,Top5$Arrest)[,2]) )
