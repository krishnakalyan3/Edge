setwd("/Users/krishna/MOOC/Edge/Data")
CPS = read.csv("CPSData.csv")
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")
str(CPS)
table(CPS$Industry)
(7073+116639)/ sum(table(CPS$Citizenship)) 
table(CPS$Race,CPS$Hispanic)
summary(CPS)

table(CPS$State, is.na(CPS$MetroAreaCode))
str(CPS$State)
sort(table(CPS$Region))
table(CPS$Region, is.na(CPS$MetroAreaCode))
str(MetroAreaCode)
str(CountryOfBirthCode)

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)

table(is.na(CPS$MetroAreaCode))
sort(table(CPS$MetroArea))
sort(tapply(CPS$Hispanic,CPS$MetroArea,mean) )

table(CPS$MetroArea,CPS$Race == "Asian" )> 1304
sum(((table(CPS$MetroArea,CPS$Race == "Asian" ))> 1304 )[,2])

table(CPS$Race == "Asian")
6520 * 20 /100
sum(sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean)) > .20)

# 3.5 
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))
# 4

# 3.6
# Iowa City, IA

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

# 4.1
# Country
# NA's 176
summary(CPS)

# 4.2
CPS$Country
sort(table(CPS$Country))

CPS$Education
# 4.3
tapply(CPS$Race == "Asian", na.rm(CPS$Country), mean)
tapply(CPS$MetroArea == 
         "New York-Northern New Jersey-Long Island, NY-NJ-PA",
       CPS$Country != "United States" ,mean)


(table(CPS$MetroArea == 
        "New York-Northern New Jersey-Long Island, NY-NJ-PA",(CPS$Country != "United States"))
)

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")

sort(
tapply( CPS$Country == "Somalia",CPS$MetroArea ,mean ,na.rm = TRUE)
)
table(CPS$Country)

getwd()
poll = read.csv("AnonymityPoll.csv")

dim(poll)
str(poll)
sum(poll$Smartphone,na.rm = T)
sum(poll$Smartphone == 0,na.rm = T)

table(poll$State,poll$Region =="South")

table(poll$Internet.Use==0,poll$Smartphone==1)
summary(poll$Smartphone)
?subset
limited = subset(poll,poll$Internet.Use==1 | poll$Smartphone==1)
dim(limited)
summary(limited)

sum(limited$Info.On.Internet ==0)
sum(limited$Worry.About.Info==1,na.rm = T)
sum(limited$Worry.About.Info==1,limited$Worry.About.Info==0,na.rm=T)

(limited$Anonymity.Possible == 1,limited$Tried.Masking.Identity==1)




