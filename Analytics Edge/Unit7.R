setwd("/Users/krishna/MOOC/Edge/Data")
WHO = read.csv("WHO.csv")

str(WHO)
plot(WHO$GNI,WHO$FertilityRate)
library("ggplot2")
scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))
scatterplot + geom_point()
scatterplot + geom_line()

scatterplot + geom_point(color= "blue",size = 3, shape=17)
scatterplot + geom_point(color= "darkred",size = 3, shape=8)
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeCycleSavings)) + 
  geom_point()


ggplot(WHO, aes(x = GNI, y = FertilityRate, color = WHO$LifeExpectancy)) + 
  geom_point()


ggplot(WHO, aes(x = FertilityRate, y =Under15 )) + 
  geom_point()

model1 = lm(Under15 ~ log(FertilityRate), WHO)
summary(model1)

ggplot(WHO, aes(x = log(FertilityRate), y =Under15 )) + 
  geom_point() + stat_smooth(method = "lm" ,level = .99)

ggplot(WHO, aes(x = log(FertilityRate), y =Under15 )) + 
  geom_point() + stat_smooth(method = "lm" ,se = F, color = "orange")

ggplot(WHO, aes(x = FertilityRate, y = Under15 , color = Region)) + 
  geom_point() +
 scale_color_brewer(palette="Dark2")

mvt = read.csv("mvt.csv", stringsAsFactors = F)
str(mvt)
mvt$Date =strptime(mvt$Date, format = "%m/%d/%y %H:%M")
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
str(mvt)
table(mvt$Weekday)
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)

ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) +geom_line(aes(group =1))

WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered = TRUE,
                            levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), alpha=0.3) + xlab("Day of the Week") + ylab("Total GTA")

table(mvt$Weekday,mvt$Hour)
DayHourCount  = as.data.frame(table(mvt$Weekday,mvt$Hour))
str(DayHourCount)

DayHourCount$Hour = as.numeric(as.character(DayHourCount$Var2))
ggplot(DayHourCount,aes(x = Hour , y = Freq) )  + geom_line(aes(group=Var1,color = Var1, size =2)) 
DayHourCount$Var1 = factor(DayHourCount$Var1, ordered = T , levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
ggplot(DayHourCount, aes(x =Hour, y = Var1)) + geom_tile(aes(fill = Freq)) +scale_fill_gradient(name = "Total MV Thefts", low = "white", high = "red") + theme(axis.title.y = element_blank())

install.packages("maps")  
install.packages("ggmap")
library(maps)
library(ggmap)

chicago  = get_map(location ="chicago",zoom = 11)
ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x = Longitude , y =Latitude))
LatLonCounts= as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))
str(LatLonCounts)
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))
str(LatLonCounts)
ggmap(chicago ) + geom_point(data=LatLonCounts, aes(x = Long , y=Lat,color = Freq, size = Freq)) +
  scale_color_gradient(low = "yellow", high="red")
ggmap(chicago) + geom_tile(data= LatLonCounts2, aes(x = Long, y = Lat , alpha= Freq), fill = "red")

LatLonCounts2 = subset(LatLonCounts,Freq > 0 )
dim(LatLonCounts2)[1] - dim(LatLonCounts)[1]

murders = read.csv("murders.csv")
statesMap = map_data("state")
?map_data
str(statesMap)
ggplot(statesMap, aes(x=long, y =lat, group = group)) + 
  geom_polygon(fill="white",color="black")
murders$region = tolower(murders$State)
murderMap = merge(statesMap,murders,by="region")
str(murderMap)
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=Population)) +
  geom_polygon(color="black") +scale_fill_gradient(low="white",high="red",guide="legend")
murderMap$MurderRate = murderMap$Murders/murderMap$Population*100000
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=MurderRate)) +
  geom_polygon(color="black") +scale_fill_gradient(low="white",high="red",guide="legend",limits = c(0,10))

ggplot(murderMap,aes(x=long,y=lat,group=group,fill=GunOwnership)) +
  geom_polygon(color="black") +scale_fill_gradient(low="white",high="red",guide="legend")




