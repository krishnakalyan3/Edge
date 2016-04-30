library(ggplot2)
getwd()
setwd("/Users/krishna/MOOC/Edge/Data")
intl = read.csv("intl.csv")
ggplot(intl,aes(x=Region,y=PercentOfIntl)) + geom_bar(stat="Identity")  +
  geom_text(aes(label=PercentOfIntl))
intl = transform(intl,Region = reorder(Region, -PercentOfIntl))
intl$PercentOfIntl = intl$PercentOfIntl * 100

ggplot(intl,aes(x=Region,y=PercentOfIntl)) +
  geom_bar(stat="identity",fill="dark blue") +
  geom_text(aes(label=PercentOfIntl),vjust = -0.4) +
  ylab("Percent of International Students") +
  theme(axis.title.x = element_blank(),axis.text.x=element_text(angle=45,hjust=1))

# New Data Set
library(ggmap)
intlall = read.csv("intlall.csv",stringsAsFactors = F)
head(intlall)
intlall[is.na(intlall)] = 0
world_map  = map_data("world")
str(world_map)
world_map = merge(world_map,intlall,by.x="region",by.y="Citizenship")
str(world_map)

# Plot
ggplot(world_map, aes(x= long,y=lat,group=group)) +
  geom_polygon(fill="white",color="black") +
  coord_map("mercator")

world_map = world_map[order(world_map$group,world_map$order),]

ggplot(world_map, aes(x= long,y=lat,group=group)) +
  geom_polygon(fill="white",color="black") +
  coord_map("mercator")

# Fix China
intlall$Citizenship[intlall$Citizenship =="China (People's Republic Of)"] = "China"
intlall$Citizenship

# Load a new world Mop
world_map  = map_data("world")
# Merge
world_map = merge(world_map,intlall,by.x="region",by.y="Citizenship")
# Reorder
world_map = world_map[order(world_map$group,world_map$order),]

ggplot(world_map, aes(x= long,y=lat,group=group)) +
  geom_polygon(aes(fill=Total),color="black") +
  coord_map("mercator")
str(world_map)

ggplot(world_map, aes(x= long,y=lat,group=group)) +
  geom_polygon(aes(fill=Total),color="black") +
  coord_map("ortho",orientation = c(20,10,10))

households = read.csv("households.csv")
str(households)
library(reshape2)
households[,1:2]
head(melt(households,"Year"))
melt(households,id="Year")

ggplot(melt(households,id="Year"),aes(x=Year, y=value,color=variable)) +
  geom_line(size=2) +
  geom_point(size=5) +
  ylab("Percentage of House")

