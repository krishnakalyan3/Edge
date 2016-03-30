getwd()
boston = read.csv("boston.csv")
str(boston)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],
       col="blue",pch=19)
