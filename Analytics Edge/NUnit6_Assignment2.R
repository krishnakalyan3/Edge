getwd()
airline = read.csv("AirlinesCluster.csv")


# Problem 1.1 - Normalizing the Data
str(airline)
summary(airline)

# Low
# BonusTrans
# FlightTrans

# High
# Balance
# BonusMiles

# Problem 1.2 - Normalizing the Data
# If we don't normalize the data, the clustering will be dominated by the variables that are on a larger scale.

# Problem 1.3 - Normalizing the Data
library(caret)
preproc = preProcess(airline)
airlinesNorm = predict(preproc, airline)
summary(airlinesNorm)
# FlightMiles
# DaysSinceEnroll 

# Problem 2.1 - Hierarchical Clustering
distance = dist(airlinesNorm, method = "euclidean")
clusterIntensity = hclust(distance, method = "ward.D")
plot(clusterIntensity)

# Problem 2.2 - Hierarchical Clustering
airClusters = cutree(clusterIntensity, k=5)
table(airClusters)

# airClusters
# 1    2    3    4    5 
# 776  519  494  868 1342 

# Problem 2.3 - Hierarchical Clustering
tapply(airline$Balance, airClusters, mean)
str(airline)
# 1         2         3         4         5 
# 57866.90 110669.27 198191.57  52335.91  36255.91 
tapply(airline$QualMiles, airClusters, mean)
tapply(airline$BonusMiles, airClusters, mean)
tapply(airline$BonusTrans, airClusters, mean)
tapply(airline$FlightMiles, airClusters, mean)
tapply(airline$FlightTrans, airClusters, mean)
tapply(airline$DaysSinceEnroll, airClusters, mean)

#  Customers who have accumulated a large amount of miles, and the ones with the largest number of flight transactions. 

# Problem 2.5 - Hierarchical Clustering
#  Customers who have accumulated a large amount of miles, mostly through non-flight transactions. 

# Problem 2.6 - Hierarchical Clustering

# Relatively new customers who seem to be accumulating miles, mostly through non-flight transactions. Relatively new customers who seem to be accumulating miles, mostly through non-flight transactions. - correct

# Problem 2.7 - Hierarchical Clustering
#  Relatively new customers who don't use the airline very often.

# Problem 3.1 - K-Means Clustering
set.seed(88 )
KMC = kmeans(airlinesNorm,centers = 5,iter.max = 1000)
table(KMC$cluster)

# 1    2    3    4    5 
# 980  142 1206 1318  353 

# Problem 3.2 - K-Means Clustering
table(KMC$cluster,airClusters)
