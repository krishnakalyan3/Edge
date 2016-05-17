getwd()
setwd("/Users/krishna/MOOC/Edge/Data")
kos = read.csv("dailykos.csv")
str(kos)

distance = dist(kos, method = "euclidean")
clusterIntensity = hclust(distance, method = "ward.D")
dim(kos)

# Problem 1.1 - Hierarchical Clustering
# We have a lot of observations, so it takes a long time to compute the distance between each pair of observations.
# We have a lot of variables, so the distance computation is long.

# Problem 1.2 - Hierarchical Clustering
plot(clusterIntensity)
# 2,3

# Problem 1.3 - Hierarchical Clustering
# 7
# 8

# Problem 1.4 - Hierarchical Clustering
kosClusters = cutree(clusterIntensity, k=7)
table(kosClusters)

# > table(kosClusters)
# kosClusters
# 1    2    3    4    5    6    7 
# 1266  321  374  139  407  714  209 

# Problem 1.5 - Hierarchical Clustering
cluster1 =subset(kos,kosClusters == 1)
tail(sort(colMeans(cluster1)))

# > tail(sort(colMeans(cluster1)))
# state republican       poll   democrat      kerry       bush 
# 0.7575039  0.7590837  0.9036335  0.9194313  1.0624013  1.7053712 

# Problem 1.6 - Hierarchical Clustering
cluster1 =subset(kos,kosClusters == 1)
tail(sort(colMeans(cluster1)))

cluster2 =subset(kos,kosClusters == 2)
tail(sort(colMeans(cluster2)))

cluster3 =subset(kos,kosClusters == 3)
tail(sort(colMeans(cluster3)))

cluster4 =subset(kos,kosClusters == 4)
tail(sort(colMeans(cluster4)))

cluster5 =subset(kos,kosClusters == 5)
tail(sort(colMeans(cluster5)))

cluster6 =subset(kos,kosClusters == 6)
tail(sort(colMeans(cluster6)))

cluster7 =subset(kos,kosClusters == 7)
tail(sort(colMeans(cluster7)))

# november, poll, vote, challenge november, poll, vote, challenge - correct
# Cluster 5 
# Cluster 7 

# Problem 2.1 - K-Means Clustering
set.seed(1000)
KMC = kmeans(kos,centers = 7)
table(KMC$cluster)

# 1    2    3    4    5    6    7 
# 146  144  277 2063  163  329  308 

# Problem 2.2 - K-Means Clustering
cluster1 =subset(kos,KMC$cluster==1)
tail(sort(colMeans(cluster1)))

cluster2 =subset(kos,KMC$cluster==2)
tail(sort(colMeans(cluster2)))

cluster3 =subset(kos,KMC$cluster==3)
tail(sort(colMeans(cluster3)))

cluster4 =subset(kos,KMC$cluster==4)
tail(sort(colMeans(cluster4)))

cluster5 =subset(kos,KMC$cluster==5)
tail(sort(colMeans(cluster5)))

cluster6 =subset(kos,KMC$cluster==6)
tail(sort(colMeans(cluster6)))

cluster7 =subset(kos,KMC$cluster==7)
tail(sort(colMeans(cluster7)))

# Cluster 3
# Cluster 2

# Problem 2.3 - K-Means Clustering
cluster2 =subset(kos,KMC$cluster==2)
tail(sort(colMeans(cluster2)))

# primaries  democrat    edward     clark     kerry      dean 
# 2.319444  2.694444  2.798611  3.090278  4.979167  8.277778 

# Hierarchical Cluster 7

# Problem 2.4 - K-Means Clustering
cluster3 =subset(kos,KMC$cluster==3)
tail(sort(colMeans(cluster3)))

# administration          iraqi       american           bush            war 
# 1.389892       1.610108       1.685921       2.610108       3.025271 
# iraq 
# 4.093863 
# Cluster 5

# Problem 2.5 - K-Means Clustering
cluster7 =subset(kos,KMC$cluster==7)
tail(sort(colMeans(cluster7)))

# presided    voter campaign     poll     bush    kerry 
# 1.324675 1.334416 1.383117 2.788961 5.970779 6.480519

table(KMC$cluster,kosClusters)

#      1    2    3    4    5    6    7
# 1    3    0   85   10   48    0    0
# 2   11    0   10    5    0    2  116
# 3   64    0   42    0  171    0    0
# 4 1045    0   79    0  145  712   82
# 5   32    0  126    1    3    0    1
# 6    0  320    8    0    1    0    0
# 7  111    1   24  123   39    0   10

# Problem 2.6 - K-Means Clustering
# Hierarchical Cluster 2 Hierarchical Cluster 2 - correct