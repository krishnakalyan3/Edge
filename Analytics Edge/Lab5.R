setwd("/Users/krishna/MOOC/Edge/Data")
movies = read.table("movieLens.txt",header = F,sep="|",quote="\"")
str(movies)
colnames(movies) = c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown","Action","Adventure",
                     "Animation","Childrens","Comedy","Crime", "Documentary","Drama","Fantasy","FilmNoir",
                     "Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies = unique(movies)
str(movies)

# Quiz 
sum(movies$Comedy)
sum(movies$Western)
sum(movies$Romance & movies$Drama)

table(movies$Comedy)
table(movies$Western)
table(movies$Romance & movies$Drama)


distances = dist(movies[2:20],method = "euclidean")
?dist
clusterMovies = hclust(distances, method = "ward.D")
plot(clusterMovies)
clustergroups = cutree(clusterMovies,k =10)
tapply(movies$Action, clustergroups, mean)
tapply(movies$Romance, clustergroups, mean)
subset(movies,Title=="Men in Black (1997)")
clustergroups[257]
cluster2 = subset(movies,clustergroups ==2)
cluster2$Title[1:10]
