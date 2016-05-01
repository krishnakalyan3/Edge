setwd("/Users/krishna/MOOC/Edge/Data")
songs = read.csv("songs.csv")
str(songs)

# Problem 1.1 - Understanding the Data
dim(subset(songs, year==2010))
sum(songs["artistname"] == "Michael Jackson")
