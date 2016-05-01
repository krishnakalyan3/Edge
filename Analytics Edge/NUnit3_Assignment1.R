setwd("/Users/krishna/MOOC/Edge/Data")
songs = read.csv("songs.csv")
str(songs)

# Problem 1.1 - Understanding the Data
dim(subset(songs, year==2010))

# > dim(subset(songs, year==2010))
# [1] 373  39

# Problem 1.2 - Understanding the Data
sum(songs["artistname"] == "Michael Jackson")

# > sum(songs["artistname"] == "Michael Jackson")
# [1] 18

# Problem 1.3 - Understanding the Data

mjT10 =subset(songs , artistname == "Michael Jackson")
table(mjT10$Top10)
subset(mjT10[,c("Top10","songtitle")],Top10 ==1)
# 
# Top10         songtitle
# 4329     1 You Rock My World
# 6207     1 You Are Not Alone
# 6210     1    Black or White
# 6218     1 Remember the Time
# 6915     1     In The Closet

# Problem 1.4 - Understanding the Data
table(songs$timesignature)
# 
# table(songs$timesignature)
# 
# 0    1    3    4    5    7 
# 10  143  503 6787  112   19 


# Problem 1.5 - Understanding the Data
songs[which.max(songs$tempo),c("songtitle")]

# > songs[which.max(songs$tempo),c("songtitle")]
# [1] Wanna Be Startin' Somethin'

# Problem 2.1 - Creating Our Prediction Model
SongsTrain = subset(songs,year <= 2009)
SongsTest = subset(songs , year == 2010)

dim(SongsTrain) 
# > dim(SongsTrain)
# [1] 7201   39

# Problem 2.2 - Creating our Prediction Model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

# The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10 The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10 - correct

# Problem 2.4 - Creating Our Prediction Model
# Mainstream listeners tend to prefer less complex songs

# Problem 2.5 - Creating Our Prediction Model
# Mainstream listeners prefer songs with heavy instrumentation 

# Problem 3.1 - Beware of Multicollinearity Issues!
cor(SongsTrain$loudness,SongsTrain$energy)

# > cor(SongsTrain$loudness,SongsTrain$energy)
# [1] 0.7399067

# Problem 3.2 - Beware of Multicollinearity Issues!
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
# Model 2 suggests that songs with high energy levels tend to be more popular. This contradicts our observation in Model 1

# Problem 3.3 - Beware of Multicollinearity Issues!
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# Problem 4.1 - Validating Our Model
predictTest = predict(SongsLog3,SongsTest,type="response") 
predictTest
op =table(SongsTest$Top10,predictTest>=0.45)
op
(309 + 19) / sum(op)
# > (309 + 19) / sum(op)
# [1] 0.8793566

# Problem 4.2 - Validating Our Model
op = table(SongsTest$Top10)
314/(sum(op))
op
# > 314/(sum(op))
# [1] 0.8418231

# Problem 4.3 - Validating Our Model
sum(predictTest>=0.45)
cm = table(SongsTest$Top10,predictTest>=0.45)
# 
# FALSE TRUE
# 0   309    5
# 1    40   19

# 19
# 5

# Problem 4.4 - Validating Our Model
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Sensitivity =  TP/(TP+FN)     #TP / Positives
Specificity =  TN/(TN + FP)   #TN / Negatives
Sensitivity
Specificity
