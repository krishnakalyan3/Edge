getwd()
setwd("/Users/krishna/MOOC/Edge/Data")
library(ggplot2)
library(maps)
library(ggmap)
library(stringr) 
library(tm) 
library(SnowballC) 
statesMap = map_data("state")

# Problem 1.1 - Drawing a Map of the US
str(statesMap)
table(statesMap$group)
# 63

# Problem 1.2 - Drawing a Map of the US
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")
#balck

# Problem 2.1 - Coloring the States by Predictions
polling = read.csv('pollingImputed.csv')
table(polling$Year)
train = subset(polling, Year <= 2008)
test = subset(polling, Year > 2008)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=train, family="binomial")
str(mod2)
TestPrediction = predict(mod2, newdata=test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, test$State)
mean(predictionDataFrame$TestPrediction)
# 22
# 0.4852626

# Problem 2.2 - Coloring the States by Predictions
predictionDataFrame$region = tolower(predictionDataFrame$test.State)
dim(statesMap)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
head(predictionMap)
dim(statesMap)

# 15034
# 15537

# Problem 2.3 - Coloring the States by Predictions
# Because we only make predictions for 45 states, we no longer have observations for some of the states. These observations were removed in the merging process.

# Problem 2.4 - Coloring the States by Predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
#  Light blue 

# Problem 2.5 - Coloring the States by Predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
# The two maps look very similar. This is because most of our predicted probabilities are close to 0 or close to 1. 

# Problem 3.1 - Understanding the Predictions
op = (subset(predictionMap,test.State == "Florida"))

# Problem 3.2 - Understanding the Predictions
predictionDataFrame
#Our prediction model did not do a very good job of correctly predicting the state of Florida, and we were very confident in our incorrect prediction.

# PROBLEM 4 - PARAMETER SETTINGS
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black",linetype=5) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
?geom_polygon
?linetype


# VISUALIZING NETWORK DATA
edges = read.csv('edges.csv')
users = read.csv('users.csv')

2*nrow(edge)/nrow(users)
# 4.949153

# Problem 1.2 - Summarizing the Data
summary(users)
# locale B

# Problem 1.3 - Summarizing the Data
table(users$school,users$gender)

# Problem 2.1 - Creating a Network
install.packages('igraph')
library(igraph)
?graph.data.frame

g = graph.data.frame(edges, FALSE, users)

# Problem 2.2 - Creating a Network
plot(g, vertex.size=5, vertex.label=NA)

# Problem 2.3 - Creating a Network
sum(degree(g) >=10)
#9

# Problem 2.4 - Creating a Network

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
min(V(g)$size)

# Problem 3.1 - Coloring Vertices
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

# Problem 3.2 - Coloring Vertices
str(users)
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)

# Problem 3.3 - Coloring Vertices
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "blue"
V(g)$color[V(g)$locale == "B"] = "red"
plot(g, vertex.shape="sphere",vertex.label=NA)

# Problem 4 - Other Plotting Options
?igraph.plotting


# VISUALIZING TEXT DATA USING WORD CLOUDS
tweets = read.csv("tweets.csv")
str(tweets)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus , tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c('appele',stopwords('english')))
allTweets = DocumentTermMatrix(corpus)
allTweets

# Problem 1.2 - Preparing the Data
#  It will be easier to read and understand the word cloud if it includes full words instead of just the word stems

# Problem 2.1 - Building a Word Cloud
#install.packages("wordcloud")
library(wordcloud)
?wordcloud
#  colnames colnames - correct

# Problem 2.2 - Building a Word Cloud
# colSums colSums - correct

# Problem 2.3 - Building a Word Cloud
dtmAbstract = as.data.frame(as.matrix(allTweets))
wordcloud(colnames(dtmAbstract),colSums(dtmAbstract),scale=c(2, 0.25),random.color =T)

# Problem 3.1 - Size and Color
negativeTweets = subset(dtmAbstract,tweets$Avg <= -1)
dim(negativeTweets)
wordcloud(colnames(negativeTweets),colSums(negativeTweets))

# Problem 4.1 - Selecting a Color Palette
#install.packages('RColorBrewer')
library(RColorBrewer)
brewer.pal()
display.brewer.all()
brewer.pal(9, "Blues")[c(-5, -6, -7, -8, -9)]
wordcloud(colnames(dtmAbstract),colSums(dtmAbstract),scale=c(2, 0.25),colors=brewer.pal(9, "Blues")[c(-5, -6, -7, -8, -9)])
wordcloud(colnames(dtmAbstract),colSums(dtmAbstract),scale=c(2, 0.25),colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)])
colors=brewer.pal(9, "Blues")
colors
