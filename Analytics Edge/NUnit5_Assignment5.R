getwd()
setwd("/Users/krishna/MOOC/Edge/Data")
wiki = read.csv("wiki.csv",stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
str(wiki)

# Problem 1.1 - Bags of Words
summary(wiki$Vandal)

# Problem 1.2 - Bags of Words
library(stringr) 
library(tm) 
library(SnowballC) 
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords('english'))
corpusAdded = tm_map(corpusAdded, stemDocument, language = 'english')
dtmAdded <- DocumentTermMatrix(corpusAdded)
(dtmAdded)

# Problem 1.3 - Bags of Words
sparse = 1 - 0.003
sparseAdded = removeSparseTerms(dtmAdded, sparse)
sparseAdded

# 166

# Problem 1.4 - Bags of Words
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemove = Corpus(VectorSource(wiki$Removed))
corpusRemove = tm_map(corpusRemove, removeWords, stopwords('english'))
corpusRemove = tm_map(corpusRemove, stemDocument, language = 'english')
dtmRemove <- DocumentTermMatrix(corpusRemove)

sparse = 1 - 0.003
sparseRemove = removeSparseTerms(dtmRemove, sparse)
wordsRemoved = as.data.frame(as.matrix(sparseRemove))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
# 162

wordsAdded
wordsRemoved

# Problem 1.5 - Bags of Words
library(caTools)
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWordsN = cbind(wiki$Vandal,wikiWords)
sample.split(123)

spl = sample.split(wikiWordsN$`wiki$Vandal`, 0.7)
train = subset(wikiWordsN, spl ==T)
test  = subset(wikiWordsN, spl ==F)
model1 = rpart(train$`wiki$Vandal` ~ . , data = train, method="class")

predTest = predict(model1, test)
predTest
cm = table(test$`wiki$Vandal`, predTest[,2]>=0.5)
(618+ 12)/sum(cm)

# 0.5417025

# Problem 1.7 - Bags of Words
prp(model1)

str(wikiWords)
# Problem 2.1 - Problem-specific Knowledge
wikiWords2 = wikiWordsN
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
sum(wikiWords2$HTTP)
# 217

# Problem 2.2 - Problem-Specific Knowledge 
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
model3 = rpart(wikiTrain2$`wiki$Vandal` ~ .  , data = wikiTrain2, method="class")
predtest  = predict(model3, wikiTest2, type = "class")
op =table(wikiTest2$`wiki$Vandal`, predtest)

(603+ 69)/ sum(op)

# Problem 2.3 - Problem-Specific Knowledge
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemove))
mean(wikiWords2$NumWordsAdded)
# 4.050052

# Problem 2.4 - Problem-Specific Knowledge
train1 = subset(wikiWords2, spl==TRUE)
test1 = subset(wikiWords2, spl==FALSE)
model4 = rpart(wikiTrain2$`wiki$Vandal` ~ .  , data = wikiTrain2, method="class")
predtest  = predict(model4, test1, type = "class")
op =table(test1$`wiki$Vandal`, predtest)
op
(535 + 301)/sum(op)

# Problem 3.1 - Using Non-Textual Data
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
train1 = subset(wikiWords3, spl==TRUE)
test1 = subset(wikiWords3, spl==FALSE)
model5 = rpart(wikiWords3$`wiki$Vandal` ~ .  , data = wikiWords3, method="class")
predtest  = predict(model5, test1, type = "class")
op =table(test1$`wiki$Vandal`, predtest)
(535 + 83)/sum(op)

