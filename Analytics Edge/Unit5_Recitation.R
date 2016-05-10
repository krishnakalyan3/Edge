setwd("/Users/krishna/MOOC/Edge/Data")
emails = read.csv("energy_bids.csv",stringsAsFactors = F)
str(emails)
emails$email[1]
strwrap(emails$email[1])
emails$responsive[1]
strwrap(emails$email[2])
emails$responsive[2]

table(emails$responsive)


library(tm)
corpus = Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
strwrap(corpus[[1]])

dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm,0.97)
dtm

labeledTerms = as.data.frame(as.matrix(dtm))
labeledTerms$responsive = emails$responsive
str(labeledTerms)

library(caTools)
library(rpart)
library(rpart.plot)
set.seed(144)
spl = sample.split(labeledTerms$responsive, 0.7)
train = subset(labeledTerms, spl ==T)
test  = subset(labeledTerms, spl ==F)
emailCart = rpart(responsive ~ . , data = train, method="class")
prp(emailCart)
pred = predict(emailCart, test)
pred[1:10,]
pred.prob  = pred[,2]

table(test$responsive, pred.prob >=0.5)
(204+18)/(11+24+204+18)
table(test$responsive)
215/(215 + 42)

library(ROCR)
predROCR = prediction(pred.prob,test$responsive)
prefROCR = performance(predROCR,"tpr","fpr")
plot(prefROCR, colorize = T )
performance(predROCR,"auc")@y.values
exp(.99)

0.90 = 1/(1+exp(-score))
0.90 *(1 + exp(-s)) = 1 
0.90 + .90 * exp(-s) = 1
.90 * exp(-s) = 1 -.90
exp(-s) = (1 - .90)/.90
s = log((1 -.90)/.90)
s
