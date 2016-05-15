getwd()
setwd("/Users/krishna/MOOC/Edge/Data")

email = read.csv("emails.csv",stringsAsFactors=FALSE)


# Problem 1.1 - Loading the Dataset
str(email)

# 'data.frame':	5728 obs. of  2 variables:

# Problem 1.2 - Loading the Dataset
sum(email$spam)
# 1368

# Problem 1.3 - Loading the Dataset
# subject

# Problem 1.4 - Loading the Dataset
# Yes

# Problem 1.5 - Loading the Dataset
max(nchar(email$text))

# Problem 1.6 - Loading the Dataset
which.min(nchar(email$text))

# Problem 2.1 - Preparing the Corpus
sparse = 1 - 0.05
corpus = Corpus(VectorSource(email$text))
corpus = tm_map(corpus , tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords('english'))
corpus = tm_map(corpus, stemDocument, language = 'english')
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, sparse)
spdtm

# 28687

# Problem 2.2 - Preparing the Corpus
# 330

# Problem 2.3 - Preparing the Corpus
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))
# 
# enron 
# 92 

# Problem 2.4 - Preparing the Corpus
emailsSparse$spam = email$spam
ham = subset(emailsSparse,spam ==0)
sort(colSums(ham))
# 6

# Problem 2.5 - Preparing the Corpus
spam = subset(emailsSparse,spam ==1)
(sort(colSums(spam)) )

#3

# Problem 3.1 - Building machine learning models
emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl ==T)
test  = subset(emailsSparse, spl ==F)

spamLog = glm(spam ~ . , data = train , family = "binomial")
spamCART = rpart(spam ~ . , data = train, method="class")
library(randomForest)
spamRF = randomForest(spam ~ . , data = train)

predlog = predict(spamLog)
predlog
predCART = predict(spamCART)
predCART
predRF = predict(spamRF,train)
predRF

table(predlog < 0.00001)
table(predlog > 0.99999)
table(predlog >= 0.00001 & predlog <= 0.99999)

# Problem 3.2 - Building Machine Learning Models
summary(spamLog)
# 0

# Problem 3.3 - Building Machine Learning Models
prp(spamCART)
# 2

# Problem 3.4 - Building Machine Learning Models
cm  =table(train$spam,predlog>=0.5)
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Acc = (TP + TN)/sum(cm) 
Acc
#  0.9990025

# Problem 3.5 - Building Machine Learning Models

library(ROCR)
predROCR = prediction(predlog,train$spam)
prefROCR = performance(predROCR,"tpr","fpr")
plot(prefROCR, colorize = T )
performance(predROCR,"auc")@y.values

# 0.9999959

# Problem 3.6 - Building Machine Learning Models
cm  =table(train$spam,predCART[,2]>=0.5)
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Acc = (TP + TN)/sum(cm) 
Acc

# 0.942394

# Problem 3.7 - Building Machine Learning Models
library(ROCR)
predROCR = prediction(predCART[,2],train$spam)
prefROCR = performance(predROCR,"tpr","fpr")
plot(prefROCR, colorize = T )
performance(predROCR,"auc")@y.values

# Problem 3.8 - Building Machine Learning Models
cm  =table(train$spam,predRF>=0.5)
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Acc = (TP + TN)/sum(cm) 
Acc

# Problem 3.9 - Building Machine Learning Models
library(ROCR)
predROCR = prediction(predRF,train$spam)
prefROCR = performance(predROCR,"tpr","fpr")
plot(prefROCR, colorize = T )
performance(predROCR,"auc")@y.values

# 0.9999928

# Problem 4.1 - Evaluating on the Test Set
cm  =table(test$spam, predict(spamLog, test) >=0.5)
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Acc = (TP + TN)/sum(cm) 
Acc
#  0.9511059

# What is the testing set AUC of spamLog?
predROCR = prediction(predict(spamLog, test),test$spam)
prefROCR = performance(predROCR,"tpr","fpr")
plot(prefROCR, colorize = T )
performance(predROCR,"auc")@y.values

# 0.9767994

# Problem 4.3 - Evaluating on the Test Set
cm  =table(test$spam, predict(spamCART, test)[,2] >=0.5)
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Acc = (TP + TN)/sum(cm) 
Acc

# 0.9394645

# Problem 4.4 - Evaluating on the Test Set
predROCR = prediction(predict(spamCART, test)[,2],test$spam)
prefROCR = performance(predROCR,"tpr","fpr")
plot(prefROCR, colorize = T )
performance(predROCR,"auc")@y.values

# Problem 4.5 - Evaluating on the Test Set
cm  =table(test$spam, predict(spamRF, test) >=0.5)
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Acc = (TP + TN)/sum(cm) 
Acc

# 0.9743888

# Problem 4.6 - Evaluating on the Test Set
predROCR = prediction(predict(spamRF, test),test$spam)
prefROCR = performance(predROCR,"tpr","fpr")
plot(prefROCR, colorize = T )
performance(predROCR,"auc")@y.values




