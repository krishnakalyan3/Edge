getwd()
setwd("/Users/krishna/MOOC/Edge/Data")

trials = read.csv("clinical_trial.csv",stringsAsFactors=FALSE)

# Problem 1.1 - Loading the Data
str(trials)
max(nchar(trials$abstract))

# > max(nchar(trials$abstract))
# [1] 3708

# Problem 1.2 - Loading the Data
table(nchar(trials$abstract)< 1)

# > table(nchar(trials$abstract)< 1)
# 
# FALSE  TRUE 
# 1748   112 

# Problem 1.3 - Loading the Data
which.min((nchar(trials$title)))
trials$title[1258]

# > trials$title[1258]
# [1] "A decade of letrozole: FACE."

# Problem 2.1 - Preparing the Corpus
library(stringr) 
library(tm) 
library(SnowballC) 
sparse = 1 - 0.05
corpusTitle = Corpus(VectorSource(trials$title))
corpusTitle <- tm_map(corpusTitle , tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords('english'))
corpusTitle = tm_map(corpusTitle, stemDocument, language = 'english')
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitles = removeSparseTerms(dtmTitle, sparse)

corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusAbstract , tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords('english'))
corpusAbstract = tm_map(corpusAbstract, stemDocument, language = 'english')
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmAbstracts = removeSparseTerms(dtmAbstract, sparse)

dtmTitle = as.data.frame(as.matrix(dtmTitles))
dtmAbstract = as.data.frame(as.matrix(dtmAbstracts))

# Problem 2.2 - Preparing the Corpus
# Abstracts tend to have many more words than titles

# Problem 2.3 - Preparing the Corpus
which.max(colSums(dtmAbstract) )

# > which.max(colSums(dtmAbstract) )
# patient 
# 212 

# Problem 3.1 - Building a model
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
head(dtmTitle)
head(dtmAbstract)

# Adding the letter T in front of all the title variable names and adding the letter A in front of all the abstract variable names.

# Problem 3.2 - Building a Model
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
dim(dtm)

# > dim(dtm)
# [1] 1860  367

# Problem 3.3 - Building a Model
library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, 0.7)
train = subset(dtm, spl ==T)
test  = subset(dtm, spl ==F)
op =table(test$trial)
(313)/sum(op)

# > (313)/sum(op)
# [1] 0.5609319

# Problem 3.4 - Building a Model
model1 = rpart(train$trial ~ . , data = train, method="class")
prp(model1)

# Tphase

# Problem 3.5 - Building a Model
predTest = predict(model1, test)
max(predTest[,2])

# > max(predTest[,2])
# [1] 0.8718861

# Problem 3.6 - Building a Model
# The maximum predicted probability will likely be exactly the same in the testing set. The maximum predicted probability will likely be exactly the same in the testing set. - correct

# Problem 3.7 - Building a Model
cm  =table(train$trial,predict(model1, train)[,2]>=0.5)
cm
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Sensitivity =  TP/(TP+FN)     #TP / Positives
Specificity =  TN/(TN + FP)   #TN / Negatives
Acc = (TP + TN)/sum(cm) 
Sensitivity
Specificity
Acc

# > Sensitivity
# [1] 0.770979
# > Specificity
# [1] 0.8643836
# > Acc
# [1] 0.8233487

# Problem 4.1 - Evaluating the model on the testing set
cm  =table(test$trial,predict(model1, test)[,2]>=0.5)
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Acc = (TP + TN)/sum(cm) 
Acc

# Problem 4.2 - Evaluating the Model on the Testing Set
library(ROCR)
predROCR = prediction(predict(model1, test , ),test$trial)
prefROCR = performance(predROCR,"tpr","fpr")
plot(prefROCR, colorize = T )
performance(predROCR,"auc")@y.values
