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