pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

# Problem 1.1 - Dataset size
dim(pisaTrain)[1]
# [1] 3663

# Problem 1.2 - Summarizing the dataset
tapply(pisaTest$readingScore,pisaTest$male,mean)
# 0        1 
# 509.2992 485.3103 

# Problem 1.3 - Locating missing values
summary(pisaTrain)
colnames(pisaTrain)[colSums(is.na(pisaTrain)) > 0]

# Problem 1.4 - Removing missing values
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
dim(pisaTrain)[1]
dim(pisaTest)[1]

# Problem 2.1 - Factor variables