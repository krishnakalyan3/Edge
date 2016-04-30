setwd("/Users/krishna/MOOC/Edge/Data")
parole = read.csv("parole.csv")
str(parole)

# Convert to Factors
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

# Problem 1.1 - Loading the Data
# What fraction of parole violators are female?
table(parole$male, parole$violator)
14/(14+64)

# x - axis male / female
# y -axis female violator / male violator

# Problem 1.2 - Loading the Data
table(parole$crime,parole$state)
# Drug Related Crime 
# (3,2) - 64 (DrugRelatedCrime, Kentuky)

# Problem 2.1 - Creating a Basic Histogram
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5)

# Problem 2.2 - Creating a Basic Histogram
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5,color="blue")

# Problem 3.1 - Adding Another Dimension
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)

# Problem 3.2 - Adding Another Dimension
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(.~male)

# Problem 3.3 - Adding Another Dimension
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)
colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data = parole, aes(x = age, fill = male)) + 
  geom_histogram(binwidth = 5) + scale_fill_manual(values=colorPalette)

# Problem 3.4 - Adding Another Dimension
ggplot(data = parole, aes(x = age, fill = male)) + 
  geom_histogram(binwidth = 5,position="identity",alpha=0.5,color="white") + scale_fill_manual(values=colorPalette)

# Problem 4.1 - Time Served
ggplot(data = parole, aes(x = time.served, fill = male)) + 
  geom_histogram(binwidth = 1,position="identity",alpha=0.5,color="white") + scale_fill_manual(values=colorPalette)

# Problem 4.2 - Time Served
ggplot(data = parole, aes(x = time.served, fill = male)) + 
  geom_histogram(binwidth = 0.1,position="identity",alpha=0.5,color="white") + scale_fill_manual(values=colorPalette)

# Problem 4.3 - Time Served
ggplot(data = parole, aes(x = time.served, fill = male)) + 
  geom_histogram(binwidth = 1,position="identity",alpha=0.5,color="white") + 
  scale_fill_manual(values=colorPalette)   +
  facet_grid( crime ~.)
# Driving-related 
# Drug-related

# Problem 4.4 - Time Served
ggplot(data = parole, aes(x = time.served, fill = crime)) + 
  geom_histogram(binwidth = 1,position="identity",alpha=0.5,color="white") + 
  scale_fill_manual(values=colorPalette)   +
  facet_grid( . ~ crime)