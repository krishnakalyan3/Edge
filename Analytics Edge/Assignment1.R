install.packages("dplyr")
library(dplyr)
getwd()
Internet.Poll = read.csv(file.choose())
str(Internet.Poll)
dim(Internet.Poll)
summary(Internet.Poll)

# How many people participated in the poll?
# 1002

summary(Internet.Poll$Smartphone)
table(Internet.Poll$Smartphone)

# How many interviewees responded that they use a smartphone?
# 487

# How many interviewees responded that they don't use a smartphone?
# 472

# How many interviewees did not respond to the question, resulting in a missing value, or NA, in the summary() output?
# 43

table(Internet.Poll$Sex, Internet.Poll$Region)
head(Internet.Poll)
op  = table(Internet.Poll$State,Internet.Poll$Region)
data <- op[order(op[,2],decreasing = T),]

op1 = Internet.Poll %>% filter(Region=="Midwest" ) %>% select(State,Region)
table(op1)

op = Internet.Poll %>% subset(Region=="South") %>% select(State,Region)
top = tapply(op$State,op$Region,  summary,na.rm=T)
sort(top$South, decreasing = TRUE)

# Which was the state in the South census region with the largest number of interviewees?
# Texas

summary(Internet.Poll)
table(Internet.Poll$Internet.Use, Internet.Poll$Smartphone)

# 186 -> No Internet Use , No Smart Phone
# 17 -> No Internet Use , Smart Phone
# 285 -> Internet , No Smart Phone
# 470 -> Internet , Smart Phone
Internet.Poll$Internet.Use

limited = subset(Internet.Poll,Internet.Poll$Internet.Use=="1" | Internet.Poll$Smartphone == "1")
summary(limited)
colnames(limited)[colSums(is.na(limited)) > 0]
mean(Internet.Poll$Info.On.Internet,na.rm=T)

table(Internet.Poll$Worry.About.Info)[2]/
sum(table(Internet.Poll$Worry.About.Info))

mean(Internet.Poll$Anonymity.Possible,na.rm=T)
mean(limited$Tried.Masking.Identity,na.rm=T)

hist(limited$Age)

plot(limited$Age, limited$Info.On.Internet)
table(limited$Age)
table(limited$Info.On.Internet)
max(table(limited$Age,limited$Info.On.Internet))
sum(jitter(c(1, 2, 3)))

plot(jitter(limited$Info.On.Internet),jitter(limited$Age))
tapply(limited$Info.On.Internet,limited$Smartphone,summary) 
tapply(limited$Tried.Masking.Identity,limited$Smartphone,summary) 
