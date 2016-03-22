getwd()
setwd("/Users/krishna/MOOC/Edge/Data")

baseball = read.csv("baseball.csv")
str(baseball)
dim(baseball)
length(table(baseball$Year))
baseball =subset(baseball,Playoffs ==1)
str(baseball)
dim(baseball)
table(baseball$Year)
PlayoffTable = table(baseball$Year)
str(PlayoffTable)
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
baseball$NumCompetitors = PlayoffTable[baseball$Year]
head(baseball)

## 2.4
str(baseball)
subset(baseball, NumCompetitors ==8)
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
table(baseball$NumCompetitors)

## 3.1
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
sum(as.numeric(baseball$WorldSeries == 0))

# 3.2
model1 = glm(WorldSeries~Year,family=binomial(),baseball)
model2 = glm(WorldSeries~RS,family=binomial(),baseball)
model3 = glm(WorldSeries~RA,family=binomial(),baseball)
model4 = glm(WorldSeries~W,family=binomial(),baseball)
model5 = glm(WorldSeries~OBP,family=binomial(),baseball)
model6 = glm(WorldSeries~SLG,family=binomial(),baseball)
model7 = glm(WorldSeries~BA,family=binomial(),baseball)
model8 = glm(WorldSeries~RankSeason,family=binomial(),baseball)
model9 = glm(WorldSeries~OOBP,family=binomial(),baseball)
model10 = glm(WorldSeries~OSLG,family=binomial(),baseball)
model11 = glm(WorldSeries~NumCompetitors,family=binomial(),baseball)
model12 = glm(WorldSeries~ League,family=binomial(),baseball)
summary(model4)


# 4.1
model13 = glm(WorldSeries~  NumCompetitors + RankSeason +  RA +  Year,
              family=binomial(),baseball)
summary(model13)

# 4.2
cor(baseball$RA,baseball$Year)
cor(baseball$Year,baseball$RankSeason)    
cor(baseball$Year,baseball$NumCompetitors)
cor(baseball$RA,baseball$RankSeason)
cor(baseball$RA,baseball$NumCompetitors)
cor(baseball$RankSeason,baseball$NumCompetitors)
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])


# 4.3
model1A = glm( WorldSeries~Year,family=binomial(),baseball)
model1B = glm( WorldSeries~RA,family=binomial(),baseball)
model1C = glm( WorldSeries~RankSeason,family=binomial(),baseball)
model1D = glm( WorldSeries~NumCompetitors,family=binomial(),baseball)
model1E = glm( WorldSeries~RA+ Year,family=binomial(),baseball)
model1F = glm( WorldSeries~ Year+RankSeason,family=binomial(),baseball)
model1G = glm( WorldSeries~Year+NumCompetitors,family=binomial(),baseball)
model1H = glm( WorldSeries~RA+RankSeason,family=binomial(),baseball)
model1I = glm( WorldSeries~RA+NumCompetitors,family=binomial(),baseball)
model1J = glm( WorldSeries~RankSeason+NumCompetitors,family=binomial(),baseball)
op =c(model1A$aic,
model1B$aic,
model1C$aic,
model1D$aic,
model1E$aic,
model1F$aic,
model1G$aic,
model1H$aic,
model1I$aic,
model1J$aic)
which.min(op)
