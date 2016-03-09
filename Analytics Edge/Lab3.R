getwd()
setwd("/Users/krishna/MOOC/Edge/Data")
baseball = read.csv("baseball.csv")
str(baseball)
moneyball = subset(baseball,Year<2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)
plot(moneyball$RD,moneyball$W)
WinsReg = lm(W~RD,data = moneyball)
summary(WinsReg)
713-614
80.88 + .105*(99)

str(moneyball)
RunsReg = lm(RA ~OOBP+OSLG , moneyball)
summary(RunsReg)
OBP =.297
SLG = .370
Runs = 2913.60*(OBP) +   1514.29*(SLG) -837.38
Runs
sum(RunsReg$coefficients * c(1,OBP,SLG))
summary(moneyball)

str(moneyball)
model1 = lm(RS~OBP+SLG,moneyball)

#Eric
sum(model1$coefficients *c(1,0.338,0.540))
# Jeremy
sum(model1$coefficients *c(1,0.391,0.450))
# Frank
sum(model1$coefficients *c(1,0.369,0.374))
# Greg
sum(model1$coefficients *c(1,0.313,0.447))
# Carlos
sum(model1$coefficients *c(1,0.361,0.500))

mat =matrix(c(1,0.338,0.540,
              1,0.391,0.450,
              1,0.369,0.374,
              1,0.313,0.447,
              1,0.361,0.500), nrow = 3, ncol = 5)

op = t(model1$coefficients)
t(op)
mat
t(mat) %*% t(op)
