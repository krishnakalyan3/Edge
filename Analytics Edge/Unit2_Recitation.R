# Recitation
getwd()
NBA = read.csv("NBA_train.csv")
str(NBA)
# WIns
# Pts points
# Opt Points
# A attempted
# Successful field goals with 3 pointers
table(NBA$W,NBA$Playoffs)
NBA$PTSdiff = NBA$PTS -NBA$oppPTS
plot(NBA$PTSdiff,NBA$W)
WinsReg = lm(W~PTSdiff,NBA)
summary(WinsReg)

PointsReg =lm(PTS ~ X2PA +X3PA +FTA + AST + ORB + DRB + TOV + STL + BLK,NBA)
summary(PointsReg)
PointsReg$residuals
SSE = sum(PointsReg$residuals^2)
RMSE = sqrt(SSE/nrow(NBA))
RMSE
mean(NBA$PTS)
summary(PointsReg)

PointsReg2 =lm(PTS ~ X2PA +X3PA +FTA + AST + ORB + DRB + STL + BLK,NBA)
summary(PointsReg2)

PointsReg3 =lm(PTS ~ X2PA +X3PA +FTA + AST + ORB  + STL + BLK,NBA)
summary(PointsReg3)

PointsReg4 =lm(PTS ~ X2PA +X3PA +FTA + AST + ORB  + STL ,NBA)
summary(PointsReg4)

SSE4 = sum(PointsReg4$residuals^2)
RMSE4 = sqrt(SSE4/nrow(NBA))
SSE4
RMSE4

NBA_test = read.csv("NBA_test.csv")
PointsPrediction = predict(PointsReg4, NBA_test)
SSE = sum((PointsPrediction - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS)-NBA_test$PTS)^2)
R2 = 1 - SSE/SST
R2
RMSE = sqrt(SSE/nrow(NBA_test))
RMSE

