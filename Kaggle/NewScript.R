rm(list=ls())
library(randomForest)
library(caret)
library(doMC)
library(e1071)
library(caTools)
library(mice)
library(caret)
library(caretEnsemble)
require(xgboost)
registerDoMC(4)
setwd("/Users/krishna/MOOC/Edge/Kaggle/Imputes")
traindf = read.csv("imputeTrain.csv",na.strings=c("","NA"),sep='|')
testdf =  read.csv("imputeTest.csv",na.strings=c("","NA"),sep='|')

# Fix Education Level
levelsE=c("Current K-12", "High School Diploma", "Current Undergraduate","Associate's Degree", "Bachelor's Degree","Master's Degree", "Doctoral Degree")
traindf$EducationLevel = ordered(traindf$EducationLevel, levels = levelsE)
testdf$EducationLevel = ordered(testdf$EducationLevel, levels = levelsE)

# Fix Income
levelsI = c("under $25,000","$25,001 - $50,000","$50,000 - $74,999","$75,000 - $100,000","$100,001 - $150,000","over $150,000")
traindf$Income = ordered(traindf$Income, levels= levelsI)
testdf$Income = ordered(testdf$Income, levels= levelsI)

# Fix YOB as factor
traindf$YOB = as.factor(traindf$YOB)
testdf$YOB = as.factor(testdf$YOB)

# Fix Party
traindf$Party = ifelse(traindf$Party == "Republican", 1, 0)

features =c('YOB','Gender','Income',
            'HouseholdStatus','EducationLevel','Party',
            'Q109244',# Are you a feminist?
            'Q115611' # Do you personally own a gun?
            #'Q99480', # Did your parents spank you as a form of discipline/punishment?
            #'Q113181', # Do you meditate or pray on a regular basis?
            #'Q116953', # Do you like rules?
            #'Q120379', # Do you have (or plan to pursue) a Masters or Doctoral degree?
            #'Q111580' # As a teenager, do/did you have parents who were generally more supportive or demanding?
)

trainC = traindf[, (names(traindf) %in% features)]
testC = testdf[, (names(testdf) %in% features)]

##############################
gbmGrid <-  expand.grid(interaction.depth = c(1,2,3,4, 5),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

Tcontrol <- trainControl(method="cv", number=5,search ="random")
testmodel = train(Party ~ . ,
                  trControl=Tcontrol, data = trainC,
                  tuneGrid = gbmGrid,
                  method = "gbm"
)
max(testmodel$results$Accuracy)
testmodel$bestTune
plot(testmodel)

