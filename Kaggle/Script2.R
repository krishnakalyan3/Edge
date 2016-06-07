rm(list=ls())
library(randomForest)
library(caret)
library(doMC)
library(e1071)
library(caTools)
library(neuralnet)
library(mice)
library(caret)
library(nnet)
library(caretEnsemble)
registerDoMC(4)
setwd("/Users/krishna/MOOC/Edge/Kaggle/Imputes")

traindf = read.csv("imputeTrain.csv",na.strings=c("","NA"),sep='|')
testdf =  read.csv("imputeTest.csv",na.strings=c("","NA"),sep='|')

# TODO
# Convert Every thing to Numerical Factors
# YOB to 6 Cateogries
# age groups below - below 20 - 20-29 - 30-39 - 40-49 - 50-59 - 60+
# Questions 


# Fix Education Level
levelsE=c("Current K-12", "High School Diploma", "Current Undergraduate","Associate's Degree", "Bachelor's Degree","Master's Degree", "Doctoral Degree")
traindf$EducationLevel = ordered(traindf$EducationLevel, levels = levelsE)
testdf$EducationLevel = ordered(testdf$EducationLevel, levels = levelsE)

# Fix Income
levelsI = c("under $25,000","$25,001 - $50,000","$50,000 - $74,999","$75,000 - $100,000","$100,001 - $150,000","over $150,000")
traindf$Income = ordered(traindf$Income, levels= levelsI)
testdf$Income = ordered(testdf$Income, levels= levelsI)

# Fix YOB as factor
#traindf$YOB = (2013 -traindf$YOB)
#testdf$YOB = (2013 - testdf$YOB)
#traindf$YOB <- cut(traindf$YOB, breaks=c(0, 20, 25,32,42,56))
#testdf$YOB <- cut(testdf$YOB, breaks=c(0, 20, 25,32,42,56))
traindf$YOB = as.factor(traindf$YOB)
testdf$YOB = as.factor(testdf$YOB)

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

features1 =c('YOB','Gender','Income',
            'HouseholdStatus','EducationLevel','Party','USER_ID',
            'Q109244','Q115611')

trainC = traindf[, !(names(traindf) %in% features1)]
testC = testdf[, !(names(testdf) %in% features1)]
trainC$Tag = "train"
testC$Tag ="test"
alldata = rbind(trainC,testC)
names(alldata)
library('mclust')
library(cluster)
library(cluster)
clusGap(alldata[,-100], kmeans, 10, B = 100, verbose = interactive())



km$tot.withinss
plot(alldata[,-100],col=km$cluster,pch=19)
par(mar=c(1,1,1,1))

#################### Default Model #############################
gbmGrid <-  expand.grid(interaction.depth = c(1),
                        n.trees = 50:280,
                        shrinkage = c(0.1),
                        n.minobsinnode = c(10:20))



Tcontrol <- trainControl(method="cv", number=10,search ="random")
testmodel = train(Party ~     EducationLevel*Income + . ,
                  trControl=Tcontrol, data = trainC, method=c('widekernelpls')
                  )
max(testmodel$results$Accuracy)
testmodel$bestTune
plot(testmodel)

######################## Ensemble ##############################
model_list <- caretList(
  Party ~ . , data=trainC,
  methodList= c('gbm','knn','widekernelpls','rpart','dnn'),
  trControl=trainControl(savePredictions="final",verboseIter = T,
                         classProbs=TRUE)
)
modelCor(resamples(model_list))

glm_ensemble <- caretStack(
  model_list
)
print(glm_ensemble)







#################### Submission ##############################
pred = predict(testmodel,testC)
op =cbind.data.frame(testdf$USER_ID  ,pred)
colnames(op) <-  c("USER_ID","Predictions")
write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)



######### % OF questions unanswered
unanswered <- function(data){
  op = table(data)
  unans = op[2]/sum(op)
  return(unans)
}
unanswered(traindf[,'Q116881'])
#unanswered(traindf$Q115777)
