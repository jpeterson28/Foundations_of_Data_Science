# John Peterson 01-10-2017
# Capstone Project - Foundations of Data Science 
# Data Set: Census Income Data Set from UCI Machine Learning Respository (http://archive.ics.uci.edu/ml/datasets/Census+Income)


# Install Packages and Load Libraries
#install.packages('dplyr')
#library(dplyr)
#install.packages('tidyr')
#library(tidyr)
#install.packages('magrittr')
#library(magrittr)
install.packages('psych')
library(psych)
install.packages('e1071')
library(e1071)

# Load data set
rawData <- read.table("/Users/johnpeterson/Desktop/Foundations of Data Science/Capstone/Raw Data/adult.data", sep = ",", col.names = c("age", "workclass", "fnlwgt", "education", "education.num", "marital.status", "occupation", "relationship", "race", "sex", "capital.gain", "capital.loss", "hours.per.week", "native.country", "greater.less.than"))
rawData <- rawData[sample(nrow(rawData), 2000),]

# Data Review
dim(rawData)
head(rawData)

# Dummy Coding Categorical Values
categoricalIndex <- c("workclass", "education", "marital.status", "occupation", "relationship", "race", "sex", "native.country")

for(i in 1:length(categoricalIndex)){
  rawData <- cbind(rawData,dummy.code(rawData[,categoricalIndex[i]]))
}

# Dropping categorical index columns
rawData <- rawData[, !(colnames(rawData) %in% c(categoricalIndex))]

#Smote for dealing with unbalanced data, install DMwR package
install.packages('DMwR')
library(DMwR)
# Call table to see difference between factors (<=50K and >50K)
table(rawData$greater.less.than)
rawData <- SMOTE(greater.less.than ~ ., rawData, perc.over =300, perc.under = 135) #Update perc.over and perc.under based on sampling of rawData

# Subset Data into Train and Test
idxTrain <- sample(seq(1,nrow(rawData)), 0.70*nrow(rawData), replace = FALSE)
idxTest <- setdiff(seq(1,nrow(rawData)), idxTrain)
trainData <- rawData[idxTrain,]
testData <- rawData[idxTest,]

# SVM trainModel Creation
xTrain <- subset(trainData, select=-greater.less.than)
yTrain <- subset(trainData, select=greater.less.than)
yTrain <- as.factor(as.character(yTrain[,1]))

trainModel <- svm(xTrain,yTrain, type = "C-classification", scale = FALSE)
summary(trainModel)

xTest <- subset(testData, select=-greater.less.than)
yTest <- subset(testData, select=greater.less.than)
yTest <- as.factor(as.character(yTest[,1]))

# trainModel Prediction
pred <- predict(trainModel, xTest)
system.time(pred <- predict(trainModel, xTest))
            
table(pred,yTest)

# Tune Model
svm_tune <- tune(svm, train.x = xTrain, train.y = yTrain,
                       kernel="radial", type = "C-classification", ranges=list(cost=2^seq(-3,5), gamma=2^seq(-3,5)), scale = FALSE)

print(svm_tune)

# Update the model after tuning
trainModel_after_tune <- svm(xTrain,yTrain, type = "C-classification", kernel = "radial", cost=1, gamma=0.125, scale = FALSE)
summary(trainModel_after_tune)

# Update predictions
testPred <- predict(trainModel_after_tune, xTest)
trainPred <- predict(trainModel_after_tune, xTrain)

# Validation Tables
table(trainPred, yTrain)

table(testPred, yTest)

# Print test and train predictions to csv
# write.csv(testPred, file="testPred.csv")
# write.csv(trainPred, file="trainPred.csv")




