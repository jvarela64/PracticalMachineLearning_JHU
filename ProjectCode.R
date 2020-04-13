# Practical Machine Learning Course Project

# Starting by reading the training file and loading the library
training=read.csv("pml-training.csv")
testing=read.csv("pml-testing.csv")
library(caret)

# Removing names, X, timestamps and windows.
filterRegEx = "timestamp|window"
training$user_name = NULL
training$X = NULL
training=training[, -grep(filterRegEx, names(training))]

testing$user_name = NULL
testing$X = NULL
testing=testing[, -grep(filterRegEx, names(testing))]

# There are variables with a lot of NAs. If the majority of column is 
# NA or empty then we exclude it from the study.

rmvMajorityNAs = function(x) {
    indexToRemove = c()
    for (i in 1:length(x)) {
        if (mean(is.na(x[,i]) || x[,i] =="") > 0.95) {
            indexToRemove = c(indexToRemove, i)
        }
    }
    x=x[,-indexToRemove]
}

training=rmvMajorityNAs(training)
testing=rmvMajorityNAs(testing)

# Will divide the training set to 70% of training and 30% of test validation.
set.seed(412)
inTrain=createDataPartition(y=training$classe, p=0.7, list=FALSE)
trainingMain = training[inTrain, ]
trainingTestVal = training[-inTrain, ]

# Let's set up two models with random forest and gradient boosting.

#RFModel = train(classe ~., data=trainingMain, method="rf")
RFModel
plot(RFModel, main = "Random Forest Model")
# The model's overall accuracy is 0.989 with 27 variables.

# Testing the Random Forest with the out of sample test set
predictTestVal = predict(RFModel, trainingTestVal)
cmRFTraining = confusionMatrix(trainingTestVal$classe, predictTestVal)
cmRFTraining

# The accuracy is 0.9927 in this out-of-sample test. 

GBMModel = train(classe ~., data=trainingMain, method="gbm", verbose = FALSE)
GBMModel
plot(GBModel, main = "GBM Model")

# The accuracy for the final model is 0.957 with 150 trees and depth of 3.

predictTestValGBM = predict(GBMModel, trainingTestVal)
cmGBMTraining = confusionMatrix(trainingTestVal$classe, predictTestValGBM)
cmGBMTraining

# The accuracy is 0.9613 in this out-of-sample test. 

# Running the Test data in the picked RFModel
predictTest = predict(RFModel, testing)
