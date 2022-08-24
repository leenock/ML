library(DataExplorer)
library(dplyr)

data = read.csv("Naive Bayes/diabetes.csv") # include the file path directory of the diabetes.csv file
data
plot_missing(data)

# Explore the variables
plot_str(data)
str(data)
dim(data)
View(data)

# View the data in table format
# install.packages("formattable") This helps to view the data in table format.
library(formattable)
formattable(data) # Check the results on the right side Viewer

library(psych)
describe(data)

# While analyzing the structure of the data set, we can see that the minimum values for Glucose, Bloodpressure, Skinthickness, Insulin, and BMI are all zero. This is not ideal since no one can have a value of zero for Glucose, blood pressure, etc. Therefore,  such values are treated as missing observations.
data[, 2:6][data[, 2:6] == 0] <- NA

# check for missing data in the data frame
sum (is.na(data))
sum(is.na(data$Glucose))
colSums(sapply(data,is.na))
plot_missing(data)


# Setting outcome variables as categorical
data$Outcome <- factor(data$Outcome, levels = c(0,1), labels = c("No", "Yes"))

# ~~~~~ Imputation using MICE package ~~~~~~~~~~~~~~~
# install.packages("mice")
library(lattice)
library(mice)
mice_mod <- mice(data[, c("Glucose","BloodPressure","SkinThickness","Insulin","BMI")], method='rf')
mice_complete <- complete(mice_mod)

#Transfer the predicted missing values into the main data set
data$Glucose <- mice_complete$Glucose
data$BloodPressure <- mice_complete$BloodPressure
data$SkinThickness <- mice_complete$SkinThickness
data$Insulin<- mice_complete$Insulin
data$BMI <- mice_complete$BMI

plot_missing(data)

# install.packages('GGally') FYI
library(GGally)
ggpairs(data)

prop.table(table(data$Outcome)) * 100

# split data into training and test data sets
library(caret)
set.seed(123)
indxTrain <- createDataPartition(y = data$Outcome, p = 0.70, list = FALSE)
train <- data[indxTrain,]
test <- data[-indxTrain,]
dim(train)
prop.table(table(train$Outcome)) * 100
dim(test)
prop.table(table(test$Outcome)) * 100

# Assigning IV and DV
x = train[,-9]
y = train$Outcome

# Building a model
library(e1071)
# Model building using Cross Validation
model = train(x, y, 'nb', metric="Accuracy", trControl=trainControl(method='cv', number=10, classProbs=TRUE))
model

# Model building using without Cross Validation
classifier = train(x, y, 'nb', metric="Accuracy")
classifier

# Model Evaluation
# Predict testing set from "model"
Predict_model <- predict(model, test)

# Predict testing set from "classifier"
Predict_classifier <- predict(classifier, test)

# Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(Predict_model, test$Outcome)
confusionMatrix(Predict_classifier, test$Outcome)

# Plot Variable performance
plot(varImp(model))
plot(varImp(classifier))

# Calculating Precision and Recall
P = precision(table(Predict_model, test$Outcome))
P
precision(Predict_model, test$Outcome, relevant = "Yes")
precision(Predict_model, test$Outcome, relevant = "No")

R = recall(table(Predict_model, test$Outcome))
R
recall(Predict_model, test$Outcome, relevant = "Yes")
recall(Predict_model, test$Outcome, relevant = "No")

F1_Score = 2*P*R/(P+R)
F1_Score


# ~~~~~~~~~~~~~ SOME Evaluation Measures ~~~~~~~~~~~~~~~~~~
# install.packages("MLmetrics")
library(MLmetrics)

accuracy(Predict_model, test$Outcome)

confusionMatrix(Predict_model, test$Outcome)

Precision(test$Outcome, Predict_model)
Precision(Predict_model, test$Outcome, positive = "No")
Precision(Predict_model, test$Outcome, positive = "Yes")

Recall(Predict_model, test$Outcome, positive = "No")
Recall(Predict_model, test$Outcome, positive = "Yes")

F1_Score(Predict_model, test$Outcome)
F1_Score(Predict_model, test$Outcome, positive = "No")
F1_Score(Predict_model, test$Outcome, positive = "Yes")



