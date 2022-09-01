# Lab 6 Logistic Regression - Binary Classification
# Objective 1 - Perform Logistic Regression with and without pre-processing and compare the results.
# Objective 2 - Perform various data normalization operations using "caret " package and compare results
# Objective 3 - Compute sensitivity, specificity and AUC.  draw ROC curve for one of teh chosen result 

library(RCurl)
fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="D:/Before_upload/AML/breast-cancer-wisconsin.data", method="libcurl")
# read the data
df <- read.table("D:/Before_upload/AML/breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(df)
View(df)

# Name the columns. 
# These names are displayed in the tree to facilitate semantic interpretation

library(DataExplorer)
library(ggplot2)
library(data.table)

df <- df [ ,-1] # drop V1
df$V11 <- factor(df$V11, levels=c(2,4), labels=c("0", "1"))
table(df$V11)

colSums(is.na(df))

# Splitting the data set into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df, SplitRatio = 0.7)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)
dim(training_set)
dim(test_set)



# Checking Class distribution
prop.table(table(df$V11))
prop.table(table(training_set$V11))
prop.table(table(test_set$V11))

# Building classifier
logit_classifier = glm(V11 ~., training_set, family = binomial)
summary(logit_classifier)

coef(logit_classifier)
confint(logit_classifier)
anova(logit_classifier, test="Chisq")

library(jtools)
summ(logit_classifier)

# While no exact equivalent to the R2 of linear regression exists, 
# the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(logit_classifier)

# test the model significance
library(aod)
wald.test(b = coef(logit_classifier), Sigma = vcov(logit_classifier), Terms = 2:3)


# Predicting the Test set results
prob_pred = predict(logit_classifier, type = 'response', test_set[ ,-10] )
prob_pred

y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred

cm = table(test_set$V11, y_pred)
cm

# Calculating the accuracy using the confusion matrix
Accuracy = sum(diag(cm))/sum(cm)
Accuracy

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Variation 1 - Run the above by removing the variable with missing values
# Checking for missing values
sum(is.na(df))
colSums(is.na(df))
# Drop a column with many missing values
df$V7 <- NULL

# data split
set.seed(123)
split = sample.split(df, SplitRatio = 0.7)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)
dim(training_set)
dim(test_set)

classifier = glm(V11 ~., training_set, family = binomial)
summary(classifier)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', test_set[ ,-9] )
y_pred = ifelse(prob_pred > 0.5, 1, 0)
cm = table(test_set$V11, y_pred)
cm

# Calculating the accuracy using the confusion matrix
Accuracy = sum(diag(cm))/sum(cm)
Accuracy


# Q1: Closely examine cm and comment
# Q2: Variation 2 - Run the Expt by imputing the missing values. comment on this experiment

# Variation 3 - Run the experiment after normalizing the values


# scaling~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This instruction only scales the attributes while retaining the class variable as it is
training_set[ ,1:8] = scale(training_set[ , 1:8])
test_set[ ,1:8] = scale(test_set[ , 1:8])

classifier = glm(V11 ~., training_set, family = binomial)
summary(classifier)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', test_set[ ,-9] )
y_pred = ifelse(prob_pred > 0.5, 1, 0)
cm = table(test_set$V11, y_pred)
cm

# Calculating the accuracy using the confusion matrix
Accuracy = sum(diag(cm))/sum(cm)
Accuracy

# Q3: Perform scaling with "preProcess_normalized" command in caret.
# Q4: Choose 2 different options of normalization and observe the output
# Q5: Compute sensitivity, specificity and AUC.  draw ROC curve for one of the chosen result 
# Q6: Compare performance with Naive Bayes & Classification Tree (DT)
