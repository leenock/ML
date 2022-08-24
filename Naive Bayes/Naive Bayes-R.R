# This is a very basic code. Please add comments, add new content into it

# install.packages("RCurl") ~ A wrapper for 'libcurl' Provides functions to allow one to compose general HTTP requests and provides convenient functions to fetch URIs, get & post forms, etc.
library(RCurl)
fileURL = "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="C:/APU/CT046-3-M-AML/CT046 - LABS/Lab 3 - Naive Bayes/breast-cancer-wisconsin.data", method="libcurl")

# read the data
df <- read.table("C:/APU/CT046-3-M-AML/CT046 - LABS/Lab 3 - Naive Bayes/breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(df)
dim(df)
View(df)


# These names are displayed in the tree to facilitate semantic interpretation
ds <- df[ , -1]
dim(ds)

# Name the columns. 
names(ds) <- c("ClumpThickness",
                 "UniformityCellSize",
                 "UniformityCellShape",
                 "MarginalAdhesion",
                 "SingleEpithelialCellSize",
                 "BareNuclei",
                 "BlandChromatin",
                 "NormalNucleoli",
                 "Mitoses",
                 "Class")

View(ds)

# Proportions of the class values
table(ds$Class) 
prop.table(table(ds$Class)) 

# Replacing the class levels with 0 & 1
ds$Class <- factor(ds$Class, levels=c(2,4), labels=c("0", "1"))
prop.table(table(ds$Class)) 

colSums(sapply(ds, is.na))

# Impute the missing values --> Pls do it as suitable


# Correlation among all the variables
corrTable <- cor(ds[,c("ClumpThickness","UniformityCellSize","UniformityCellShape","MarginalAdhesion","SingleEpithelialCellSize","BareNuclei","BlandChromatin","NormalNucleoli","Mitoses")])
corrTable 


# Remove the high correlated variables (either one)


set.seed(1234)
ind <- sample(2, nrow(ds), replace=TRUE, prob=c(0.7, 0.3))
train <- ds[ind==1,]
test <- ds[ind==2,]
dim(train)
dim(test)

# Running Naive Bayes using e1071 library
library(e1071)

# model building
classifier = naiveBayes(x = train[ ,-10], y = train$Class) 
summary(classifier)

# Test the classifier on training data 
y_pred_train = predict(classifier, newdata = train[ ,-10])
cm_1 = table(train$Class, y_pred_train)
cm_1

# Test the classifier on the test data
y_pred_test = predict(classifier, newdata = test[ ,-10])
cm_2 = table(test$Class, y_pred_test)
cm_2

# Laplace Smoothing ~~~~~~~~~~~~~~~~~
classifier_s = naiveBayes(x = train[ ,-10], y = train$Class, laplace = 1 )


# Using the classifier on training data to test the predictions
y_pred_train_raw = predict (classifier_s, newdata = train[ ,-10], type = "raw" ) # prob values
y_pred_train_class = predict (classifier_s, newdata = train[ ,-10], type = "class" ) # class predictions
y_pred_train_class

cm_s = table(train$Class, y_pred_train_class)
cm_s

# Calculating Precision, Recall & F1 Score ~~~~~~~~~~~~~~~~~~~
library(caret)
confusionMatrix(y_pred_train_class, train$Class)

P = precision(table(y_pred_train_class, train$Class))
P

precision(y_pred_train_class, train$Class, relevant = "1")
R = recall(table(y_pred_train_class, train$Class))
R

recall(y_pred_train_class, train$Class, relevant = "1")
F1_Score = 2*P*R/(P+R)
F1_Score


# Apply Laplace Smoothing with Threshold and EPS
classifier = naiveBayes(x = train[ ,-10], y = train$Class, laplace=1 )
y_pred_train_raw = predict (classifier, newdata = train[ ,-10], type = "raw", threshold = 0.001, eps = 0) # default values for eps & threshold
y_pred_train_raw

