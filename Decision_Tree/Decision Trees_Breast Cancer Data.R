# Decision Tree Classification on Breast cancer dataset
# Downloading the file
library(RCurl)
fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="C:/APU/CT046-3-M-AML/CT046 - LABS/Lab 4 - Decision Trees/breast-cancer-wisconsin.data", method="libcurl")
# read the data
data <- read.table("breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(data)

# Remove ID column, col = 1
data <- data[,-1]
dim(data)

library(formattable)
formattable(data)
# Name the columns. 
# These names are displayed in the tree to facilitate semantic interpretation

names(data) <- c("ClumpThickness",
                 "UniformityCellSize",
                 "UniformityCellShape",
                 "MarginalAdhesion",
                 "SingleEpithelialCellSize",
                 "BareNuclei",
                 "BlandChromatin",
                 "NormalNucleoli",
                 "Mitoses",
                 "Class")

# Numerical values in the response variable are converted to labels
formattable(data)

data$Class <- factor(data$Class, levels=c(2,4), labels=c("benign", "malignant"))

summary(data)

# Proportions of the class values
table(data$Class)
prop.table(table(data$Class)) 
 
# Note that there are 16 missing values in BareNuclei
# Later you will see that there is no imputation of these missing values. 
# Investigate how decision trees handle missing values
# Read rpart documentation from this.
# This link has some extra information: 
# https://stats.stackexchange.com/questions/96025/how-do-decision-tree-learning-algorithms-deal-with-missing-values-under-the-hoo

# Dividing the dataset into training and validation sets. There are many ways to do this.
# Alternate method is also listed here.

set.seed(123)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
train_data <- data[ind==1,]
test_data <- data[ind==2,]
table(train_data$Class)
table(test_data$Class)

# Proportions of the class values
prop.table(table(train_data$Class)) 


# Alternate methods for data split
# Create training and testing sets
library(caTools)
set.seed(123)
split = sample.split(data$Class, SplitRatio = 0.7)
dataTrain = subset(data, split == TRUE)
dataTest = subset(data, split == FALSE)
table(dataTrain$Class)

 install.packages('rpart') #--> (Recursive Partitioning And Regression Trees) and the R implementation of the CART algorithm
 install.packages("rpart.plot")
 install.packages("party")
library(rpart)
library(rpart.plot)
library(party)

"Can generate different types of trees with rpart
Default split is with Gini index"

tree = rpart(Class~ ., data=train_data)
tree
prp(tree) # plot Rpart Model
prp (tree, type = 5, extra = 100)
rpart.plot(tree, extra = 101, nn = TRUE) # nn displays the node numbers

# DT using Party Package
c_tree = ctree(Class~ ., data=train_data)
c_tree
plot(c_tree)


# Split with entropy information
ent_Tree = rpart(Class ~ ., data=train_data, method="class", parms=list(split="information"))
ent_Tree
prp(tree) # GINI index
prp(ent_Tree) # ENTROPY index

plotcp(tree)
plotcp(ent_Tree)

# Here we use tree with parameter settings.
install.packages("mlr")
library(mlr)
getParamSet("classif.rpart")

# This code generates the tree with training data
tree_with_params = rpart(Class ~ ., data=train_data, method="class", minsplit = 1, minbucket = 10, cp = 0) # cp is the complexity parameter
prp (tree_with_params)
print(tree_with_params)
summary(tree_with_params)
plot(tree_with_params)
text(tree_with_params)
plotcp(tree_with_params)

# Now we predict and evaluate the performance of the trained tree model 
Predict = predict(tree_with_params, test_data)
# Now examine the values of Predict. These are the class probabilities
Predict

# pred <= predict (mymodel, dataset, type = 'prob')
# To produce classes only, without the probabilities, run the next command.
# By default threshold is set at 0.5 to produce the classes

Predict = predict(tree_with_params, test_data, type = "class")
Predict


# Producing confusion matrix
Confusion_matrix = table(Predict, test_data$Class)
Confusion_matrix

# Calculating the accuracy using the cofusion matrix
Accuracy = sum(diag(Confusion_matrix))/sum(Confusion_matrix)
Accuracy

# Performance of the DT model
library(caret)
confusionMatrix(Predict, test_data$Class)

# ROC curve
# install.packages("ROCR")
library(ROCR)
# install.packages("gplots")

# To draw ROC we need to predict the prob values. So we run predict again
# Note that PredictROC is same as Predict with "type = prob"

Predict_ROC = predict(tree_with_params, test_data)
Predict_ROC
Predict_ROC[,2]

pred = prediction(Predict_ROC[,2], test_data$Class)
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc


# Variable Importance
varImp(tree_with_params)

tree_with_params$variable.importance

