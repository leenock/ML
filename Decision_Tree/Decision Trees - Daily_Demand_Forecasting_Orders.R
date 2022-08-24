data = read.csv("Daily_Demand_Forecasting_Orders.csv", sep = ";")
data
View(data)
str(data)
summary(data)

library(formattable)
formattable(data)

set.seed(123)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
train_data <- data[ind==1,]
test_data <- data[ind==2,]
table(train_data$Target_Total.orders)
table(test_data$Target_Total.orders)

library(rpart)
library(rpart.plot)
library(party)

"Can generate different types of trees with rpart
Default split is with Gini index"

tree = rpart(Target_Total.orders~ ., data=train_data)
tree
prp(tree) # plot Rpart Model
prp (tree, type = 5, extra = 100)
rpart.plot(tree, extra = 100, nn = TRUE)
printcp(tree)

# Split with entropy information
ent_Tree = rpart(Target_Total.orders ~ ., data=train_data, method="anova", parms=list(split="information"))
ent_Tree
prp(ent_Tree)


library(rpart.plot)
plotcp(tree)

# Here we use tree with parameter settings.
# This code generates the tree with training data
tree_with_params = rpart(Target_Total.orders~ ., data=train_data, method="anova", minsplit = 1, minbucket = 10, cp = 0)
prp (tree_with_params)
print(tree_with_params)
summary(tree_with_params)
plot(tree_with_params)
text(tree_with_params)
plotcp(tree_with_params)

# Now we predict and evaluate the performance of the trained tree model 
Predict = predict(tree, test_data)
# Now examine the values of Predict. These are the class probabilities
Predict
table(Predict, test_data$Target_Total.orders) 

# Evaluation Measure for Numeric TV
library(caret)
summary(Predict)
RMSE(Predict, test_data$Target_Total.orders)

# MSE = RMSE^2
MAE(Predict, test_data$Target_Total.orders)
varImp(tree)

# ~~~~~~~~~~~~~~~~~~~~ Producing confusion matrix
Confusion_matrix = table(Predict, test_data$Target_Total.orders)
Confusion_matrix

Accuracy = sum(diag(Confusion_matrix))/sum(Confusion_matrix)
Accuracy
