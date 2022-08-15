# Multiple Linear Regression (MLR)~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Importing the data set
# include the directory of your dataset.
dataset = read.csv("50_Startups.csv")
head(dataset)
dim(dataset)
sum(is.na(dataset))
colSums(is.na(dataset))
View(dataset)

table(dataset$State)

# Splitting the data set into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
head(training_set)
dim(training_set)
dim(test_set)


# Fitting Multiple Linear Regression to the Training set
regressor1 = lm(formula = Profit ~ ., data = training_set)
summary(regressor1)

# Alternative method to get the model parameters
library(jtools)
summ(regressor1, confint = TRUE)

# Predicting the Test set results
y_pred = predict(regressor1, newdata = test_set)
y_pred
table(y_pred, test_set$Profit) # Comparing the predicted and actual value

# Building the optimal model using Backward Elimination ~~~~~~~~~~~~~~
regressor2 = lm(formula = Profit ~ R.D.Spend + Administration + 
                  Marketing.Spend + State, data = dataset)
summary(regressor2)

regressor3 = lm(formula = Profit ~ R.D.Spend + Administration + 
                  Marketing.Spend, data = dataset)
summary(regressor3)

regressor4 = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)
summary(regressor4)

regressor5 = lm(formula = Profit ~ R.D.Spend,
               data = dataset)
summary(regressor5)

# RMSE on test set
sqrt(mean((test_set$Profit-y_pred)^2))

library(caret)
summary(y_pred)
RMSE(y_pred, test_set$Profit) # Root mean squared error
MAE(y_pred, test_set$Profit) # Mean Absolute Error
