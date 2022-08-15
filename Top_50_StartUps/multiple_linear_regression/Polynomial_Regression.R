library(tidyverse)
library(caret)
library(ggplot2)

# Load the data
data("Boston", package = "MASS")
View(Boston)

# Split the data into training and test set
library(caTools)
set.seed(123)
split = sample.split(Boston$medv, SplitRatio = 0.8)
training_set = subset(Boston, split == TRUE)
test_set = subset(Boston, split == FALSE)
dim(training_set)
dim(test_set)

# Checking relationship between the IV and TV --> Linear
ggplot(training_set, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)

# Build the model
model <- lm(medv ~ poly(lstat, 5, raw = TRUE), 
            data = training_set)
summary(model)

# Checking relationship between the IV and TV --> Polynomial
ggplot(training_set, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))


# Predicting the Test set results
y_pred = predict(model, newdata = test_set)
y_pred
table(y_pred, test_set$medv)

library(caret)
summary(y_pred)
RMSE(y_pred, test_set$medv) # Root mean squared error
MAE(y_pred, test_set$medv) # Mean Absolute Error

