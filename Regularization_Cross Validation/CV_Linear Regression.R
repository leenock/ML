# CV_Linear Regression 

library(tidyverse) # for easy data manipulation and visualization
library(caret) # for easily computing cross-validation methods

# Load the data
# built-in R swiss data for predicting fertility score on the basis of socio-economic indicators.
data("swiss")
# Inspect the data
View(swiss)
str(swiss)


# Split the data into training and test set
set.seed(123)
training.samples <- swiss$Fertility %>%
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- swiss[training.samples, ]
test.data <- swiss[-training.samples, ]


# Build the Linear Regression model
model <- lm(Fertility ~., data = train.data)
summary(model) # details of the model including the coefficients
print(model) # print only the coefficients


# Make predictions and compute the R2, RMSE and MAE
predictions <- predict(model, test.data)
table(predictions, test.data$Fertility)

# Error values
DF_model <- data.frame( R2 = R2(predictions, test.data$Fertility),
            RMSE = RMSE(predictions, test.data$Fertility),
            MAE = MAE(predictions, test.data$Fertility))
DF_model

# using the library(caret)
RMSE(predictions, test.data$Fertility) # Root mean squared error
MAE(predictions, test.data$Fertility) # Mean Absolute Error

# ~~~~~~~~~~~~~~~ Leave one out cross validation - LOOCV ~~~~~~~~~~~~~~~ 
# Leave out one data point and build the model on the rest of the data set
# Test the model against the data point that is left out at step 1 and record the test error associated with the prediction
# Repeat the process for all data points

# Define training control
train.control <- trainControl(method = "LOOCV")

model_LOOCV <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control, verboseIter = T)

# Performance Measures
print(model_LOOCV)

# Results
model_LOOCV$results
plot(model_LOOCV$finalModel)

# Make predictions and compute the R2, RMSE and MAE
predictions_LOOCV = predict(model_LOOCV, test.data)
table(predictions_LOOCV, test.data$Fertility)

RMSE(predictions_LOOCV, test.data$Fertility) # Root mean squared error
MAE(predictions_LOOCV, test.data$Fertility) # Mean Absolute Error

# ~~~~~~~~~~~~~~~ K-fold cross-validation ~~~~~~~~~~~~~~~ 
# Randomly split the data set into k-subsets (or k-fold) (for example 5 subsets)
# Reserve one subset and train the model on all other subsets
# Test the model on the reserved subset and record the prediction error
# Repeat this process until each of the k subsets has served as the test set.
# Compute the average of the k recorded errors. This is called the cross-validation error serving as the performance metric for the model.

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10, verboseIter = T)

model_kcv <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)

# Performance Measures
print(model_kcv)

# Results
model_kcv$results
plot(model_kcv$finalModel)

# Make predictions and compute the R2, RMSE and MAE
predictions_kcv <- predict(model_kcv, test.data)
table(predictions_kcv, test.data$Fertility)

RMSE(predictions_kcv, test.data$Fertility) # Root mean squared error
MAE(predictions_kcv, test.data$Fertility) # Mean Absolute Error


# ~~~~~~~~~~~~~~~ Repeated K-fold cross-validation ~~~~~~~~~~~~~~~ 
# The process of splitting the data into k-folds can be repeated a number of times, this is called repeated k-fold cross validation.
# The final model error is taken as the mean error from the number of repeats.
# The following example uses 10-fold cross validation with 3 repeats:

# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3, verboseIter = T)


model_rkcv <- train(Fertility ~., data = swiss, method = "lm",
                   trControl = train.control)

# Performance Measures
print(model_rkcv)

# Results
model_rkcv$results
plot(model_rkcv$finalModel)

# Make predictions and compute the R2, RMSE and MAE
predictions_rkcv <- predict(model_rkcv, test.data)
table(predictions_rkcv, test.data$Fertility)


# Compare Models
DF_model
print(model_LOOCV)
print(model_kcv)
print(model_rkcv)
