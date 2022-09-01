# Lab 6 Logistic Regression - Multinomial Classification

library(tidyverse)
library(caret)
library(nnet)

# Load the data
data("iris")
head(iris)
str(iris)

table(iris$Species)

# Split the data into training and test set
set.seed(123)
training.samples <- iris$Species %>% 
  createDataPartition(p = 0.8, list = FALSE)
train_data  <- iris[training.samples, ]
test_data <- iris[-training.samples, ]

cat("Dimesion of Training data: \n", dim(train_data))
cat("Dimesion of Testing data: \n", dim(test_data))

# Fit the model
model = nnet::multinom(Species ~., data = train_data)
# Summarize the model
summary(model)


# Make predictions
predicted.classes <- model %>% predict(test_data)
predicted.classes

library(caret)
confusionMatrix(table(predicted.classes, test_data$Species))
