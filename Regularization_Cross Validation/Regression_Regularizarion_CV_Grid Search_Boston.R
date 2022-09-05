# This lab includes application of Regularization, Cross Validation and Grid Search to a linear regression model
# It uses Boston housing dataset to build a model to predict the price of houses
# install.packages('glmnet')
# install.packages("mlbench")
# install.packages('psych')
# Libraries Needed
library(caret)
library(Matrix)
library(glmnet) # Lasso and Elastic-Net Regularized Generalized Linear Models
library(mlbench)
library(psych)

# Data - load data from mlbench
data("BostonHousing")
data <- BostonHousing
str(data)

# remove variables 4 , 14 - response variable medv, zoom text
# View correlation between attributes
# pairs.panels(data[c(-4,-14)], cex=2)

# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train_data <- data[ind==1,]
test_data <- data[ind==2,]


# ~~~~~~~~~~ The basic Linear Regression Model ~~~~~~~~~~
lm1 <- lm(medv ~., train_data) 
summary(lm1)

lm2 <- lm(medv ~ crim+zn+nox+rm+age+dis+rad+
           tax+ptratio+b+lstat, train_data)
summary(lm2)

pred = predict(lm2, test_data)
rmse = sqrt(mean((test_data$medv-pred)^2))
rmse

# Exercise 1
# compare lm1, lm2 
# comparison may be done by examining the summary of the models,
# testing their performance on the test data

# Exercise 2
# Continue eliminating features one by one based on their
# p value and check the performance of the model. 
# How would you decide on the final model?


# Custom Control Parameters
custom = trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)

# Fit a Linear Model with Cross Validation
set.seed(1234)
lm_cv <- train(medv ~.,
            train_data,
            method = 'lm',
            trControl = custom)
summary(lm_cv)

# Results
lm_cv$results
plot(lm_cv$finalModel)


# Ridge Regression
set.seed(1234)
ridge <- train(medv ~.,
               train_data,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 0,
                                      lambda = seq(0.001, 1, length = 5)),
               trControl = custom)

# Plot Results
plot(ridge)
print(ridge)
plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = 'dev', label=T)
plot(varImp(ridge, scale=T))
lridge=ridge$finalModel # Final model which can be used for prediction


# Lasso Regression
set.seed(1234)
lasso <- train(medv ~.,
               train_data,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 1,
                                      lambda = seq(0.001, 1, length = 5)),
               trControl = custom)

# Plot Results
plot(lasso)
print(lasso)
plot(lasso$finalModel, xvar = 'lambda', label=T)
plot(lasso$finalModel, xvar = 'dev', label=T)
plot(varImp(ridge, scale=T))
llesso=lasso$finalModel # Final model which can be used for prediction



# Elastic Net Regression
set.seed(1234)
en <- train(medv ~.,
            train_data,
            method = 'glmnet',
            tuneGrid = expand.grid(alpha = seq (0, 1, length = 10) ,
                                   lambda = seq(0.001, 1, length = 5)),
            trControl = custom)

# Plot Results
plot(en)
print(en)
plot(en$finalModel, xvar = 'lambda', label=T)
plot(en$finalModel, xvar = 'dev', label=T)
plot(varImp(en))
len=en$finalModel # Final model which can be used for prediction

# Compare Models
model_list <- list(LinearModel= lm_cv, Ridge = ridge, Lasso = lasso, Elasticnet = en)
res <- resamples(model_list)
summary(res)

# Best Model
en$bestTune
best <- en$finalModel
coef(best, s = en$bestTune$lambda)


saveRDS(en, "final_model.rds")
fm <- readRDS("final_model.rds")
print(fm)

# Prediction
p1 <- predict(fm, train_data)
sqrt(mean((train_data$medv-p1)^2))

p2 <- predict(fm, test_data)
sqrt(mean((test_data$medv-p2)^2))

