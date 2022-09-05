# Aim of this lab is:  To build ridge, lasso regression models using cross validation
# To observe how lambda-the regularization coefficient, affects the error
# To observe the relationship between lambda and attribute coefficients in each of the models
# To compare the performance of the two models

# It uses Boston housing data set to build a model to predict the price of houses

# Libraries Needed
library(mlbench)    # Required for caret, Boston data set
library(ggplot2)    # Plotting
library(caret)      # Sampling, cross validation
library(glmnet)     # regression models
library(tidyverse)  # for plotting
library(broom)      # for plotting coeffs


# Data - load data from mlbench
data("BostonHousing")
data <- BostonHousing
str(data)

# df <- data("BostonHousing")

# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train_data <- data[ind==1,]
test_data <- data[ind==2,]

# Go to help on glmnet. Note that glmnet uses matrices for x, y.
# converting data frame into matrix

# Method 1 - train data
x <- data.matrix(train_data[,1:13])
y <- train_data$medv


# Method 2
# x= model.matrix(medv~., train_data)[,-1]
# In the above expression where the formula is used "medv~., train_data", 
# in matrix conversion medv goes into first column, this is then removed by removing the 1st column "-1"
# y <- train_data$medv

# Build a Linear Regression model without any regularization
# For numerical prediction choose family = Gaussian, for classification family = binomial
reg <- glmnet(x, y, family="gaussian")
p <- predict(reg, x) # remember to use x and not train_data
rmse_reg <- sqrt(mean((train_data$medv-p)^2))
rmse_reg

# using test data
a <- data.matrix(test_data[,1:13])
p_2 <- predict(reg, a) 
rmse_reg_2 <- sqrt(mean((test_data$medv-p_2)^2))
rmse_reg_2


# Ridge regression - glmnet parameter alpha=0 for ridge regression
# glmnet by default chooses 100 lambda values that are data dependent
l_ridge <- glmnet(x, y, family="gaussian", alpha=0)
plot(l_ridge, xvar = 'lambda', label=T)


# From the plot note that the coefficients reduce when lambda increases. However all 13 attributes remain, none of them are dropped. 
# Now we need to find the best value for lambda. 
# This may be done using the built-in cross validation of cv.glmnet. Look up this function the value of k for CV
cv_out_ridge = cv.glmnet(x, y, family="gaussian", alpha = 0)
plot (cv_out_ridge)
names(cv_out_ridge)

# two lambda values may be noted. 'lambda.min', 'lambda.1se'- lambda for error within 1 standard deviation
lambda_min <- cv_out_ridge$lambda.min
lambda_min
lambda_1se<- cv_out_ridge$lambda.1se
lambda_1e


# Now let us plot the ridge regression output once again
plot(l_ridge, xvar = 'lambda', label=T)
abline(v = log(cv_out_ridge$lambda.1se), col = "red", lty = "dashed")
abline(v = log(cv_out_ridge$lambda.min), col = "blue", lty = "dashed")


# Now set lambda to one of these values and build the model
l_ridge_final <- glmnet(x, y, family="gaussian", lambda = lambda_1se, alpha=0)
coef(l_ridge_final)
plot(coef(l_ridge_final))


# alternate plot of the coeffs
coef(l_ridge_final) %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Top 25 influential variables") +
  xlab("Coefficient") +
  ylab(NULL)


# Prediction with training set
p1 <- predict(l_ridge_final, x) # remember to use x and not train_data
rmse_l_ridge_final <- sqrt(mean((train_data$medv-p1)^2))
rmse_l_ridge_final

# Convert test set attributes to a matrix, and predict target variable on test set and compute rmse


# Repeat the same with lasso regression ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cv_out_lasso = cv.glmnet(x, y, alpha = 1)
plot (cv_out_lasso)
names(cv_out_lasso)
# two lambda values may be noted. 'lambda.min', 'lambda.1se'- lambda for error within 1 standard deviation
lambda_min <- cv_out_lasso$lambda.min
lambda_min
lambda_1se<- cv_out_lasso$lambda.1se
lambda_1se

# Now let us plot the lasso regression output once again
l_lasso <- glmnet(x, y, family="gaussian", alpha=1)
plot(l_lasso, xvar = 'lambda', label=T)
abline(v = log (cv_out_lasso$lambda.1se), col = "red", lty = "dashed")
abline(v = log (cv_out_lasso$lambda.min), col = "blue", lty = "dashed")

# Now set lambda to one of these values and build the model
l_lasso_final <- glmnet(x, y, family="gaussian", lambda = lambda_1se, alpha=1)
coef(l_lasso_final)
plot(coef(l_lasso_final))

# Note the number variables for varying lambda
# Note the coeffs of lasso regression
# Finally predict using train and test set 
# compare the two models

