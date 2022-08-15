# Multiple Linear Regression~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Importing the data set
# include the directory of your dataset.
dataset = read.csv("50_Startups.csv")
head(dataset)
dim(dataset)
sum(is.na(dataset))
colSums(is.na(dataset))
View(dataset)# View the data set using the data viewer
str(dataset)

table(dataset$State)

# Encoding the categorical data
library(plyr)
dataset$State = revalue(dataset$State, c("California" = "1", "Florida" = "2", "New York" = "3"))
View(dataset)
head(dataset)
str(dataset)

# One Hot Encoding the categorical data (Alternative)
library(mltools)
library(data.table)
dataset$State = factor(dataset$State)
dataset1 = data.table(dataset)
dataset2 = one_hot(dataset1, cols = "auto")
View(dataset2)
str(dataset2)

# Splitting the data set into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
dim(training_set)
dim(test_set)


# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ ., data = training_set)
summary(regressor)
anova(regressor)

confint(regressor) # 95% CI for the coefficients
confint(regressor, level=0.99) # 99% CI for the coefficients
attributes(regressor)
regressor$coefficients # or coef(regressor)

plot(dataset$R.D.Spend, dataset$Profit, main="Scatterplot")
abline(regressor, col=2, lwd=3)

# Predicting the Test set results 95% CI by default
y_pred = predict(regressor, newdata = test_set, interval = "confidence", conf.level = 0.95)
y_pred
table(y_pred[1:10], test_set$Profit) # Comparing the predicted and actual value

library(caret)
summary(y_pred)
RMSE(y_pred, test_set$Profit) # Root mean squared error
MAE(y_pred, test_set$Profit) # Mean Absolute Error


