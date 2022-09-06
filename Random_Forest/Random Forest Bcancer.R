library(RCurl)
# Dataset
fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="breast-cancer-wisconsin.data", method="libcurl")
# read the data
df <- read.table("breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(df)
# Name the columns. 
# These names are displayed in the tree to facilitate semantic interpretation

df <- df [ ,-1]
df$V11 <- factor(df$V11, levels=c(2,4), labels=c("1", "2"))

# Removing columns with missing Values
sum (is.na(df))
colSums(sapply(df,is.na))
df$V7 <- NULL

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df$V11, SplitRatio = 0.7)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)
str(training_set)

# Model building
library(randomForest)
set.seed(123)
rf <- randomForest(V11~., data = training_set)
print(rf)
attributes(rf)

p1 <- predict(rf, training_set)
p1
cm_train <- table (p1, training_set$V11)
cm_train
train_accuracy = sum(diag(cm_train)/sum(cm_train))
train_accuracy

p2 <- predict(rf, test_set)
cm_test <- table(p2, test_set$V11)
cm_test
test_accuracy = sum(diag(cm_test)/sum(cm_test))
test_accuracy # Accuracy

plot(rf)

library(caret)
confusionMatrix(p1, training_set$V11) # Root mean squared error
confusionMatrix(p2, test_set$V11) # Mean Absolute Error


# In the plot black solid line for overall OOB error and the color lines, one for each class' error.

# Tuning mtry --> mtry: Number of variables randomly sampled as candidates at each split.
library(caret)
str(training_set)
tuneRF(training_set[ ,-9], training_set$V11,
      stepFactor=0.5,
      plot = TRUE,
      ntreeTry = 500,
      trace = TRUE,
      improve = 0.05)

rf1 <- randomForest(V11~.,data = training_set,
                   ntreeTry = 500,
                   mtry=2,
                   importance = TRUE,
                   proximity = TRUE)
print(rf1)

p1 <- predict(rf1, training_set)
cm1 <- table(p1, training_set$V11)
cm1
p2 <- predict(rf1, test_set)
cm2 <- table(p2, test_set$V11)

# Number of nodes for trees
hist(treesize(rf),
     main = "No. of nodes for trees",
     col = "green")

varImpPlot(rf)
importance(rf)
varUsed(rf) # which IV actually used to build the RF model


# Tune using Random Search ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
control = trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:8))
rf_random = train(V11~., data=training_set, method="rf", metric="Accuracy", tuneLength=15, tuneGrid=tunegrid, trControl=control)
print(rf_random)
plot(rf_random)
attributes(rf_random)

# Tune using Grid Search ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:8)) # mtry = sqrt(ncol(training_set))
rf_grid <- train(V11~., data=training_set, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_grid)
plot(rf_grid)
attributes(rf_grid)


# Tuning on mtry and ntree ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(mtry=c(1:8))
modellist <- list()
for (ntree in c(500, 1000, 1500, 2000, 2500)) {
   set.seed(123)
   fit <- train(V11~., data=training_set, method="rf", metric="Accuracy", trControl=control, tuneGrid = tunegrid, ntree=ntree)
   key <- toString(ntree)
   modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)

attributes(results)
