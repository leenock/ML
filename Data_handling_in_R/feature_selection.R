# Remove Redundant Features ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(mlbench)
library(caret)
library(ggplot2)

data(PimaIndiansDiabetes)
str(PimaIndiansDiabetes)
# set.seed(7)

correlationMatrix <- round(cor(PimaIndiansDiabetes[,1:8]), 2)
print(correlationMatrix)

library(reshape2)
melted_cormat <- melt(correlationMatrix)
head(melted_cormat)

ggheatmap = ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)

# find attributes that are highly corrected (ideally > 0.8)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.8)
print(highlyCorrelated)


# Rank Features By Importance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# Learning Vector Quantization (LVQ) model
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)

importance <- varImp(model, scale=FALSE)

print(importance)
plot(importance)


# Feature Selection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Recursive Feature Elimination or RFE.
# install.packages("randomForest")

library(randomForest)
require(caTools)

control <- rfeControl(functions=rfFuncs, method="cv", number=10)

results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
print(results)

predictors(results)

plot(results, type=c("g", "o"))
