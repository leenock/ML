# Remove Redundant Features ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(mlbench)
library(caret)
library(ggplot2)

data(PimaIndiansDiabetes)
str(PimaIndiansDiabetes)
View(PimaIndiansDiabetes)

# labeling the target variable values
PimaIndiansDiabetes$diabetes <- factor(PimaIndiansDiabetes$diabetes, levels=c("neg","pos"), labels=c("0", "1"))

# checking the class distribution of original data
table(PimaIndiansDiabetes$diabetes)
prop.table(table(PimaIndiansDiabetes$diabetes)) 
barplot(prop.table(table(PimaIndiansDiabetes$diabetes)), col = rainbow(3), 
        ylim = c(0, 1), main = "Class Distribution")


library(lattice)
library(grid)
library(UBL)

data_SMOTE = SmoteClassif(PimaIndiansDiabetes$diabetes ~ ., PimaIndiansDiabetes, C.perc = "balance")
table(data_SMOTE$diabetes)
prop.table(table(data_SMOTE$diabetes)) 
dim(data_SMOTE)


library(ROSE)
data_balanced_over = ovun.sample(diabetes ~., 
                                 PimaIndiansDiabetes, method = "over", 
                                 N = 1000, seed=123)$data
table(data_balanced_over$diabetes)
dim(data_balanced_over)

barplot(prop.table(table(data_balanced_over$diabetes)), col = rainbow(3), 
        ylim = c(0, 1), main = "Class Distribution")
