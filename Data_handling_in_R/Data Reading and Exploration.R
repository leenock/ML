# Read a dataset from a Excel file
library(readxl)
mart = read_excel("C:/Users/leenock/OneDrive - Asia Pacific University/Desktop/Github/ML/Data_handling_in_R/AmazingMartEU2.xlsx", sheet = "OrderBreakdown")
View(mart)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~ Method 1 ~~~~~~~~~  
# Read a dataset from a CSV file
iris = read.csv('C:/Users/leenock/OneDrive - Asia Pacific University/Desktop/Github/ML/Data_handling_in_R/Iris.csv', header = T)
# print(df)
head(iris)
install.packages("DataExplorer")
library(DataExplorer)
plot_str(iris)

# ~~~~~~~~~ Method 2 ~~~~~~~~~ 
# Read dataset from a url
iris_1 <- read.table(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), sep = ',') 
head(iris_1)


# ~~~~~~~~~ Method 3 ~~~~~~~~~ 
# install.packages("RCurl") ~ A wrapper for 'libcurl' Provides functions to allow one to compose general HTTP requests and provides convenient functions to fetch URIs, get & post forms, etc.
library(RCurl)
fileURL = "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
download.file(fileURL, destfile="C:/APU/CT046-3-M-AML/CT046 - LABS/Lab 2 - Data Handling/iris.data", method="libcurl")
# read the data
data = read.table("C:/APU/CT046-3-M-AML/CT046 - LABS/Lab 2 - Data Handling/iris.data", sep = ',')

# Since the data downloaded from method 2 and 3 needs to be modified by setting 
# the variable names


# View dataset
dim(iris) # dimension
str(iris) # structure
head(iris) # first 6 records
head(iris, n=10) # first 10 records
summary(iris)


# install.packages("psych")
library(psych)
describe(iris)

# install.packages("pastecs")
library(pastecs)
stat.desc(iris)


# check for missing data in the data frame
is.na(iris)
sum (is.na(iris))
colSums(sapply(iris,is.na))



# ~~~~~~~~~ Dataset Exploration ~~~~~~~~~ 
# install.packages('DataExplorer')
library(DataExplorer)
library(dplyr)
plot_str(iris)

# % of missing values in each variable
plot_missing(iris) 


# ~~~~~~~~~~~ Data Exploration ~~~~~~~~~~~ 
plot_histogram(iris)
plot_density(iris)
plot_boxplot(iris, 'Species')

boxplot(SepalLengthCm ~ Species, data = iris, xlab = "Species",
        ylab = "SepalLengthCm", main = "[Suitable Title]")

plot_scatterplot(iris[-1], 'Species')
plot_scatterplot(iris,'SepalLengthCm')

plot_correlation(iris[-1],'continuous')
ds = iris [ , 2:6] # To store only the needed variables
head(ds)
plot_scatterplot(ds,"Species")

# Histogram ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
ggplot(iris, aes(x = SepalLengthCm)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", bins = 10) + 
  labs(title="Sepal Length",
       x = "Sepal Length (cm)") + geom_density(fill = "indianred3")

# Barplot
barplot(table(iris$Species), main="Species Distribution", col=c("skyblue","red", "lightgreen"))


#  ~~~~~~~~~ Data Manipulation ~~~~~~~~~ 
library(dplyr)
# drop a column
df<- select(iris, -Id)
# change col names to lowercase
ds <- df
colnames(ds) <- tolower(colnames(ds))
print(colnames(ds))


# ~~~~~~~~~ Split dataset ~~~~~~~~~ 
# install.packages('caTools') # https://www.rdocumentation.org/packages/caTools/versions/1.17.1
# Stratified sampling
library(caTools)
split = sample.split(df$Species, SplitRatio = 0.80)
training_set = subset(df, split == TRUE)
dim(training_set)
test_set = subset(df, split == FALSE)
dim(test_set)

# Checking if there is any bias in sampling ~~~~~~~~~~~~~~~~~
prop.table(table(training_set$Species))
prop.table(table(test_set$Species))


# random sampling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(123)
ind <- sample(2, nrow(df), replace = T, prob = c(0.80, 0.20))
train <- df[ind==1,]
test <- df[ind==2,]
prop.table(table(train$Species))
prop.table(table(test$Species))
