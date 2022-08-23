# Data Preprocessing
library(dplyr)
library(DataExplorer)
# Importing the dataset
dataset = read.csv('Data.csv')
dataset
str(dataset)

sum(is.na(dataset))
colSums(sapply(dataset,is.na))

dataset <- dataset %>% mutate(Country = replace(Country, Country == '', NA))
colSums(sapply(dataset,is.na))

plot_missing(dataset) 

# gives a very nice presentable/shareable rendered markdown in html
create_report(dataset)

# Taking care of missing data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)

dataset$Salary = ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                        dataset$Salary)
dataset

# Rounding all the numeric variables at once using dplyr library ~~~
d = dataset %>% mutate_at(vars(Age, Salary), funs(round(., 0)))
d

# Rounding all the numeric variables one by one ~~~~~~~~~~~~~~~~
dataset$Age <- round(dataset$Age) # Round off the column to integer
dataset
dataset$Salary <- round(dataset$Salary) # Round off the column to integer
dataset



# To filling/impute missing values based on the mode ~~~~~~~~~~~~``
dataset <- dataset %>% mutate(Country = replace(Country,is.na(Country),"Spain"))
dataset



# To remove the missing records ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
na.omit(dataset)

# To drop a column/variable
dataset <- drop_columns(dataset, "Country")

# To drop multiple columns/variables
dataset <- drop_columns(dataset, c("Country", "Age"))


# Encoding categorical data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dataset$Country = factor(dataset$Country,
                         levels = c('France', 'Spain', 'Germany'),
                         labels = c(1, 2, 3))
dataset$Purchased = factor(dataset$Purchased,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1))
dataset


# Export Data ~~~~~~~~~~~~~~~~~~~~~~~~ you can choose your destination location directory
write.csv('Data_New.csv', row.names = FALSE)
