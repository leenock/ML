# Data Pre-processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Importing the dataset
iris = read.csv('Iris.csv')
str(iris)
iris = iris[-1]
head(iris)
summary(iris)

# install.packages('rcompanion')
library(rcompanion)
plotNormalHistogram(iris$SepalLengthCm)
plotNormalHistogram(iris$SepalWidthCm)
plotNormalHistogram(iris$PetalLengthCm)
plotNormalHistogram(iris$PetalWidthCm)


# Feature Scaling: Feature scaling is a method used to normalize the range of independent variables or features of data.
# ~~~~~~~~~~~~~~~~~`Transformation ~~~~~~~~~~~~~~~~~`
# Square root transformation
T_sqrt = sqrt(iris$SepalLengthCm)
plotNormalHistogram(T_sqrt)


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Min- Max Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

iris.norm <-as.data.frame(lapply(iris[0:3], normalize))
ds <- cbind(iris.norm, iris$Species)
str(ds)


# ~~~~~~~~~~~~~~~~~~~ Data Transformation ~~~~~~~~~~~~~~~~~~~~~~~~~~~
install.packages('rcompanion')
library(rcompanion)
Turbidity = c(1.0, 1.2, 1.1, 1.1, 2.4, 2.2, 2.6, 4.1, 5.0, 10.0, 4.0, 4.1, 4.2, 4.1, 5.1, 4.5, 5.0, 15.2, 10.0, 20.0, 1.1, 1.1, 1.2, 1.6, 2.2, 3.0, 4.0, 10.5)
plotNormalHistogram(Turbidity)

# Square root transformation
T_sqrt = sqrt(Turbidity)
plotNormalHistogram(T_sqrt)

# Cube root transformation
T_cub = sign(Turbidity) * abs(Turbidity)^(1/3)
plotNormalHistogram(T_cub)

# Log transformation
T_log = log(Turbidity)
plotNormalHistogram(T_log)
