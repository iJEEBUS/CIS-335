# Ronald Rounsifer
# CIS 335-01
# Professor Scripps
# 02/19/2018
library("rpart", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rpart.plot", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

gini <- function(x, y) {
  return (1 - (length(x)/(length(x) + length(y)))^2 - (length(y)/(length(x) + length(y)))^2)
}

# Load data
x <- read.table("hw04dataAirline.txt", header = TRUE)

# Compute GINIs
gini_weather = gini(x[x[,2]=='n',2], x[x[,2]=='y', 2])
gini_time = gini(x[x[,3]=='m',3], x[x[,3]=='a', 3]) # largest gini
gini_airline = gini(x[x[,4]=='d',4], x[x[,4]=='u', 4]) # smallest gini


# QUESTION 3 - Building a decision tree
# a
data_training <- read.table("hw04dataTrain.txt", header = TRUE)
data_test <- read.table("hw04dataTest.txt", header = TRUE)

# b
# Remove the bench attribute bc it is more closely correlated to
# the other attributes than the military attribute

# c
fit <- rpart(team~age+height+weight+dash+military, method = "class", data=data_training)
rpart.plot(fit)

# d
predictions <- predict(fit, data_test)
