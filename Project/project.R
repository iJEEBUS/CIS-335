# You are consulting a company in order to help them improve
# the health of their employees most effectively. Make use the two datasets
# provided to conduct your analysis before submitting your final
# report on the time/places of coupon distributions.
# 
# In order to complete one must prepare/clean the dat
# 1 - Prepare/clean the data
# 2 - Explore the data using visualization tools
# 3 - Analyze the data using any ML techniques learning in class
#
# Methods used: Decision Tree Classifier, K-Nearest Neighbors
#
# Author: Ronald Rounsifer
# Date: 04.20.2018

## Init the correct folder so everything works correctly
rm(list = ls())
setwd("~/Desktop/School/Winter-2018/CIS-335/Project")
library("rpart", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rpart.plot", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("e1071")
library("naivebayes")
library("nnet")
library("rpart")
library("FNN")


# load in data
health = read.table("healthSurvey.txt", header = TRUE, sep=" ")
empls = read.table("emplData.txt", header = TRUE, sep = " ")

# Create BMI column and calculate BMI for all users
health$BMI <- 0
BMIs = health[,12]
heights  = health[,2]
weights = health[,3]
BMIs = (weights / (heights^2)) * 703
health$BMI <- BMIs

# Create and calculate BMI categories for all users
health$BMI_cat <- 0
health[health[,12] <= 24,13] = 1
health[health[,12] > 24 & health[,12] <= 29, 13] = 2
health[health[,12] > 29, 13] = 3
health$BMI_cat = as.factor(health$BMI_cat)

# Get rid of unneeded rows of data in health survey data
health = subset(health, select = c(fitbit, bike, eval, coach, lost, BMI_cat))

# Naive bayes stuff
d=ncol(health)
n=nrow(health)
index=sample(n) %% 10

decision_tree <- function() {
  fit = rpart(lost~., health[index!=0, ])
  pred = as.integer(predict(fit, health[index==0, ], type="vector"))
  
  rpart.plot(fit, type=4, extra=106)
  t=table(pred, health[index==0,5])
  t
  
  precision = (t[2,2]) / (t[2,2] + t[2,1])
  recall = (t[2,2]) / (t[2,2] + t[1,2])
  
  prec = paste("Precision:", precision, sep = " ")
  rec = paste("Recall:", recall, sep = " ")
  total = paste("Total:", (precision*recall), sep = " ")
  print(prec)
  print(rec)
  print(total)
}

naive <- function() {
  fit = naiveBayes(lost~., health[index!=0, ])
  pred = predict(fit, health[index==0, ])
  t=table(pred, health[index==0,5])
  t
  
  precision = (t[2,2]) / (t[2,2] + t[2,1])
  recall = (t[2,2]) / (t[2,2] + t[1,2])
  
  prec = paste("Precision:", precision, sep = " ")
  rec = paste("Recall:", recall, sep = " ")
  total = paste("Total:", (precision*recall), sep = " ")
  print(prec)
  print(rec)
  print(total)
}

support_vector_machine <- function() {
  fit = svm(health[index != 0, 5], health[index != 0, 5], scale = TRUE, kernel = "linear")
  pred = predict(fit, health[index==0,-d])
  t = table(pred, health[index==0,d])
  t
  
  precision = (t[2,2]) / (t[2,2] + t[2,1])
  recall = (t[2,2]) / (t[2,2] + t[1,2])
  
  prec = paste("Precision:", precision, sep = " ")
  rec = paste("Recall:", recall, sep = " ")
  print(prec)
  print(rec)
}


## Where to place the coupons

decision_tree_coupons <- function() {
  empls[,1] <- as.factor(empls[,1])
  empls[,2] <- as.factor(empls[,2])
  empls[,3] <- as.factor(empls[,3])
  empls[,4] <- as.factor(empls[,4])
  empls[,5] <- as.factor(empls[,5])
  empls[,6] <- as.factor(empls[,6])
  fit = rpart(lunchTime~., method = "class", data = empls)
  pred = as.integer(predict(fit, empls[index==0, ], type="vector"))
  
  rpart.plot(fit, type=4, extra=106)
  t=table(pred, empls[index==0,5])
  t
  
  precision = (t[2,2]) / (t[2,2] + t[2,1])
  recall = (t[2,2]) / (t[2,2] + t[1,2])
  
  prec = paste("Precision:", precision, sep = " ")
  rec = paste("Recall:", recall, sep = " ")
  total = paste("Total:", (precision*recall), sep = " ")
  print(prec)
  print(rec)
  print(total)
}




empls$healthy_BMI <- 0
empls$overweight_BMI <- 0
empls$obese_BMI <- 0

empls[empls[,1]==1, 6] <- 1
empls[empls[,1]==2, 7] <- 1
empls[empls[,1]==3, 8] <- 1
knn <- function() {
  
  total_withinss_vector <- c(1:25)
  
  for (k in 1:25) {
    sse = kmeans(empls, k+1)
    total_withinss_vector[k] = sse$tot.withinss
  }
  
  plot(total_withinss_vector, type = "l", main = "Total Within Sum of Squares for Various Number of Clusters", xlab = "Number of Clusters", ylab = "Total Within Sum of Squares")
  
  c2 = kmeans(empls, 6)
  
  plot(empls[,3],
       empls[,8],
       pch = c2$cluster,
       col=c2$cluster,
       xlab = "Lunch Time",
       ylab = "obese_BMI"
       )
  c2$centers
  #c2 # use this to view all available commands
}
#rpart.plot(fit)
