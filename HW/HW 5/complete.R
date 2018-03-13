## 1 
# Clear and load in the new data
rm(list = ls())
x <- read.table("dataProb1.txt", header = TRUE)

# Prior probabilities of being late or not
prob_yes <- length(x[x[,5]=='y',5]) / length(x[, 5]) 
prob_no <- length(x[x[,5]=='n',5]) /  length(x[, 5]) 

# Likelihoods for weather
prob_weather_not_late <- length(x[x[,2]=='n' & x[,5]=='n', 5]) / length(x[x[,5]=='n',5])
prob_weather_late <- length(x[x[,2]=='n' & x[,5]=='y', 5]) / length(x[x[,5]=='y',5])

# Likelihoods for time of day
prob_ToD_not_late <- length(x[x[,3]=='a' & x[,5]=='n', 5]) / length(x[x[,5]=='n', 5])
prob_ToD_late <- length(x[x[,3]=='a' & x[,5]=='y', 5]) / length(x[x[,5]=='y', 5])

# Likelihoods for airline
prob_delta_not_late <- length(x[x[,4]=='d' & x[,5]=='n',5]) / length(x[x[,5]=='n', 5])
prob_delta_late <- length(x[x[,4]=='d' & x[,5]=='y', 5]) / length(x[x[,5]=='y', 5])


## 2
# Clear and load in the new data
rm(list = ls())
x <- read.table("dataProb2.txt", header = TRUE)

# Priors
prob_positive <- length(x[x[,4]=='+',4]) / length(x[,4])
prob_negative <- length(x[x[,4]=='-',4]) / length(x[,4])

# Means and standard deviations of the positively classified data
pos_attr1_mean <- mean(x[x[,4]=='+',2])
pos_attr2_mean <- mean(x[x[,4]=='+', 3])
pos_attr1_sd <- sd(x[x[,4]=='+',2])
pos_attr2_sd <- sd(x[x[,4]=='+',3])

# Means and standard deviations of the negatively classified data
neg_attr2_mean <- mean(x[x[,4]=='-', 3])
neg_attr1_mean <- mean(x[x[,4]=='-', 2])
neg_attr1_sd <- sd(x[x[,4]=='-', 2])
neg_attr2_sd <- sd(x[x[,4]=='-', 3])