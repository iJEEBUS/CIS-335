# Ronald Rounsifer
# CIS 335-01
# Professor Scripps
# 02/05/2018

setwd("~/Desktop/School/Winter-2018/CIS-335/HW/HW 3")

##### 1
numbers <- c(45, 49, 37, 51, 39)
cat("\nNumbers: ", numbers)
normalized <- (numbers-min(numbers))/(max(numbers) - min(numbers))
cat("\nNormalized Numbers: ", normalized)

##### 2
x <- read.csv("hw03data.csv", sep=',', header = TRUE)
age_salary_means <- colMeans(x[,1:2])
participant_means <- colMeans(x[x[,3]=='y',1:2])
not_participant_means < -colMeans(x[!x[,3]=='y',1:2])

##### 3
covariance_matrix <- cov(x[,1:2])
correlation_matrix <- cor(x[,1:2])
sd_salary <- sd(x[,2])
cat("\nStandard Deviation of salary: ", sd_salary)
cat("\n")
cat("\n")


##### 4
plot(x)

hist(x[x[,3]=='y',1], 
     main = "Frequency of Ages of Participants", 
     xlab = "Age", 
     ylim = c(0,200))
hist(x[!x[,3]=='y',1], 
     main = "Frequency of Ages of Non-Participants", 
     xlab = "Age", 
     ylim = c(0,200))
hist(x[x[,3]=='y',2], 
     main = "Frequency of Salaries of Participants", 
     xlab = "Salary", 
     ylim = c(0,200))
hist(x[!x[,3]=='y',2], 
     main = "Frequency of Salaries of Non-Participants", 
     xlab = "Salary", 
     ylim = c(0,200))

boxplot(x[x[,3]=='y', 1], x[!x[,3]=='y',1])

boxplot(x[x[,3]=='y',2], x[!x[,3]=='y',2])

# The age variable appears to separate the data for paticipation a lot better
# as it is is easy to see that those who are younger tend to not be competing.