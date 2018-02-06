##### A
x <- read.table("hw02dataTrain.txt", header = TRUE)
# age = ordinal, team = nominal
dash_mean <- mean(x[,5])
dash_sd <- sd(x[,5])
dash_three_sd <- dash_sd * 3
outliers <- x[x[,5] < (dash_mean - dash_three_sd) | x[,5] > (dash_mean + dash_three_sd),5]
dash_samples <- sample(x[,5], 30)
x$fast <- 'n'
x[x[,5]<14,9]<-'y'
correlations <- cor(x[2:7])
# Bench and military are very highly correlated

##### B
x <- read.table("hw02dataTrain.txt", header = TRUE)
weight_mean <- mean(x[,4], na.rm = TRUE)
x[is.na(x[,4]),4] <- weight_mean

##### C
x <- read.table("hw02dataTrain.txt", header = TRUE)
team_y <- mean(x[x[,8]=='y',4], na.rm = TRUE)
team_n <- mean(x[x[,8]=='n',4], na.rm = TRUE)
x[x[,8]=='n' & is.na(x[,4]),4] <- team_n 
x[x[,8]=='y' & is.na(x[,4]),4]<-team_y
