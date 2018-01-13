# List the files in the folder
list.files()

# Show the objects in the workspace
ls

# Create a vector of 100 elements with mean of 100 and std dev of 20
iq <- rnorm(100,100,20)

# Add ten to every element of iq
iq <- iq + 10

# Calculate the new mean and std dev
mean <- mean(iq)
std_dev <- sd(iq)

# Create a random vector of numbers
random_numbers <- sample(1:10)

# Create a vector
A <- c(3,4,7,2,1)

# Create a second vector
B <- c(1,0,0,1,1)

# Calculate a * b (not dot product)
AB = A * B

# Calculate the dot product of A and B
AB_dot = a %*% b

# Read the file into the matrix X
X = read.table("hw01data.txt",header=TRUE)

# Show the first row X
X[1,]

# Show only the second and third columns
X[,2:3]

# Calculate the column means of rows 1 - 4
colMeans(X[,1:4])

# Show just the value at row 25, column 3
X[25,3]