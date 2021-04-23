x <- c(44,NA,34,NA,78)
x
x * 3


y <- rnorm(100)
y
z <- rep(NA,1000)
z

my_data <- sample(c(y,z),100)
my_data

my_na <- is.na(my_data) # count the numbers of NA in the data
my_na

sum(my_na) # how many missing values are there in a dataframe in R










