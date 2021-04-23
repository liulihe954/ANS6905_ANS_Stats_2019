rm(list=ls())
# List is a vector whcih contains multiple datatypes

num_vector <- c(0.5,55,-10,6)
num_vector

tf <- num_vector < 1
tf

my_char <- c("My","name","is")


my_char <- paste(my_char,collapse = ":")
length(my_char)

c(my_char,"Anil")
name <- c("Anil")

paste(my_char,name,collapse=" ")


paste("Hello","World!",sep = " ")

paste(1:3,c("X","Y","Z","A"), sep = " ")


# vector and matrix have one data strcuture