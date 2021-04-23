rm(list=ls())
# matrix contain only a single class of data and dataframe contains many classes of data.
my_vector <- c(1:20)
my_vector
length(my_vector)

dim(my_vector) <- cr(1,20)
dim(my_vector)
class(my_vector)

# soometimes we have to convert the charater vector to numeric vector in R

# matrix, dataframe and lists are all the vectors of simply an atomic vector 
my_matrix2 <- matrix(1:20, nrow=4, ncol=5, byrow = T)
my_matrix2

patients <- c("Bill","Gina","Kelly","Sean")
patients
my_data <- data.frame(cbind(patients,my_matrix2))
my_data


# matrix contains only one type of data class if we try to combine character vector with numeric matrix, it forcefully coerces everythin

vector1 <- c(1:4)
vector_2 <- c("Bill","Gina","Kelly","Sean")
data.frame(cbind(vector_2,vector1))
cnames <- c("patient", "age", "weight", "bp", "rating", "test")

colnames(my_data) <- cnames
my_data

# to give a heading to the dataframe, first create a vector of column and give names to the dataframe in R

