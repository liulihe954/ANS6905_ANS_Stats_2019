rm(list=ls())
getwd() # where am I ?

# set working directory
setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab1_regression")

# files in the dataset folder
list.files("datasets_lab2")

# read the data from a file
states.data <- readRDS("dataSets_lab1/states.rds") # interactive name of the file
dim(states.data)
head(states.data,10)

# Examine the data before fitting models
# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat) # what is the information we got from the summary

# correlation between expense and csat
cor(sts.ex.sat) # correlation between 1 and 1
                # spearman corrleation coefficient

# Plot the data before fitting the models
plot(sts.ex.sat)

# # Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set

# Summarize and print the results
summary(sat.mod) # show regression coefficients table
coef(summary(sat.mod))


# Examine the model object
class(sat.mod)
names(sat.mod)
sat.mod$coefficients
sat.mod$residuals
sat.mod$fitted.values
sat.mod$df.residual # degree of freedom of residuals
methods(class = class(sat.mod))[1:9]
cooks.distance(sat.mod)


# Use function methods to get more information about fit
confint(sat.mod) # how precise you estimate the coefficets of the data

hist(residuals(sat.mod)) # residuals are normally distributed

# Linear Regression assumptions
# Investiagate the assumptions visually by plotting the model
par(mar = c(4, 4, 2, 2), mfrow = c(2, 2)) #optional
plot(sat.mod, which = c(1, 2,3,4)) # "which" argument optional




# Multiple linear regressions

# Select one or more additional predictors to your model adn repeat steps 1 to 3.
# is the model better predicts the equation

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod)) # shows regression coefficient table


# Modeling interactions
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
sat.expense.by.percent

#Show the results
coef(summary(sat.expense.by.percent)) # show regression coefficients table


# Regression with categorical predictors
str(states.data$region)
states.data$region <- factor(states.data$region)

Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table 
anova(sat.region) # show ANOVA table # anova(model)

# Setting factor reference group and contrast
#The default in R is treatment contrasts, with the first level as the reference.
#We can change the reference group or use another coding scheme using the C function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))
     
     

?contrasts



####################### Logistic regression ###########################################
NH11 <- readRDS("dataSets_lab1/NatHealth2011.rds")
NH11
str(NH11$hypev)
levels(NH11$hypev) # for any factor variabel we cans see the levels
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
               data=NH11, family="binomial")
coef(summary(hyp.out)) # summary table of coefficents in log-odds form

# Lets transform the coefficents to make them easier to interpret
hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out)) # DO we have to change the SE
hyp.out.tab

































#list.files("TA_AppliedStatistics_Animal_Sciences")

#setwd("D:/TA_AppliedStatistics_Animal_Sciences")
#DMI_1 <- read.table("DMI_simple_lab1.1.csv", header = T, check.names = F,na.strings = -9,sep = ",")  
#DMI <- read.csv('DMI_simple.csv') ##creates a data set within R 

DMI.lm <- lm(DMI ~ BW, data = DMI_1) ##this is your linear model

summary(DMI_1) ##provides mean, range, etc. of your data set 
summary(DMI.lm) ##provides your estimates, SE, t-value, etc. for linear model 
anova(DMI.lm) 
plot(DMI)
plot(DMI.lm) 
