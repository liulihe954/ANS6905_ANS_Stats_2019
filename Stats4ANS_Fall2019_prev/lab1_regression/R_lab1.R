rm(list = ls()) 
getwd() # where am I ?

# set working directory
setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab1_regression/datasets_lab1/")

# read the data from a file
lab1.data <- read.csv("lab1.1.csv",header = T,as.is = F)
head(lab1.data);dim(lab1.data)
str(lab1.data)
lab1.data$Trt <- as.factor(lab1.data$Trt)
lab1.data$Diet <- as.factor(lab1.data$Diet)
class(lab1.data) # data.frame

## data for Simple LinearRegression
bw.data <- subset(lab1.data,select = c("DMI","BW"))
bw.data

## Fit the regression model
DMI.mod <- lm(DMI~BW, # regression formula
              data = bw.data) # data set

# summarize and print the results

summary(DMI.mod) # show regression coefficient table/least square estimates

coef(DMI.mod)
confint(DMI.mod,interval = "confidence")
anova(DMI.mod)



######################## estimate, SE, t value  #################################
x <- model.matrix(~ BW,bw.data)
x
y <- bw.data$DMI
xtxi <- solve(t(x) %*% (x))
xtxi
beta_hat <- xtxi %*% t(x) %*% y
beta_hat

# more efficient way to calculate least square estimate
beta_hat1 <- solve(crossprod(x,x),crossprod(x,y))
beta_hat1

# Residual SE
sqrt(deviance(DMI.mod)/df.residual(DMI.mod))

# SE of estimates
se <- sqrt(diag(xtxi))*1.484
se

# t value estimate
t_value <- summary(DMI.mod)$coefficients[2,1] / summary(DMI.mod)$coefficients[2,2]
t_value

# Goodness of fit
R2 <- (cor(bw.data$BW,bw.data$DMI))^2
R2

### Examine the model objects

class(DMI.mod)
names(DMI.mod)
DMI.mod$df.residual # degree of freedom of residuals
round(sum(DMI.mod$residuals),2)
DMI.mod$fitted.values


###  Multiple linear Regression with continuous and categorical predictors

full.data <- read.csv("lab1.1.csv",header = T,colClasses = c("numeric","numeric","factor","factor"))
head(full.data);dim(full.data)
str(full.data)


## Fit the regression model
DMI.mod1 <- lm(DMI~ BW + Diet + Trt, # regression formula
               data = full.data) # data set
str(full.data)
summary(DMI.mod1)
anova(DMI.mod1)



## R -way of handling two levels of numeric vector:class and continuous 

# continuous
values <- c(1,2)
full.data$new_diet <- values[full.data$Diet]
head(full.data)
str(full.data)
DMI.mod_conti <- lm(DMI~ BW + new_diet + Trt, # regression formula
               data = full.data) # data set


summary(DMI.mod_conti)
anova(DMI.mod_conti)


# class 

full.data$new_diet <- as.factor(full.data$new_diet)
str(full.data)
DMI.mod_class <- lm(DMI~ BW + new_diet + Trt, # regression formula
               data = full.data) # data set
summary(DMI.mod_class)
anova(DMI.mod_class)


# Setting factor reference group and contrasts
DF <- within(full.data,Diet <- relevel( Diet,ref = "LC"))
DMI.mod2 <- lm(DMI ~ BW + Diet + Trt,data = DF)
summary(DMI.mod2)
anova(DMI.mod2)


## Full vs reduced model 
DMI.mod3 <- lm(DMI ~ BW + Diet , data = full.data) # reduced model
summary(DMI.mod3)

# compare using the anova function
anova(DMI.mod3,DMI.mod1)


# Testing whether pedictors are orthogonal or not
DMI.mod4 <- lm(DMI ~BW + Diet + Trt,data = full.data)
summary(DMI.mod4)
anova(DMI.mod4)

DMI.mod5 <- lm(DMI ~ Trt + Diet + BW, data = full.data)
anova(DMI.mod5)


install.packages("car")
library(car)
Anova(DMI.mod4,type = "III")





