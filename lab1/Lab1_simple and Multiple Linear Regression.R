rm(list=ls())
getwd() # where am I ?

# set working directory
setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab1_regression/datasets_lab1/")
setwd("C:/Users/liulihe/Desktop/applied_statitics_in_ANS/lab/lab1")

# read the data from a file
lab1.data <- read.csv("lab1.1.csv",header=T,as.is=T)
head(lab1.data);dim(lab1.data)
str(lab1.data)
lab1.data$Trt <- as.factor(lab1.data$Trt)
lab1.data$Diet <- as.factor(lab1.data$Diet)
class(lab1.data) # data.frame

## data for Simple LinearRegression
bw.data <- subset(lab1.data,select=c("DMI","BW"))


## Fit the regression model
DMI.mod <- lm(DMI~BW, # regression formula
              data= lab1.data) # data set

# summarize and print the results
summary(DMI.mod) # show regression coefficient table
coef(DMI.mod)
confint(DMI.mod,interval = "confidence")
anova(DMI.mod)

######################## estimate, SE, t value  #################################
slope_BW <- cor(lab1.data$DMI, lab1.data$BW) * sd(lab1.data$DMI) /  sd(lab1.data$BW)
slope_BW

se <- sqrt(diag(vcov(DMI.mod)))
se

t_value <- summary(DMI.mod)$coefficients[2,1] / summary(DMI.mod)$coefficients[2,2]
t_value

################################################################

### Examine the model objects

class(DMI.mod)
names(DMI.mod)
DMI.mod$df.residual # degree of freedom of residuals
round(sum(DMI.mod$residuals),2)
DMI.mod$fitted.values


###  Multiple linear Regression with categorical predictors

full.data <- read.csv("lab1.1.csv",header=T,colClasses = c("numeric","numeric","factor","factor"))
head(full.data);dim(full.data)
str(full.data)

## Fit the regression model
DMI.mod1 <- lm(DMI~ BW + Diet + Trt, # regression formula
               data= full.data) # data set
summary(DMI.mod1)
anova(DMI.mod1)

# Setting factor reference group and contrasts
DF <- within(full.data,Diet <-relevel(Diet,ref = "LC"))
DMI.mod2 <- lm(DMI ~ BW + Diet + Trt,data = DF)
summary(DMI.mod2)
anova(DMI.mod2)


## Full vs reduced model 

DMI.mod3 <- lm(DMI ~ Diet + Trt, data = full.data) # reduced model
summary(DMI.mod3)

# compare using the anova function
anova(DMI.mod1,DMI.mod3)


# Testing whether pedictors are orthogonal or not

DMI.mod4 <- lm(DMI ~   Trt+ BW + Diet,data = full.data)
summary(DMI.mod4)
anova(DMI.mod4)

library(car)
Anova(DMI.mod4,type ="III")



