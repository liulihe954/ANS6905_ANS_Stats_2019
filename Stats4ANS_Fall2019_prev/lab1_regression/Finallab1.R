rm(list=ls())
getwd() # where am I ?

# set working directory
setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab1_regression")

# files in the dataset folder
list.files("datasets_lab1")

# read the data from a file
full.data <- read.csv("DMI_multiple.csv",header=T,as.is=F)
head(full.data);dim(full.data)
str(full.data)
full.data$Trt <- as.factor(full.data$Trt)
class(full.data) # data.frame

##  Pull data for Simple LinearRegression
bw.data <- subset(full.data,select=c("DMI","BW"))


## Fit the regression model
DMI.mod <- lm(DMI~BW, # regression formula
              data= bw.data) # data set

# summarize and print the results
summary(DMI.mod) # show regression coefficient table
coef(DMI.mod)
confint(DMI.mod,interval = "confidence")
anova(DMI.mod)

summary(DMI.mod)$coefficients[, 2]

######################## slope, SE, t value #################################
slope_BW <- cor(bw.data$DMI, bw.data$BW) * sd(bw.data$DMI) /  sd(bw.data$BW)
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

full.data <- read.csv("DMI_multiple.csv",header=T,colClasses = c("numeric","numeric","factor","factor"))
head(full.data);dim(full.data)
str(full.data)

## Fit the regression model
DMI.mod1 <- lm(DMI~ Trt + BW + Diet, # regression formula
              data= full.data) # data set
summary(DMI.mod1)
anova(DMI.mod1)
library(car)
Anova(DMI.mod1,type ="III")

## Full vs reduced model 
DMI.mod2 <- lm(DMI ~ Trt + Diet, data = full.data)
summary(DMI.mod2)

# compare using the anova function
anova(DMI.mod2,DMI.mod1)

# Modeling interactions
DMI_int <- lm(DMI ~ Trt + BW + Diet + Trt*Diet, data = full.data)
summary(DMI_int)


# Testing whether the pedictors are orthogonal or not

DMI.mod3 <- lm(DMI ~   Diet*Trt + BW + Diet + Trt,data = full.data)
summary(DMI.mod3)
anova(DMI.mod3)

library(car)
Anova(DMI.mod3,type ="III")

# Setting factor reference group and contrasts
DF <- within(full.data,Diet<-relevel(Diet,ref = "LC"))
DMI.mod3 <- lm(DMI ~   Trt+Diet*Trt + BW + Diet,data = DF)
summary(DMI.mod3)
anova(DMI.mod3)






















DMI.lm.full <- lm(DMI ~ BW + Diet + Trt, data = bodyweight) # linear covariate 
summary(DMI.lm.full)
anova(DMI.lm.full)
m1 <- anova(DMI.lm.full)

DMI.lm.reduced <- lm(DMI ~ BW + Trt, data = bodyweight)
summary(DMI.lm.reduced)
m2 <-anova(DMI.lm.reduced)

library(cars)
anova(DMI.lm.reduced,DMI.lm.full,test="Chisq") # reduction in the residual sum of square is sttaistically significant or not
