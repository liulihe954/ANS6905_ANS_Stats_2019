rm(list = ls())
install.packages("car")
install.packages("MASS")
library(car)
library(MASS)


# set working directory
#setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab2_modelassumptions/")
setwd("C:/Users/liulihe/Desktop/applied_statitics_in_ANS/lab/lab2")
getwd()

# read the data from a file
lab2.1data <- read.csv("lab2.1_BHBA_Glucose.csv",header = T,as.is = T)
head(lab2.1data);dim(lab2.1data)
str(lab2.1data)

## Fit the regression model
ketosis.mod <- lm(BHBA_7_DIM ~ Glucose_7_DIM, # regression formula
                 data = lab2.1data) # data set

# summarize and print the results

summary(ketosis.mod) # show regression coefficient table/least square estimates
anova(ketosis.mod,type = "III")
confint(ketosis.mod,interval = "confidence")
anova(ketosis.mod)



# Residual Plots

# simple residuals vs predicted value
plot(fitted(ketosis.mod),residuals(ketosis.mod),col = "red")
abline(h = 0,lty = 4)
max(ketosis.mod$residuals)
which.max(residuals(ketosis.mod))

dev.off()
# standardized residuals vs predicted value
plot(fitted(ketosis.mod),rstandard(ketosis.mod),col = "red")
abline( h = 0,lty = 4)
max(rstandard(ketosis.mod))
which.max(rstandard(ketosis.mod))



# studentized residuals ( external studentized residuals /studentized deleted residuals) vs predicted value
plot(fitted(ketosis.mod), rstudent(ketosis.mod), col = "red")
abline(h = 0, lty = 2)
max(abs(rstudent(ketosis.mod)))
which.max(abs(rstudent(ketosis.mod)))

# unusual data diagnostics

# Leverage

hii_1 <- data.frame(hatvalues(ketosis.mod)) 
plot(hatvalues(hii_1))##??
#plot(hii_1)
head(hii_1)

# hat values are diagonal elements of hat matrix

# A little bit of matrix algebra

lab2.1data$mu <-  1
X <- cbind (lab2.1data$mu,lab2.1data$Glucose_7_DIM)
X

X_prime <- t (X)
X_prime

inverse <- X_prime %*%X
solve (inverse)

H = X %*%solve(inverse)%*%X_prime
H
hii <- data.frame(diag(H)) # hii (leverage) is the ith diagonal element of the hat matrix
head(hii)

# Outlier
library(car)
outlierTest(ketosis.mod)

# Removing unusual observations

ketosis.mod_new <- lm(BHBA_7_DIM ~ Glucose_7_DIM, # regression formula
                 subset = (1:93)[-c(29,58,92)],data = lab2.1data) # data set

summary(ketosis.mod_new) # No influential points, just an outlier
Anova(ketosis.mod_new, type = "III")


# Cook's distance

cd1 <- data.frame(cooks.distance(ketosis.mod))
plot(cooks.distance(ketosis.mod))
round(head(cd1),4)

# Formula for cook's distance

r2 <- as.matrix(ketosis.mod$residuals/sqrt(0.5527*(1- hii)))
r2
Di_new <- (r2*r2/2)*hii/(1 - hii)
round(head(Di_new),4)


# Checking Model assumptions (part-II)

rm(list = ls())

# set working directory
setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab2_modelassumptions/")


# read the data from a file
rm(list=ls())

lab2.data <- read.csv("Lab2.2_herd_trt_mastitis.csv",header = T,as.is = T)
head(lab2.data);dim(lab2.data)

lab2.data$herd <- as.factor(lab2.data$herd)
lab2.data$treatment <- as.factor(lab2.data$treatment)

values <- c(1,2,3)
lab2.data$herd <- values[lab2.data$herd]
##??
values <- c(1,2)
lab2.data$treatment <- values[lab2.data$treatment]
##??


## Fit the regression model

somatic.cells <- lm(scc ~ herd + treatment, # regression formula
                 data = lab2.data) # data set
summary(somatic.cells)
anova(somatic.cells)


# checking constant error variances

# simple residuals vs predicted value
plot(fitted(somatic.cells),residuals(somatic.cells),col = "red")
abline(h = 0,lty = 4)

#checking normality
qqnorm(somatic.cells$residuals,ylab ="Raw Residuals", xlab="Theoretical Quantiles")
qqline(somatic.cells$residuals, col=2,lwd =2)

#histogram of residuals
hist(somatic.cells$residuals)

# Shapiro_Wilk test
shapiro.test(somatic.cells$residuals)


# Box-cox transformation
# ML Methods

library(MASS)
boxcox(somatic.cells) # lambda = 0 then yt = log(y)

#logtransformed model
log_somatic.cells <- lm(log(scc)~herd + treatment,data = lab2.data)
summary(log_somatic.cells)
anova(log_somatic.cells)
library(car)
Anova(log_somatic.cells, type = "III")


###########compare plots for somatic_cell_count_data before vs after transformatio
dev.off()
par(mfrow=c(3,2))

plot(fitted(somatic.cells),residuals(somatic.cells),col = "red", ylab ="Raw Residuals", xlab= "Fitted Values")
abline(h = 0,lty = 4)

plot(fitted(log_somatic.cells),residuals(log_somatic.cells),col = "red", ylab =" Raw_Residuals", xlab = "Fitted Values" )
abline(h = 0,lty = 4)


qqnorm (somatic.cells$residuals,ylab ="Raw Residuals", xlab="Theoretical Quantiles")
qqline (somatic.cells$residuals, col=2,lwd =2)

qqnorm (log_somatic.cells$residuals,ylab ="Raw Residuals", xlab="Theoretical Quantiles")
qqline (log_somatic.cells$residuals, col=2,lwd =2)

hist (somatic.cells$residuals)
hist (log_somatic.cells$residuals)


















































































































































































































































































