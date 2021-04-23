rm(list=ls())
try(dev.off(),silent = TRUE)
par(mfrow=c(1,2))
plot(airquality$Wind,airquality$Ozone,main="Ozone and Wind")
plot(airquality$Wind, airquality$Solar.R, main = "Ozone and Solar Radiation")

# When we have to clear the plotting area
try(dev.off(),silent=TRUE)
plot.new()
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0)) # outer margin area and margins
plot(airquality$Wind, airquality$Ozone, main = "Ozone and Wind")


# When we have Base_potiing_system in R
setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab1_regression/datasets_lab1")
try(dev.off(),silent=TRUE)
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
plot(airquality$Wind, airquality$Ozone, main = "Ozone and Wind")
plot(airquality$Solar.R, airquality$Ozone, main = "Ozone and Solar Radiation")

##################################################################

pollution <- read.csv("avgpm25.csv", colClasses = c("numeric", "character", "factor", "numeric", "numeric"))
head(pollution)
str(pollution)
high <- pollution$pm25[pollution$pm25>15]
low <- pollution$pm25[pollution$pm25<5]
ppm <- pollution$pm25
plot.new()
par(mfrow=c(1,1))

####### boxplot
boxplot(pm25 ~ region, data = pollution, col = "red")



################################################################################
a <- with(pollution,plot(latitude,pm25))
a

###
Output: We've seen that we can use a function call as an argument when calling another function. We'll do this again when we call plot with the arguments latitude and pm25 which are both from our data frame pollution. We'll call plot from inside the R command with which evaluates "an R expression in an environment constructed from data". We'll use pollution as the first argument to with and the call to plot as the second. This allows us to avoid typing "pollution$" before the arguments to plot, so it saves us some typing and adds to your base of R knowledge. Try this now.

###
clearPlot.R
clearplot.new()

## Regression Models Binary Outcomes
plot(c(3,23,29,55), c(0.5, 0.5, 1.0, 1.0), type='l', lwd=5, col="purple", col.lab="purple", ylim=c(0.25,1),
     xlab="Ravens' Score", ylab="Probability of a Ravens win", col.main="purple",
     main="Ravens' win vs score probabilities: GLM maximum likelihood estimates\ncompared to crude estimates.")
lines(mdl$data$ravenScore, mdl$fitted.values, lwd=5, col="black")
legend('bottomright', c("Crude estimates", "GLM maximum likelihood estimates"), lwd=5, lty=1,
       col=c("purple", "black"))

boxplot(ravenScore ~ ravenWin, ravenData, col=0x96, lwd=3, horizontal=TRUE, 
        col.lab="purple", col.main="purple", xlab="Ravens' Score", 
        main="Ravens' Wins and Losses vs Score")


### Residuals
data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e -(y - yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))

#### Residuals variation
local({
  set.seed(13121098)
  n <- 50
  x <- rnorm(n, sd=.5)
  y <- x + rnorm(n, sd=.3)
  out1 <<- data.frame(y=c(5,y), x=c(0,x))
  out2 <<- data.frame(y=c(0,y), x=c(5,x))
})

# Variance infaltion factor in R
head(swiss)

# 
mdl1 <- lm(Fertility ~.,swiss)
mdl1
vif(mdl1)

##
mdl2 <- lm(Fertility ~ . -Examination, swiss)
mdl2

install.packages("car")
library(car)
