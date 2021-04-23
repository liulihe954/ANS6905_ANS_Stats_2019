install.packages("UsingR")
library(UsingR)


alligator <- data.frame(lnlength=c(3.87,3.61,4.33,3.43,3.81,3.83,3.46,3.76,
                                   3.50,3.58,4.19,3.78,3.71,3.73,3.78),
                        lnWeight = c(4.87,3.93,6.46,3.33,4.38,4.70,3.50,4.50,
                                     3.58,3.64,5.90,4.48,4.43,4.42,4.25))
alligator

plot(lnWeight~lnlength,data=alligator,xlab="Snout length",ylab="Weights",main="CentralFlorida")

alligator.lm <- lm(lnWeight ~ lnlength, data = alligator)
alligator.lm

## This is important to look for the homoscedasticity 

plot(resid(alligator.lm) ~ fitted(alligator.lm),xlab="Fitted Values",ylab="Residuals",
     main = "Residual Diagnostic Plot")


## Normal qq plot
qqplot( ~ resid(alligator.lm),
        xlab = "Theoretical Quantiles",
        ylab = "Residuals"

        
install.packages("swirl")
library("swirl")
ls() # list of the variable saved in your workspace
rm(list=ls()) # You have to remove list of teh variablessaved in your workspace

install_course("Regression_Models")
swirl()
AnilSigdel



