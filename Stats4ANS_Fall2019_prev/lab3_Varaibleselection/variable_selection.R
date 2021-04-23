rm(list = ls())
library(car)
library(MASS)

setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab3_Varaibleselection")
heifer_data <- read.csv("heifer_data.csv",header = T,as.is = T)
head(heifer_data)

####### Testing based procedure
lmod <- lm(BW ~ AgeD + WitHt + HipHt + Girth, data = heifer_data)
anova(lmod)

lmod <- update(lmod, .~. - WitHt )
summary(lmod)

lmod <- update(lmod, .~. -HipHt)
summary(lmod)

# Removal of two predictors do not cause reduction in fit
#lmod <- update (lmod, .~. -AgeD)
#summary(lmod)


######### Perform Backward Elimination, Forward Selection, and Stepwise Regression
######### Testing based procedure
######### fit1 and fit2 represent "extreme" models
library(MASS)
fit1 <- lm(BW ~ AgeD + WitHt + HipHt + Girth, data = heifer_data)
fit2 <- lm(BW ~ 1,data = heifer_data )

# Backward selection 
stepAIC(fit1,direction = "backward")

# Forward selection 
stepAIC(fit2,direction = "forward",scope = list(upper = fit1,lower = fit2))

# stepwise regression
stepAIC(fit2,direction = "both", scope = list(upper = fit1,lower = fit2))


########## Perform all possible regressions (aka all subset regressions)
########## Prints out best 4 models of each # of predictors

install.packages("leaps")
library(leaps)
allbw <- regsubsets(BW ~ AgeD + WitHt + HipHt + Girth, nbest = 4, data = heifer_data)
aprout <- summary(allbw)
with(aprout,round(cbind(which,rsq,adjr2,cp,bic),3)) 

plot(allbw)

# To get Mallow's Cp:
install.packages("locfit")
library(locfit)
aprout$cp

# To get Residual SE
aov(lmod)

# Evaluate Multi-Collinearity
vif(heifer_multicol) # variance inflation factors for each of the regressor







































