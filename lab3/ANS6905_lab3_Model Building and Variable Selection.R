##===============================================================================##
##                            0.Before the War                                   ## 
##===============================================================================##
### prepare the packages we need.
library(car);library(MASS);library(leaps);library(tidyverse):library(faraway)
## If you are missing something; uncomment the following line and fill in the pck name
# install.packages()
## Remember to set working directory
# setwd("/Users/liulihe95/Desktop/ANS6905_ANS_Stats_2019/lab3/")


#####
### prepare dataset packages we need.
Heifer_data <- read.csv("heifer_data.csv",header = T,as.is = T) %>% select(-Heifer)
str(Heifer_data) # predict BW

##===============================================================================##
##                        1.Detect multicollinearity (using vif)                 ## 
##===============================================================================##
# when two or more explanatory variables are moderately or highly correlated
# impacts both estimation and inference
# large changes in estimate & significance of β^hat when one or more explanatory variables are added or deleted

# calculate VIF by hand (take AgeD as an example)
lmod_full <- lm(BW ~ .,Heifer_data) # fit model
x = model.matrix(lmod_full) # get model matrix
Vif_AgeD = 1/(1-summary(lm(x[,2] ~ x[,-2]))$r.squared) # extract R-square calculation
Vif_AgeD 
# or we use fvif() unction from other package
library(faraway) # use packge
vif(lmod_full) # call funtion vif()
round(vif(lmod_full)[1],3) == round(Vif_AgeD,3) # same value

# Dealing with that...
# 1. collecting?
# 2. advanced methods: ridge regression or principle-component regression
# 3. **model respecification**: redefine or eliminate some explanatory variables

##===============================================================================##
##                    2.Select the "best" subset of predictors                   ## 
##===============================================================================##
# Besides that...
# Unnecessary predictors bring about noise to estimations, degree of fredom will be wasted.
# Collecting more data my cost time/money.

# Criteria
# 1. Maximum adjusted R sequare and minimum MSE (equivalent)
# 2. Mallows's Cp (Cp = p)
# 3. Lowest:  AIC (Akaike's information criterion) 
#            /BIC (Bayesian information criterion)
(lmod_full)

# One approach: given a criterion, fit all possible models, and then identify the best one
# k possible explanatory variables -> 2^k possible models
# 
# Alternative approach: 
#          sequential model selection (k(k+1)/2 out of 2^k models will be considered)
# Forward Selection // Backward Elimination // Stepwise Regression
# 









####### Testing based procedure
lmod <- lm(BW ~ AgeD + WitHt + HipHt + Girth, data = heifer_data)
anova(lmod)
summary(lmod)

lmod <- update(lmod, .~. - WitHt )
summary(lmod)

lmod <- update(lmod, .~. -HipHt)
summary(lmod)

# Removal of two predictors do not cause reduction in fit
lmod <- update (lmod, .~. -AgeD)
summary(lmod)


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
########## Prints out best 6 models of each # of predictors

install.packages("leaps")
library(leaps)
allbw <- regsubsets(BW ~ AgeD + WitHt + HipHt + Girth, nbest = 6, data = heifer_data)
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
vif(fit1) # variance inflation factors for each of the regressor







































