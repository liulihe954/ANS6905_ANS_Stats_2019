##===============================================================================##
##                            0.Before the War                                   ## 
##===============================================================================##
### prepare the packages we need.
library(car);library(MASS);library(leaps);library(tidyverse);library(faraway);library(locfit)

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

# check the model and the veriables
summary(lmod_full)
Anova(lmod_full,type = 3)
round(cor(Heifer_data[,-5]),2) 
# check cor coef, so high...Additional predictors offer no mre explanatory effect when some are already included


# Besides that...
# Unnecessary predictors bring about noise to estimations, degree of fredom will be wasted.
# Collecting more data my cost time/money.

##===============================================================================##
##                    2.Select the "best" subset of predictors                   ## 
##===============================================================================##
# Criteria
# 1. Maximum adjusted R sequare and minimum MSE (equivalent)
# 2. Mallows's Cp (Cp = p)
# 3. Lowest:  AIC (Akaike's information criterion) 
#            /BIC (Bayesian information criterion)


# One approach: given a criterion, fit all possible models, and then identify the best one
# k possible explanatory variables -> 2^k possible models
# 
#### calculate everything by hand and gather information succinctly
# calculation pre: just formating
all_combo <- expand.grid(c("AgeD","0"),c("WitHt","0"),c("HipHt","0"),c("Girth","0"))
all_combo = cbind(a = "BW~~", all_combo)
all_combo[all_combo=="0"] <- NA
all_model = apply(all_combo, 1, paste, collapse="+")
for (i in seq_along(all_model)){
  all_model[i] = gsub("NA\\+", "", all_model[i])
  all_model[i] = gsub("\\+NA", "", all_model[i])
  all_model[i] = gsub("~\\+", "", all_model[i])
}
all_model = all_model[-length(all_model)]
all_model # now we exhust all combinations and have all possible models
# calculation using loop, go through every single model
R_adj <- numeric();AIC <- numeric();BIC <- numeric();Cp <- numeric()
for (i in seq_along(all_model)){
  lmod = do.call(lm,list(formula=all_model[i],data = Heifer_data))
  R_adj[i] <- summary(lmod)$adj.r.squared
  AIC[i] <- nrow(Heifer_data)*log((anova(lmod)$'Sum Sq'[length(anova(lmod)$'Sum Sq')])/nrow(Heifer_data))+(2*length(lmod$coefficients))
  BIC[i] <- nrow(Heifer_data)*log((anova(lmod)$'Sum Sq'[length(anova(lmod)$'Sum Sq')])/nrow(Heifer_data))+ length(lmod$coefficients)* log(nrow(Heifer_data))
  Cp[i] <- (anova(lmod)$'Sum Sq'[length(anova(lmod)$'Sum Sq')])/summary(lmod_full)$sigma^2-(nrow(Heifer_data)-2*length(lmod$coefficients))
}
# gather info
Gather = cbind(All_Model = all_model,
               R_adj= round(R_adj,4),
               AIC = round(AIC,4),
               BIC = round(BIC,4),
               Mallows = round(Cp,4))
Gather = data.frame(Gather)

#### using functions
library(leaps)
all_model2 <- regsubsets(BW ~ .,nbest = 6,data = Heifer_data) # exhaustively
all_model2_summary <- summary(all_model2)
all_model2_summary$which # all combinations
Gather2 = with(all_model2_summary,round(cbind(which,rsq,adjr2,cp,bic),3)) ;Gather2[,9] = Gather2[,9] + 2007.494
Gather2
Gather

# why they dont have AIC and why their BICs are substracted by 2007.494???
with(all_model2_summary,round(cbind(which,rsq,adjr2,cp,bic),3))[,9]
names(all_model2_summary)

# Alternative approach: 
# sequential model selection (k(k+1)/2 out of 2^k models will be considered)
# Forward Selection // Backward Elimination // Stepwise Regression

### Demo
library(MASS)
full_fit <- lm(BW ~ AgeD + WitHt + HipHt + Girth, data = heifer_data)
min_fit <-  lm(BW ~ 1, data = Heifer_data)

# Backward selection 
stepAIC(full_fit,direction = "backward",scope = list(upper = full_fit,lower = min_fit))
# Forward selection 
stepAIC(min_fit, direction = "forward",scope = list(upper = full_fit,lower = min_fit))
# stepwise regression
stepAIC(min_fit , direction = "both", scope = list(upper = full_fit,lower = min_fit))

# Note: variable selection is sensitive to outliers and influential points.





































