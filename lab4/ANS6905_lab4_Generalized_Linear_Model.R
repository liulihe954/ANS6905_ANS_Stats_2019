##===============================================================================##
##                            0.Before the War                                   ## 
##===============================================================================##
### prepare the packages we need.
library(car);library(MASS);library(leaps);
library(tidyverse);library(faraway);
library(locfit)
## If you are missing something; uncomment the following line and fill in the pck name
# install.packages("locfit")
## Remember to set working directory
# setwd("/Users/liulihe95/Desktop/ANS6905_ANS_Stats_2019/lab4/")

#  1.Categorical Variables 
# Categorical Variables
# variables that can take one of a limited, and usually fixed, number of possible values

# Use Generalized Linear model to model the relationship, when we have categorical response variables.
# a link function is used to specify the expected error distribution - (e.g Binomial, Binary, Poisson)

# Model a dichotomous variable (risk)
# Model count data (count of events)

##===============================================================================##
##                       1. Binomial and Binary                                  ## 
##===============================================================================##
### Contingency Table for categorical variables ###
Parity = factor(c(rep("P",240) ,rep("M",560)),levels = c("P","M"))
Dystocia = factor(c(rep("D",35),rep("ND",205),rep("D",35),rep("ND",525)),levels = c("D","ND"))
Parity_Dystocia_data = data.frame(Parity = Parity, Dystocia = Dystocia) ## create data
str(Parity_Dystocia_data)
head(Parity_Dystocia_data,20)
# Turn to Contingency Table - show cross-classified categorical data on two or more variables
CT = ftable(Dystocia ~ Parity, Parity_Dystocia_data)
# ?xtabs()
CT ## create a 2-by-2 table
# Tip：can change levels using releve
#Parity_Dystocia_data$Parity = relevel(data$Parity, "M") 


# Question:Is there an association between parity and dystocia?
# H0: There is no association between Parity and Dystocia, / Dystocia is independent of Parity
# How we test this?

# 1. Chi-square test for independence
# Data pre
P_Dh0 = ((35+35)/(35+35+525+205));P_Dh0
Parity_Dystocia_exp = data.frame(Parity = factor(c(rep("P",240) ,rep("M",560)),levels = c("P","M")), 
                                 Dystocia = factor(c(rep("D",240*P_Dh0),rep("ND",240*(1-P_Dh0)),rep("D",560*P_Dh0),
                                                     rep("ND",560*(1-P_Dh0))),levels = c("D","ND")))
CT_exp = ftable(Dystocia ~ Parity,Parity_Dystocia_exp)
#
Chisq_Dystoia = list(CT_Obs = CT,
                     CT_Exp = CT_exp)
Chisq_Dystoia

# Calculation
T0 = sum(((35-21)^2/21)+((205-219)^2/219)+((35-49)^2/49)+((525-511)^2/511))
T_005 = qchisq(0.95, df=1)
T0 > T_005 # so we reject H0

# Use functions
chisq.test(CT, correct = FALSE)$expected # Expected under H0
# chisquare test for independence
chisq.test(CT, correct = FALSE) ## without Yate's correction; to = SUM of (O-E)2/E
chisq.test(CT, correct = TRUE) ## with Yate's correction; to = SUM of (O-E)2/E-0.5

# f2.Fisher exact test
# Hypergeometric distribution
# by hand
# e.g D among Primiparous (Total -800, D -70, )
CT
N = (35+35+205+525)
n = (35+205)
S = (35+35)

P_005 = 0
for (k in c(35:70)){
 prob_tmp = choose(S,k)*choose(N-S,n-k)/choose(N,n)
 P_005 = P_005 + prob_tmp
}
P_005

# function:
fisher.test(CT,alternative = "g") 
# Reject H0
# Ho= there is no association based on CUMULATIVE HYPERGEOMETRIC DISTRIBUTION



## categorical data :Binary response variable
# Date pre
D = c(12,8,23,27)
ND = c(90,210,115,315)
Parity = c("P","M","P","M")
Breed = c("J","J","H","H")
#
Parity_Dystocia_data2 = data.frame(D = D, ND = ND, Parity = Parity, Breed = Breed)
Parity_Dystocia_data2
# set you ref befor model
Parity_Dystocia_data2$Parity = relevel(Parity_Dystocia_data2$Parity, "P")
Parity_Dystocia_data2$Breed= relevel(Parity_Dystocia_data2$Breed, "J")


# Binomial distribution
# each cow can be considered as a Bernoulli trialwith two possible outcomes
# within parity and breed, there are N (Total) identical and independent Bernoulli trials
# y defined as the number of successes(No. Dystocia) in N trials follows a binomial process

# Fit model
# log(P_D/(1-P_D)) = β0 + βp * Parity + βh * Breed
logit.model = glm(cbind(D,ND) ~ Parity + Breed, Parity_Dystocia_data2, family = "binomial")
summary(logit.model)
# interpret
exp(-0.9671)
exp(0.6008)

# 1.evaluate the significance of a predictor; --- Wald test
#z.value=estimates/SE(estimates); z.value follow Z distibution N(0,1)
#e.g. ParityM
z_M = abs(-0.967/0.2542)
z_005 = pnorm(0.05/2, mean = 0, sd = 1)
z_M > z_005

#
# 2 .evaluate the significance of a predictor; --- Likelihood Ratio test
#z.value=estimates/SE(estimates); z.value follow Z distibution N(0,1)
#e.g. BreedH
model_full = glm(cbind(D,ND) ~ Parity + Breed, Parity_Dystocia_data2, family = "binomial")
DV1 = deviance(model_full); DV1
model_nested = glm(cbind(D,ND) ~ Parity, Parity_Dystocia_data2, family = "binomial")
DV2 = deviance(model_nested); DV2
# alternative syntax 1
deltaDF = DV2-DV1
deltaDF > qchisq(0.95, df=1) #Under 0.05 level
# alternative syntax 2
anova(model_nested,model_full,test = "Chisq")
# alternative syntax 3
pchisq(deltaDF, df = 1, lower = FALSE)

#
## 3. goodness-of-fit test
## H0:the model fits the data well
## The residual deviance follows (approximately) a chi-sqare distribution, df = (n-p)
logit.model = glm(cbind(D,ND) ~ Parity + Breed,Parity_Dystocia_data2, family = "binomial")
dev = deviance(logit.model); dev
n = 4 ; df = n - (2+1) # what's n here? Not the No of cows!
df = df.residual(logit.model); df # alternatively
pchisq(dev,df,lower = FALSE) # Ho: The model fits the data (fail to reject)


## predictions
# before that we first check the predictions
Obs_P = Parity_Dystocia_data2$D/(Parity_Dystocia_data2$D + Parity_Dystocia_data2$ND)## observed probabilities
attr(Obs_P,"names") = c("D/P/J","D/M/J","D/P/H","D/M/H")
Exp_P = fitted(logit.model) ## predicted probabilities
Obs_P;Exp_P

## e.g. D/M/H
coef(logit.model)
exp(-2.1439+ -0.9671+0.6008)/(exp(-2.1439+ -0.9671+0.6008)+1)
residuals(logit.model, type = "response") ## residuls = Obs.Prob - Pred.Prob

## Binary Data: 1 0r 0 (yes or no)
# 1. 
Dystocia = c(rep(1,12),rep(0,90),
             rep(1,8),rep(0,210),
             rep(1,23),rep(0,115),
             rep(1,27),rep(0,315))
Parity = c(rep("P",(102)),rep("M",218),rep("P",138),rep("M",342))
Breed = c(rep("J",320),rep("H",480)) # new variable

Parity_Dystocia_data3 = data.frame(Dystocia = Dystocia,Parity = Parity,Breed = Breed)
head(Parity_Dystocia_data3,20)
Parity_Dystocia_data3$Parity = relevel(Parity_Dystocia_data3$Parity, "P")
Parity_Dystocia_data3$Breed= relevel(Parity_Dystocia_data3$Breed, "J")
## 
logit.model_alternative= glm(Dystocia ~ Parity + Breed,Parity_Dystocia_data3, family = "binomial")
summary(logit.model_alternative)

## Wald Test
logit.model_alternative = glm(Dystocia ~ Parity + Breed, data = Parity_Dystocia_data3, family = "binomial")
summary(logit.model)
summary(logit.model_alternative) 

# Importantly, in this case the deviancedoes not assess the goodness-of-fitof the model

##===============================================================================##
##                               2. Count Data                                   ## 
##===============================================================================##
#### 2.Count Data
Mastitis_data = read.table("Mastitis_count.csv", header = TRUE,sep=",")
Mastitis_data$Lactation = as.factor(Mastitis_data$Lactation)
str(Mastitis_data)
#fit model
poisson.model = glm(Mastitis ~ Lactation + THI, Mastitis_data, family = "poisson")


## Interpretations
summary(poisson.model)

## Wald Test
# e.g. THI
summary(poisson.model)
z_M_THI = abs(0.07015/0.01060)
z_005_THI = pnorm(0.05/2, mean = 0, sd = 1)
z_M_THI  > z_005_THI

## likelihood ratio test
poisson.model_full= glm(Mastitis ~ Lactation + THI, Mastitis_data, family = "poisson") ## full model
poisson.model_nested = glm(Mastitis ~ Lactation, Mastitis_data, family = "poisson") ## reduced model
anova(poisson.model_full,poisson.model_nested,test = "Chisq")

## goodness-of-fit test: residual deviance
# Importantly, in this case the deviance does not assess the goodness-of-fitof the model
poisson.model = glm(Mastitis ~ Lactation + THI, Mastitis_data, family = "poisson")
dev = deviance(poisson.model); dev
df = df.residual(poisson.model); df
pchisq(dev,df,lower = FALSE) # Ho: The model fits the data (fail to reject)

## Prediction
# e.g. Location=2; THI = 69
summary(poisson.model)
log_lambda = -5.24712 + 1.08386 + 0.07015*69
lambda = exp(log_lambda)
predict(poisson.model)
# plotting
names(Mastitis_data)
Mastitis_data$phat <- predict(poisson.model, type="response")
Mastitis_data <- Mastitis_data[with(Mastitis_data, order(THI,Lactation)),]
ggplot(Mastitis_data, aes(x = THI , y = phat, colour = Lactation)) +
  geom_point(aes(y = Mastitis), alpha=.5,position = position_jitter(h=.1)) + 
  geom_smooth()
dev.off()
