rm(list = ls())

#####################
## Contingency Table for categorical variables

Floor_type = c(rep("NCF",499) ,rep("CF",221))
Tret = c(rep("Pre",252),rep("Post",247),rep("Pre",108),rep("Post",113))

data = data.frame(Floor = Floor_type, Trt = Tret) ## create data
head(data)
str(data)

CT = ftable(Floor_type ~ Tret, data)
CT ## create a 2-by-2 table

cot_table <- xtabs(Floor_type ~ Tret, data) # To create a contigency table




data$Parity = relevel(data$Parity, "Post") # probability of Dystocia is same among different parity levels
chisq.test(CT, correct = FALSE)$expected # Expected under H0

# chisquare test for independence

chisq.test(CT, correct = FALSE) ## without Yate's correction; to = SUM of (O-E)2/E
chisq.test(CT, correct = TRUE) ## with Yate's correction; to = SUM of (O-E)2/E-0.5

# fisher exact test

fisher.test(CT) ## Fisher's Exact Test; Ho= there is no association based on CUMULATIVE HYPERGEOMETRIC DISTRIBUTION; 

#####################
## categorical data :Binary response variable
#Creating table
D = c(12,8,23,27)
ND = c(90,210,115,315)
Parity = c("P","M","P","M")
Breed = c("J","J","H","H")

data = data.frame(D = D, ND = ND, Parity = Parity, Breed = Breed); data
data$Parity = relevel(data$Parity, "M")
data$Breed= relevel(data$Breed, "J")


## Wald test
#First test to evaluate the significance of apredictor
#z.value=estimates/SE(estimates); z.value follow Z distibution N(0,1)

logit.model = glm(cbind(D,ND) ~ Parity + Breed, data, family = "binomial")
summary(logit.model)

## likelihood ratio test 
#Second test to evaluate the significance of apredictor

modelF = glm(cbind(D,ND) ~ Parity + Breed, data, family = "binomial") ## full model
DF = deviance(modelF); DF

modelR = glm(cbind(D,ND) ~ Breed, data, family = "binomial") ## reduced model
DR = deviance(modelR); DR

DR-DF

anova(modelR,modelF, test = "Chisq")
pchisq(DR - DF, df = 1, lower = FALSE)

## goodness-of-fit test
logit.model = glm(cbind(D,ND) ~ Parity + Breed, data, family = "binomial")
dev = deviance(logit.model); dev
df = df.residual(logit.model); df
pchisq(dev,df,lower = FALSE) # Ho: The model fits the data (fail to reject)

## predictions
data
coef(logit.model)
data$D/(data$D + data$ND) ## observed probabilities
fitted(logit.model) ## predicted probabilities
residuals(logit.model, type = "response") ## residuls = Obs.Prob - Pred.Prob

data.frame(Observed = data$D,
           Fitted = round((data$D+data$ND) * fitted(logit.model),0)) # Observed vs Fitted

#####################
## Binary Data

Dystocia = c(rep(1,12),rep(0,90),
             rep(1,8),rep(0,210),
             rep(1,23),rep(0,115),
             rep(1,27),rep(0,315))

Parity = c(rep("P",(102)),rep("M",218),rep("P",138),rep("M",342))
Breed = c(rep("J",320),rep("H",480))

datae = data.frame(Dystocia = Dystocia, 
                   Parity = Parity,
                   Breed = Breed);dim(datae)
head(datae)
datae$Parity = relevel(datae$Parity, "M")
datae$Breed= relevel(datae$Breed, "J")

## Wald Test
logit.model.e = glm(Dystocia ~ Parity + Breed, data = datae, family = "binomial")
summary(logit.model)
summary(logit.model.e)

## Testing the significance of predictor
modelF = glm(Dystocia ~ Parity + Breed, data = datae, family = "binomial") ## full model
DF = deviance(modelF); DF
modelR = glm(Dystocia ~ Breed, data = datae, family = "binomial") ## reduced model
DR = deviance(modelR); DR
(DR - DF)

anova(modelR,modelF, test = "Chisq")
pchisq(DR - DF, df = 1, lower = FALSE)

#####################
## Count Data
setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab4_Generalized_Linear_Model")
data = read.table("Mastitis_count.csv", header = TRUE,sep=",")
head(data)
data$Lactation = as.factor(data$Lactation)
str(data)

poisson.model = glm(Mastitis ~ Lactation + THI, data, family = "poisson")
summary(poisson.model)

## likelihood ratio test

modelF = glm(Mastitis ~ Lactation + THI, data, family = "poisson") ## full model
DF = deviance(modelF); DF
modelR = glm(Mastitis ~ Lactation, data, family = "poisson") ## reduced model
DR = deviance(modelR); DR
(DR - DF) ## greater than 3.841 (X2 df = 1)

anova(modelR,modelF, test = "Chisq")
pchisq(DR - DF, df = 1, lower = FALSE)

## goodness-of-fit test
poisson.model = glm(Mastitis ~ Lactation + THI, data, family = "poisson")
dev = deviance(poisson.model); dev
df = df.residual(poisson.model); df
pchisq(dev,df,lower = FALSE) 

## predictions

estimates = coef(poisson.model);estimates
THI = seq(from = 30, to = 80, by = 1)
THI
yL1 = exp(estimates[1] + THI * estimates[4]) # Lactation 1
yL1
yL2 = exp(estimates[1] + estimates[2] + THI * estimates[4])
yL2
yL3 = exp(estimates[1] + estimates[3] + THI * estimates[4])
yL3

