rm(list=ls())

cats.uni <- read.fwf("http://www.stat.ufl.edu/~winner/data/zylkene_uni_cat.dat",
                     width=c(rep(8,7)),col.names=c("id","weight","age","gender","trt","timepnt","y"))

attach(cats.uni)

id <- factor(id) 
trt <- factor(trt)
timepnt <- factor(timepnt,ordered=T) # good way to handle the order of time point


install.packages("lmerTest")
library(lmerTest)

cat.mod3 <- lmer(y ~ trt*timepnt + (1|trt:id))
summary(cat.mod3)
anova(cat.mod3)



#### Fitting AutoRegressive Error Structure #####
library(nlme)
fit.ar1 <- lme(y ~ trt*timepnt, 
               random= ~1 | id,
               corr = corAR1(form= ~ 1 |id))
summary(fit.ar1)
anova(fit.ar1)
AIC(fit.ar1)
getVarCov(fit.ar1, individual="4",type="conditional") # Fitting Auto Regressive Error Structure in the model

### Fitting heterogenous error structure ####
fit.ar1H <- lme(y ~ trt*timepnt, 
               random= ~1 | id,
               corr = corAR1H(form= ~ 1 |id))





detach(cats.uni)


##### Fitting Compoundsymmetry Error structure ##################
fit.CS <- lme(y ~ trt*timepnt,      # fixed effects
               random= ~1 |id,     # random effects
                data = cats.uni,     # data
               correlation = corCompSymm(, form = ~ 1 | id)) # correlation matrix within 1 patient

summary(fit.CS)
anova(fit.CS)
AIC(fit.CS)
getVarCov(fit.CS, individual="1",type="conditional")


# How to fit different variance parameters for different visits
csh_model_right = lme(y ~ trt*timepnt,                          # fixed effects 
                      random      = ~1|,                   # random effects 
                      data        = cats.uni,                # data
                      weights     = varIdent(form=~1|timepnt),      # different "weight" within each visit (I know)
                      correlation = corCompSymm(form = ~ 1 | id))                # CS correlation matrix within subject per random statement abov) 
getVarCov(csh_model_right, individual="1",type="conditional")

### How to get 

cat.mod3 <- lmer(y ~ trt + timepnt + trt:timepnt + (1|trt:id))
summary(cat.mod3)
anova(cat.mod3)


#############################################################
cats.multi <- read.fwf("http://www.stat.ufl.edu/~winner/data/cats_anxiety1.dat",
                       width=c(rep(8,13)), col.names=c("id","weight","age","gender",
                                                       "enviro", "origin","trt_cat","result","es1","es2","es3","es4","es5"))

attach(cats.multi)

trt_cat <- factor(trt_cat)
id <- factor(id)


##### Model with Greenhouse-Geisser and Huynh-Feldt df adjustments

dayLevels <- c(1,2,3,4,5)
dayFactor <- as.factor(dayLevels)
dayFrame <- data.frame(dayFactor)
dayBind <- cbind(es1,es2,es3,es4,es5)
dayModel <- lm(dayBind ~ trt_cat)

library(car)
cat.mod5 <- Anova(dayModel, idata=dayFrame, idesign = ~dayFactor)
summary(cat.mod5)