rm(list=ls())

phys1 <- read.csv("http://www.stat.ufl.edu/~winner/data/psychophysiology_rm.csv",
                  header=T)
attach(phys1);   names(phys1)
View(phys1)


time <- factor(time_lb,ordered=T) # ordered is True
patnum <- factor(patnum)

library(nlme)

#### Compound symmetry ############################################

fit.cs <- gls(lbrow ~ time,                              # fixed effect
              corr = corCompSymm(, form= ~ 1 | patnum) )
summary(fit.cs)
anova(fit.cs)
AIC(fit.cs)
getVarCov(fit.cs, individual="1",type="conditional")

####  Unstructured Symmetry ############################################
fit.un <- gls(lbrow ~ time,
              corr=corSymm(form = ~ 1 | patnum),
              weights = varIdent(form = ~ 1 | time)) # different varaiability at different time

summary(fit.un)
anova(fit.un)
AIC(fit.un)
getVarCov(fit.un, individual="1",type="conditional")



################ Autocorrelation1 ##############################
fit.ar1 <- gls(lbrow ~ time,
               corr = corAR1(, form= ~ 1 | patnum))
summary(fit.ar1)
anova(fit.ar1)
AIC(fit.ar1)
getVarCov(fit.ar1, individual="1",type="conditional") # same varaibility at different time points

#### ARH1 ##############################

fit.arh1 <- gls(lbrow ~ time,
                corr = corAR1(, form = ~ 1 | patnum), weight = varIdent(form = ~ 1 | time))
summary(fit.arh1)
anova(fit.arh1)
AIC(fit.arh1)
getVarCov(fit.arh1, individual="1",type="conditional")


### how to find which model performances better ####
anova(fit.arh1,fit.ar1)




