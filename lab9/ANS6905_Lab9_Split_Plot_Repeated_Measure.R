#rm(list=ls())
# install.packages('agricolae') # Statistical Procedures for Agricultural Research
# install.packages("lme4")
# install.packages("nlme")
# install.packages('lmerTest')
# install.packages('glmmTMB')
library(tidyverse)
library(lme4)
library(lmerTest)
library(nlme)
library(gridExtra)
library(faraway)
library(pbkrtest)
library(car)
library(glmmTMB)

##==============================###
#=== Split-plot Design in R    ===#
##==============================###
# setwd AND read
setwd("/Users/liulihe95/Desktop/ANS6905_ANS_Stats_2019/lab9")
split.plot.data.raw <- read.csv("LAB8_Split_Plot.csv",header =T)
names(split.plot.data.raw)
str(split.plot.data.raw)
# massage
split.plot.data = split.plot.data.raw %>% 
  dplyr::select(-Animal) %>% 
  dplyr::mutate_at(c("Parity","MP","Postnatal"),factor) %>% 
  dplyr::mutate_at(c("MP"), funs(dplyr::recode(.,`5`= "a", `10`= "b",`15`= "c"))) %>% 
  rename(TrtA = MP,TrtB = Postnatal)
str(split.plot.data)
summary(split.plot.data)

### Let's go graphical
split.plot.data %>%
  ggplot(aes(y=AVG_gr,x=TrtA,color=TrtB))+
  geom_point()+
  facet_wrap(~Parity)

### look at them seperately
# first lets prepare some colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
} #  note: R recognize color as code fromat
gg_color_hue(3) # e.g.
### plots
colors = gg_color_hue(3)
p1 = 
  split.plot.data %>% # parity 1
  dplyr::filter(Parity == "1") %>% 
  ggplot(aes(y=AVG_gr,x=TrtB))+
  geom_point(color = colors[1])+
  facet_wrap(~TrtA)
p2 = 
  split.plot.data %>% # parity 2
  dplyr::filter(Parity == "2") %>% 
  ggplot(aes(y=AVG_gr,x=TrtB))+
  geom_point(color = colors[2])+
  facet_wrap(~TrtA)
p3 = 
  split.plot.data %>% # parity 3
  dplyr::filter(Parity == "3") %>% 
  ggplot(aes(y=AVG_gr,x=TrtB))+
  geom_point(color = colors[3])+
  facet_wrap(~TrtA)
grid.arrange(p1, p2,p3, nrow = 3)
dev.off()
# The MP and postnatal are fixed effects, but the Parity is clearly a random effect.
# We must also consider the interaction between Postnatal and MP

# calculate degree of freedom
summary(split.plot.data)
block.df = 3 - 1 
trt.MP.df = 3 - 1
trt.Psnt.df = 4 - 1 
Inact.MP.Psnt.df = trt.MP.df * trt.Psnt.df
res.df = (4*3 - 1) * (3-1)


# Think what to include in the model
# lmer - Liner;Mixed:Effect model
library(lme4)
mod.mix0 = lmer(AVG_gr ~  TrtA * TrtB + (1|Parity) + (1|Parity:TrtA),split.plot.data)
mod.mix = lmer(AVG_gr ~  TrtA * TrtB + (1|Parity),split.plot.data)
mod.mix2 = lmer(AVG_gr ~  TrtA * TrtB + (1|Parity:TrtA),split.plot.data)
# The 1 indicates that the random effect is constant within each group.

# check anova
anova(mod.mix0, 
      type = c("3"), 
      ddf = "Kenward-Roger")
#lmerTest::anova.lmerModLmerTest(mod.mix,ddf = "lme4")
anova(mod.mix)

sumary(mod.mix0)
sumary(mod.mix)
sumary(mod.mix3)

# largest variance component is that due to the error = 23.07; 
#while parity effect =6.12 

# test interaction term
mod.mix.small <- lmer(AVG_gr ~ TrtA + TrtB  + (1|Parity), split.plot.data)
KRmodcomp(mod.mix,mod.mix.small) # An approximate F-test based on the Kenward-Roger approach.
# no significant interaction
anova(mod.mix.small)

# Partial-F-like(?) test for lmer
mod.mixi <- lmer(AVG_gr ~ TrtB+ (1|Parity), split.plot.data)
KRmodcomp(mod.mix, mod.mixi)
mod.mixv <- lmer(AVG_gr ~ TrtA + (1|Parity), split.plot.data)
KRmodcomp(mod.mix,mod.mixv)
# same conclusion

# dignostic
plot(fitted(mod.mix),residuals(mod.mix),xlab="Fitted",ylab="Residuals")
qqPlot(split.plot.data$AVG_gr)

##======================###
##   Repeated Measures  ##
##=====================###
# In repeated measures designs, there are several individuals (or units) and measurements are taken repeatedly on each individual.
# When these repeated measurements are taken over time, it is called a longitudinal study or, in some applications, a panel study.

#Often it is reasonable to believe that the response of each individual has several components: 
# a fixed effect, which is a function of the treatment;
# a random effect, which expresses the variation between individuals; 
# an error, which is due to measurement or unrecorded variables


rep.data.raw <- read.csv("LAB8_Repeated_Measure.csv",header=T)
names(rep.data.raw)
str(rep.data.raw)
# massage
rep.data = rep.data.raw %>%
  #dplyr::mutate(ID = as.character(ID)) %>% 
  dplyr::mutate_at(c("TRT","Day","ID"),factor) %>%
  dplyr::mutate_at(c("TRT"),funs(dplyr::recode(.,`0`= "A", `2`= "B",`4`= "C")))
str(rep.data)
summary(rep.data)
rep.data$ID = factor(rep.data$ID,levels(rep.data$ID)[c(4,5,1:3)])
### Let's go graphical
rep.data %>%
  ggplot(aes(y=BW,x=Day,color=Day))+
  geom_point()+
  facet_wrap(~TRT,ncol = 1)

# subject is the experimental unit; not a measure
# 3 reatments with 5 subjects per treatment
# each subject measured 4 times in 4 time points (periods)
# yijk = mu + trt * time + subject(trt) + error
# what is a random term here?
# covariance between measurements on the same subject:
# it is assumed that the covariances between measurements on different subjects are zero

# challenge: measurements on the same subject can be correlated
# define an covariance structure

# MODELING OPTIONS:
# homogenous variances and covariances among repeated measures
# heterogenous variances and covariances among repeated measures

#equal variance across measurements and equal covariance between measurements


# ====================================== #
##=  variance - covariance structure   =##
# ====================================== #


######## Unstructured Symmetry #######
fit.us <- lme(BW ~ TRT * Day,  # fixed effect
              random = ~ 1|ID, # random effect
              data = rep.data,  # data
              correlation = corSymm(form = ~ 1|ID), # grouping factor is ID
              weights = varIdent(form = ~ 1|Day))   # Different variances for each measurements
anova(fit.us)
AIC(fit.us)
getVarCov(fit.us, individual="1",type="conditional")
######## Compound Symmetry ############
fit.cs <- lme(BW ~ TRT*Day,# fixed effect
              random = ~1|ID,# random effect
              data = rep.data,# data
              correlation = corCompSymm(form = ~ 1|ID))# within group correlation structure

anova(fit.cs)
AIC(fit.cs)
getVarCov(fit.cs, individual="1",type="conditional")
######## Auto Regressive ###############
fit.ar <- lme(BW ~ TRT * Day,  # fixed effect
              random = ~ 1|ID, # random effect
              data = rep.data,  # data
              correlation = corAR1(form = ~ 1|ID))
anova(fit.ar)
AIC(fit.ar)
getVarCov(fit.ar, individual="1",type ="conditional")
#########    VCOV: Toeplitz   ################
fit.toep <- glmmTMB(BW ~ TRT + Day + TRT:Day + (1|TRT:ID), 
                    data = rep.data, dispformula=~0)
VarCorr(fit.toep)

########    VCOV: spatial power ##################
fit.sp <- lme(BW ~ TRT*Day,  # fixed effect
              random = ~ 1|ID, # random effect
              data = rep.data,  # data
              correlation = corExp(form = ~ 1|ID))

anova(fit.sp)
AIC(fit.sp)
getVarCov(fit.sp, individual="1",type ="conditional")
###### selection of the model ############################
anova(fit.cs,fit.us)
anova(fit.AR,fit.us)
