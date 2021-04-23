rm(list=ls())
library(car)


# set working directory
setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab8_Splitplot_RepeatedMeasures/")

# read the data from a file
full.data <- read.csv("LAB8_SPLITPLOT.csv",header=T,as.is=T)

full.data$Parity <- factor(full.data$Parity)
full.data$MP <- factor(full.data$MP)
full.data$Postnatal <- factor(full.data$Postnatal)
str(full.data)


# Exploratory data analysis
with(full.data, xyplot(AVG_gr~ Postnatal | MP))

with(full.data, xyplot(AVG_gr~ Postnatal | MP, groups = Parity,aspect = "xy", 
                       type = "a"))

# Ignoring Experimental design and running factorial analysis
res.bad <- lm(AVG_gr~ MP*Postnatal, data = full.data)
anova(res.bad)

# correctly defining error term for main plot factor
res.good <- aov(AVG_gr~ Parity + MP*Postnatal + Error(Parity:MP), data = full.data)
summary(res.good)

# Checking assumptions: use error term.You can add the term without error, but the F tests are wrong. Assumption checking is OK
res2.good <- aov(AVG_gr~ Parity + MP*Postnatal + Parity:MP, data = full.data)
summary(res2.good)

plot(res2.good, 1)
plot(res2.good, 2)
plot(res2.good, 5)

# Differences in sub-plot factor
diff_subf <-with(full.data, HSD.test(AVG_gr, Postnatal, DFerror = 18, MSerror = 260))
names(diff_subf)


################## Repeated Measures ######################################
library(nlme)
library(lmerTest)

cat.mod3 <- lmer(y ~ trt*timepnt + (1|trt:id))
summary(cat.mod3)
anova(cat.mod3)

# read the data from a file
rep.data <- read.csv("LAB8_Repeated measures.csv",header=T,as.is=T)
head(rep.data)

rep.data$ID <- factor(rep.data$ID)
rep.data$TRT <- factor(rep.data$TRT)
rep.data$Day <- factor(rep.data$Day, ordered = T)
str(rep.data)

library(lmerTest)
cat.mod3 <- lmer(BW ~ TRT*Day  + (1|TRT:ID), data = rep.data)
anova(cat.mod3)


######## Compound symmetry ############################################

fit.cs <- lme(BW ~ TRT*Day,  # fixed effect
          random = ~1|ID, # random effect
          data = rep.data,  # data
          correlation = corCompSymm(form = ~ 1 |ID))

anova(fit.cs)
AIC(fit.cs)
getVarCov(fit.cs, individual="1",type="conditional")


######## Unstructured Symmetry ############################################
fit.us <- lme(BW ~ TRT*Day,  # fixed effect
              random = ~ 1|ID, # random effect
              data = rep.data,  # data
              correlation = corSymm(form = ~ 1 |ID), # corr US
              weights = varIdent(form = ~ 1 |Day))

anova(fit.us)
AIC(fit.us)
getVarCov(fit.us, individual="1",type="conditional")


######## AR1 ############################################
fit.AR <- lme(BW ~ TRT*Day,  # fixed effect
              random = ~ 1|ID, # random effect
              data = rep.data,  # data
              correlation = corAR1(form = ~ 1 |ID))
anova(fit.AR)
AIC(fit.AR)
getVarCov(fit.AR, individual="1",type="conditional")

###### selection of the model ############################
anova(fit.cs,fit.us)
anova(fit.us,fit.AR)













































