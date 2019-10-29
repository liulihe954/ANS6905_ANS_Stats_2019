#install.packages("constrast")
library(car)
library(contrast)
setwd("/Users/liulihe95/Desktop/ANS6905_ANS_Stats_2019/lab5/")
# read the data from a file
lab5.data <- read.csv("Lab5_Canola_trial_CRD.csv",header = T,as.is = T)
# Convert  variable to factor
lab5.data$Trt <- as.factor(lab5.data$Trt)
str(lab5.data)


# Completely Randomized Design
# one-way analysis of variance
DMI.mod <- aov(ADG ~ Trt, data = lab5.data)
summary(DMI.mod) # reject the null
qf(0.05,2,12,lower.tail = F) # get F value based on significance level

#DMI.mod$residuals
qqnorm(DMI.mod$residuals,pch=20)
qqline(DMI.mod$residuals)

# boxplot for one factor analysis
boxplot(ADG~Trt,data=lab5.data)

# F-test assumption: equal variance among treatment levels (homogenity of variances )
# H0 : ;σA2 = σB2 = σC2
# Levene Test
# test whether the means of these deviations are equal for all treatments
# if the means (dj )are equal, then the variance among trt levels will be the same   
leveneTest(DMI.mod) #Accept the H0


# t test for significance
DMI.mod_t <- lm(ADG ~ Trt, data = lab5.data)
summary(DMI.mod_t) # yij = β0 + trti + ε

# treatment effect
model.tables(DMI.mod)

# Estimating the overall mean and treatment effect
ADG.mean <- mean(lab5.data$ADG)
ADG.mean

# different format
ADG.mean + 0.01867 # yij = mu + αi + ε
summary(DMI.mod_t) # yij = β0 + trti + ε


# Muliple comparison testing
# Tukey HSD test
DMI.mod2 <- TukeyHSD(DMI.mod,conf.level = 0.95,ordered = T) 
DMI.mod2 # check to see if 0 falls within the CI

# The powerdepends on a number of factors:
# the statistical significance criteriaused in the test 
# the magnitude of the effectof interest in the population (effect size)
# the sample size (n)

# given α and β, how many replications (r) do we need?


# power and sample size
model.tables(DMI.mod)
c(0.01866667, -0.12733333, 0.10866667) + ADG.mean
grp.means <- c(1.084,0.938,1.173)
grp.means

# number of replication per treatmnet group required 
summary(DMI.mod)
power.anova.test(groups=3,
                 n = 10,
                 between.var = var(grp.means),
                 within.var= 0.01393,
                 sig.level=.05,
                 power= NULL)

power.anova.test(groups=3,
                 n = NULL,
                 between.var = var(grp.means),
                 within.var= 0.01393,
                 sig.level=.05,
                 power=.95)

# levels of the factor
levels(lab5.data$Trt)

# tell R which groups to compare
#c1 <- c(1,-0.5,-0.5)  # comparing the average effects of the factor with control
c1 <- c(0.5,0.5,-1) 
c2 <- c(1,-1,0) # Canola vs Carinata

# combined the above 2 lines into a matrix
mat <- cbind(c1,c2)

# tell R that the matrix gives the contrasts you want
contrasts(lab5.data$Trt) <- mat

# these lines give you your results
DMI.mod3 <- aov(ADG~Trt, data = lab5.data)
# summary(DMI.mod3)

# Make sure to use summary.aov here or 'split' might not work
summary.aov(DMI.mod3, 
            split=list(Trt=list("Control v.s. Ave_treatment" =1,"Canola v.s. Carinata" = 2))) 

