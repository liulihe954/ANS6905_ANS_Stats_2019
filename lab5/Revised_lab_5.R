rm(list = ls()) 
library(car)
install.packages(pkgs='constrast')
library(contrast)

# set working directory
#setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab5_Onewayanova/")
#setwd("C:/Users/liulihe/Desktop/applied_statitics_in_ANS/lab/lab5)
setwd("C:/Users/liuli/Desktop/applied_statitics_in_ANS/lab/lab5")

# read the data from a file
lab5.data <- read.csv("Lab5_Canola_trial_CRD.csv",header = T,as.is = T)
head(lab5.data);dim(lab5.data)


# Convert  variable to factor
lab5.data$Trt <- as.factor(lab5.data$Trt)
str(lab5.data)

# Run the model
DMI.mod <- aov(ADG~Trt, data = lab5.data)
summary(DMI.mod) # compare between treatment variabilty vs within treatment variability
names(DMI.mod)

# change the significance level at alpha = 0.01
qf(0.01,2,12,lower.tail = F) # interested in 0.01th quantile of F distribution
qf(0.05,2,12,lower.tail = F) 

# Estimating the overall mean and treatment effect
ADG.mean <- mean(lab5.data$ADG)
ADG.mean

# treatment effect
model.tables(DMI.mod)

# boxplot for one factor analysis
boxplot(ADG~Trt,data=lab5.data)

# Testing the validity of the assumptions
x11()
split.screen(c(2,2))
screen(1)
plot(DMI.mod$fitted.values,DMI.mod$residuals,main="Residuals vs Fitted",pch=20)
abline(h =0,lty =2)
screen(2)
plot(DMI.mod$model$Trt,DMI.mod$residuals,main= "Residuals vs levels")
screen(3)
plot(1:15,DMI.mod$residuals, main = " Residuals vs Time order", pch = 20)
screen(4)
qqnorm(DMI.mod$residuals,pch=20)
qqline(DMI.mod$residuals)

# Testing for homogenity of variances 
leveneTest(DMI.mod)

# Muliple comparison testing
# Tukey HSD test
DMI.mod2 <- TukeyHSD(DMI.mod, ordered = T) # what is alpha here !
DMI.mod2 # check to see if 0 falls within the CI


# power and sample size
grp.means <- c(1.084,0.938,1.173)
grp.means

# number of replication per treatmnet group required 
power.anova.test(groups=3,between.var=var(grp.means),within.var=0.01393,
                 sig.level=.01,power=.90)

# Orthogonal contrasts

# levels of the factor

levels(lab5.data$Trt)

# tell R which groups to compare
c1 <- c(0.5,0.5,-1)  # comparing the average effects of the factor with control
c2 <- c(1,-1,0) # Canola vs Carinata

# combined the above 2 lines into a matrix
mat <- cbind(c1,c2)

# tell R that the matrix gives the contrasts you want
contrasts(lab5.data$Trt) <- mat

# these lines give you your results
DMI.mod3 <- aov(ADG~Trt, data = lab5.data)
summary(DMI.mod3)

# Make sure to use summary.aov here or 'split' might not work
summary.aov(DMI.mod3, split=list(Trt=list("treatment vs. control"=1, " Canola vs Carinata" = 2))) 
