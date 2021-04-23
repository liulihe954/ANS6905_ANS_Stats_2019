rm(list=ls())

# set working directory
setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab6_factorial_anova")

# read the data from a file
lab6.data <- read.csv("Lab6_Factorial_Anova.csv",header = T,as.is = T)
head(lab6.data);dim(lab6.data)


# Convert  variable to factor
lab6.data$Energy <- as.factor(lab6.data$Energy)
lab6.data$Choline <- as.factor(lab6.data$Choline)
str(lab6.data)


# Residuals and Model adequacy
Weight.mod <- lm(BHBA ~ Energy + Choline, # regression formula
                 data=lab6.data)

# Residuals vs predicted yield
plot(Weight.mod$fitted.values,Weight.mod$residuals,main="Residuals vs Fitted",pch=20)
abline(h =0,lty =2)

# qqplot
qqnorm(Weight.mod$residuals,pch=20)
qqline(Weight.mod$residuals)


# Numeric summary of the data
with(lab6.data, tapply(BHBA, list(Energy,Choline), mean))

# Run the factorial anova with interaction
aov.out <- summary(aov(BHBA ~ Energy +  Choline + Energy : Choline, data = lab6.data))
aov.out

# Run the factorial anova without interaction ( Addition Model)
summary(aov(BHBA ~ Energy +  Choline, data = lab6.data))

# Effects
coded <- function(x) ifelse(x == x[1], -1, 1)
summary(lm( BHBA~ coded(Energy) * coded(Choline), lab6.data))

# The effects and interactions are twice the value in the estimate column 
model.tables(aov(BHBA ~ Energy +  Choline + Energy : Choline, data = lab6.data))

# Main effect of Choline is:
# Average response for presence of choline to average response for absence of Choline

#(0.3 + 0.2)/2  - (0.3333333 + 0.2666667)/2
#-0.025000 * 2

# Interaction Plot
with(lab6.data, interaction.plot(x.factor = Energy,trace.factor = Choline, response = BHBA, fun = mean, type = "b", legend = T,
                                     ylab= " Ketone_bodies", main = " Interaction plot", pch = c(1,19)))


# Run post hoc test
TukeyHSD(aov(BHBA ~ Energy +  Choline + Energy : Choline, data = lab6.data), conf.level=.99) 





















