rm(list=ls())
library(car)
library(MASS)

#setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab3_Varaibleselection")
setwd("C:/Users/liulihe/Desktop/applied_statitics_in_ANS/lab/lab3")
getwd()
heifer_data <- read.csv("multicol_heifer_data.csv",header=T,as.is=T)
head(heifer_data)

heifer_multicol <- lm(BW ~ AgeD + WitHt + HipHt + Girth, data = heifer_data)
summary(heifer_multicol)

# Evaluate Multi-Collinearity
vif(heifer_multicol) # variance inflation factors for each of the regressor

# Variable selection - Backward selection
step <- stepAIC(heifer_multicol, direction="backward")
step$anova # display results

# Variable selection - Forward selection
step <- stepAIC(heifer_multicol, direction="forward")
step$anova # display results

# Variable selection - Stepwise selection
step <- stepAIC(heifer_multicol, direction="both")
step$anova # display results





