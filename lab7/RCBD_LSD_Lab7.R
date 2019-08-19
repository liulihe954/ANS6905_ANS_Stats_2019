## Randomized Complete Block Design #################
#####################################################
setwd("C:/Users/liuli/Desktop/applied_statitics_in_ANS/lab/lab7")
# read the data from a file
data_block_design <- read.csv("Lab7_CRBD_feedlot_data.csv",header = T,as.is = T)
head(data_block_design);dim(data_block_design)

# Mean of each treatmnet level
with(data_block_design,tapply(Cumm_DMI,Diet,mean))

# Mean of each block
with(data_block_design,tapply(Cumm_DMI,Blk,mean))

# fit a model 
fit_rcbd <- aov(Cumm_DMI ~ Diet + Blk , data = data_block_design)
summary(fit_rcbd)
#summary(aov(fit_rcbd))
fit_rcbd$fitted.values

# Change the order of fitting 
fit_rcbd_1 <- aov(Cumm_DMI ~  Blk + Diet  , data = data_block_design)
summary(fit_rcbd_1) # Balanced design and orthigonal predictors
summary(aov(fit_rcbd_1))

# Key assumption: No interaction between block and treatment

install.packages("asbio")
library(asbio)
with(data_block_design, tukey.add.test(Cumm_DMI, Diet, Blk)) 

# Interaction plot
with(data_block_design, interaction.plot(Diet, Blk, Cumm_DMI))


####Simple Latin Square Design#####
#################################

rm(list = ls())

# set working directory
setwd("D:/AppliedstatisticsforAnimalSciences_Fall2017/lab7_RCBD_LSD/")


# read the data from a file
data_LSD <- read.csv("LAB7_LSD_MilkYield.csv",header = T,as.is =T)
head(data_LSD);dim(data_LSD)
data_LSD$Period <- as.factor(data_LSD$Period)
data_LSD$Trt <- as.factor(data_LSD$Trt)
data_LSD$Cow <- as.factor(data_LSD$Cow)
str(data_LSD)

#table(data_LSD$Trt)

# Check the latin square structure in R
table_LSD <- matrix(data_LSD$MilkYield,4,4)
table_LSD
dev.off()

# Plot the data:
plot(MilkYield ~ Period, data = data_LSD) # Not much differences in bocking variables in the model
plot(MilkYield ~ Cow, data = data_LSD)
plot(MilkYield ~ Trt, data = data_LSD)

# fit the Latin Square model ( Reference model)
model_LSD <- lm(MilkYield ~ Period + Cow + Trt, data = data_LSD)
summary(model_LSD)
summary(aov(model_LSD))
model_LSD$fitted.values


#  Fit the Latin Square model (Effect model)
# Effect model where intercept is equal to the grand mean
options("contrasts")
model_LSD_alternate <- lm(MilkYield ~ Period + Cow + Trt, data = data_LSD,
                          contrasts = list(Period = "contr.sum", Cow = "contr.sum",
                            Trt = "contr.sum"))
summary(model_LSD_alternate)
model_LSD_alternate$fitted.values


# Change the order of fitting 
model_LSD_1 <- aov(MilkYield ~ Trt + Period + Cow, data = data_LSD)
summary(model_LSD_1) # balanced design and orthogonal predictors 

# Row means, Column means and Treatment means
model.tables(model_LSD_1,"means")

# Row effects, Column effects and Treatment effects
model.tables(model_LSD_1,"effects")


# Residual diagnostics
plot(model_LSD$fitted.values,model_LSD$residuals,xlab = "Fitted_Values",ylab="Residuals") 
abline(a = 0,b = 0, col="red", lwd = 2,lty ="dashed")
dev.off()

# QQPlot
stdRes = rstandard(model_LSD)
qqnorm(stdRes,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes, col=2,lwd=2)
dev.off()

install.packages("car")
library(car)
shapiro.test(model_LSD$residuals)



##### Replicated Latin Square Design ##########
###############################################


# read the data from a file
data_rep_LSD <- read.csv("LAB7_Duplicate Latin Square ION.csv",header =T,as.is=T)
head(data_rep_LSD);dim(data_rep_LSD)

data_rep_LSD$Pen <- as.factor(data_rep_LSD$Pen)
data_rep_LSD$Square <- as.factor(data_rep_LSD$Square)
data_rep_LSD$Period <- as.factor(data_rep_LSD$Period)
data_rep_LSD$TRT <- as.factor(data_rep_LSD$TRT)
str(data_rep_LSD)

# Means

tapply(data_rep_LSD$ADG,data_rep_LSD$Pen,mean)
tapply(data_rep_LSD$ADG,data_rep_LSD$Square,mean)
tapply(data_rep_LSD$ADG,data_rep_LSD$Period,mean)
tapply(data_rep_LSD$ADG,data_rep_LSD$TRT,mean)


# Model where Pen is nested in Square
rep_LSD_mod1 <- aov(ADG ~ Period + Square/Pen + TRT, data = data_rep_LSD)
summary(rep_LSD_mod1)
TukeyHSD(rep_LSD_mod1,"TRT")


