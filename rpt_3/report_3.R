rm(list=ls())
setwd("C:/Users/liuli/Desktop/applied_statitics_in_ANS/report")
install.packages("car");library(car)
install.packages("contrast");library(contrast)

my_data<-read.csv("DataReportThree.txt",header=T,as.is=T,sep="",colClasses = c("factor","factor","factor","factor","numeric") )
head(my_data);dim(my_data)
str(my_data)

my_table <- matrix(my_data$DMI,4,4)
my_table

Trt<-c(my_data[,3])
T1<-which(my_data$FatLevel=="2"& my_data$FatSaturation=="unsaturated")
T2<-which(my_data$FatLevel=="2"& my_data$FatSaturation=="saturated")
T3<-which(my_data$FatLevel=="4"& my_data$FatSaturation=="unsaturated")
T4<-which(my_data$FatLevel=="4"& my_data$FatSaturation=="saturated")
Trt[T1]<-c("T1");Trt[T2]<-c("T2");Trt[T3]<-c("T3");Trt[T4]<-c("T4")
my_data_r<-cbind(my_data[,c(1,2,5)],Trt)



plot(DMI ~ STEERS, data = my_data_r) # Not much differences in bocking variables in the model
plot(DMI ~ PERIODS, data = my_data_r)
plot(DMI ~ Trt, data = my_data_r)
dev.off()

my_data$FatSaturation = relevel(my_data$FatSaturation, "unsaturated")
model_test <- lm(DMI ~ STEERS+ PERIODS + FatLevel + FatSaturation + FatLevel * FatSaturation, data = my_data)
summary(model_test)
anova(model_test)
model_test_2 <- aov(DMI ~ STEERS+ PERIODS + FatLevel + FatSaturation + FatLevel * FatSaturation, data = my_data)
Anova(model_test, type = "III")
model_test$fitted.values

# Row effects, Column effects and Treatment effects
model.tables(model_test_2,"effects")







#Fit the Latin Square model (Reference model)
model_ref <- lm(DMI ~ STEERS+ PERIODS +Trt, data = my_data_r)
model_ref_2 <- aov(DMI ~ STEERS+ PERIODS +Trt, data = my_data_r)
#summary(model_ref)
#summary(aov(model_ref))
##model_ref$fitted.values
#anova(model_ref)
#Anova(model_ref, type = "III")

DMI.mod2 <- TukeyHSD(model_ref_2, ordered = T) # what is alpha here !
DMI.mod2 # check to see if 0 falls within the CI



#Fit the Latin Square model (Effect model)(intercept=grand mean)
#my_data_r[,4]<-relevel(my_data_r[,4],ref="T1")
#options("contrasts")
#model_efc <- lm(DMI ~ STEERS+ PERIODS + Trt, data = my_data_r,
#                          contrasts = list(PERIODS = "contr.sum", PERIODS = "contr.sum",
                  
#                                                                    Trt = "contr.sum"))
#summary(model_efc)
#summary(aov(model_efc))
#model_efc$fitted.values
#anova(model_efc)
#Anova(model_efc, type = "III")


model_ref_1 <- aov(DMI ~ STEERS+ PERIODS + Trt, data = my_data_r)
#summary(model_ref_1)
# Row means, Column means and Treatment means
model.tables(model_ref_1,"means")
# Row effects, Column effects and Treatment effects
model.tables(model_ref_1,"effects")


# Residual diagnostics
plot(model_test$fitted.values,model_test$residuals,xlab = "Fitted_Values",ylab="Residuals") 
abline(a = 0,b = 0, col="red", lwd = 2,lty ="dashed")
dev.off()

# QQPlot
stdRes = rstandard(model_test)
qqnorm(stdRes,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes, col=2,lwd=2)
dev.off()


shapiro.test(model_test$residuals)




coded <- function(x) ifelse(x == x[1], -1, 1)
summary(lm( DMI~ coded(FatLevel) * coded(FatSaturation), my_data))

model.tables(model_test_2)


with(my_data, interaction.plot(x.factor = FatLevel,trace.factor = FatSaturation, response = DMI, fun = mean, type = "b", legend = T,
                                 ylab= " DMI", main = " Interaction plot", pch = c(1,19)))


# Run post hoc test
TukeyHSD(aov(BHBA ~ Energy +  Choline + Energy : Choline, data = lab6.data), conf.level=.99) 

