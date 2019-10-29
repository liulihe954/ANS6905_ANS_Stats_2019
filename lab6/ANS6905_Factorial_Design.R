# read the data from a file
setwd("/Users/liulihe95/Desktop/ANS6905_ANS_Stats_2019/lab6/")
lab6.data <- read.csv("Lab6_Factorial_Anova.csv",header = T,
                      colClasses=c("NULL",NA,NA,NA)) # "NULL" (note the quotes!) means skip the column, NA means that R chooses the appropriate data type for that column.
str(lab6.data)

# Numeric summary of the data
with(lab6.data, tapply(BHBA, list(Energy,Choline), mean))


# Residuals and Model adequacy
Weight.mod <- lm(BHBA ~ Energy + Choline,data=lab6.data)
summary(Weight.mod)
# Residuals vs predicted yield
plot(Weight.mod$fitted.values,Weight.mod$residuals,main="Residuals vs Fitted",pch=20)
abline(h =0,lty =2)

# qqplot
qqnorm(Weight.mod$residuals,pch=20)
qqline(Weight.mod$residuals)


# Run the factorial anova with interaction
model.aov <- aov(BHBA ~ Energy +  Choline + Energy : Choline, data = lab6.data)
summary(model.aov)

M_test<-lm(BHBA ~ Energy +  Choline + Energy : Choline, data = lab6.data)
summary(M_test)
M_test$fitted.values

# Run the factorial anova without interaction ( Addition Model)
summary(aov(BHBA ~ Energy +  Choline, data = lab6.data))



# Effects
PredictorCvt <- function(x) ifelse(x == x[1], -1, 1)
summary(lm(BHBA ~ PredictorCvt(Energy) * PredictorCvt(Choline),lab6.data))

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




