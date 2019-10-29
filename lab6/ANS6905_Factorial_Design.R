library(tidyverse)
# read the data from a file
setwd("/Users/liulihe95/Desktop/ANS6905_ANS_Stats_2019/lab6/")
lab6.data <- read.csv("Lab6_Factorial_Anova.csv",header = T,
                      colClasses=c("NULL",NA,NA,NA)) %>% # "NULL" (note the quotes!) means skip the column, NA means that R chooses the appropriate data type for that column.
  mutate(Choline = relevel(Choline,"NO")) %>% 
  mutate(Energy = relevel(Energy,"low"))

str(lab6.data) # two factors; 4 possible combinations; 3 rep each cell

# Numeric summary: mean of each cell
summary_table = with(lab6.data, tapply(BHBA, list(Energy,Choline), mean))
round(summary_table,4)
# alternatively: get the sum first and then the mean
xtabs(BHBA ~ Energy +  Choline,lab6.data)
round(xtabs(BHBA ~ Energy +  Choline,lab6.data)/3,4)

# Interaction Plot
with(lab6.data, interaction.plot(x.factor = Energy,
                                 trace.factor = Choline, 
                                 response = BHBA, fun = mean, 
                                 type = "b", legend = T,
                                 ylab= " Ketone_bodies", 
                                 main = " Interaction plot", pch = c(1,19)))
dev.off()


# linear model: check interaction term
# partial F test for interaction significance
model.interact<-lm(BHBA ~ Energy +  Choline + Energy : Choline, data = lab6.data)
model.no.interact<-lm(BHBA ~ Energy +  Choline, data = lab6.data)
anova(model.interact,model.no.interact) # fail to reject the null
# (Alternatively) run the factorial anova with interaction 
summary(aov(BHBA ~ Energy +  Choline + Energy : Choline, data = lab6.data))

# model
summary(model.no.interact)

# diagnostic
# Residuals vs predicted yield
plot(model.no.interact$fitted.values,model.no.interact$residuals,
     main="Residuals vs Fitted",pch=20)
abline(h =0,lty =2)
# qqplot
qqnorm(Weight.mod$residuals,pch=20)
qqline(Weight.mod$residuals)
dev.off()

# Treatment Effects
# (The effects are twice the value in the estimate column)
model.tables(aov(BHBA ~ Energy +  Choline, data = lab6.data))
0.04167 * 2 # engergy

# Interaction Plot
with(lab6.data, interaction.plot(x.factor = Energy,
                                 trace.factor = Choline, 
                                 response = BHBA, fun = mean, 
                                 type = "b", legend = T,
                                 ylab= " Ketone_bodies", 
                                 main = " Interaction plot", pch = c(1,19)))
# illustration
summary_table
((0.3333333 - 0.2666667) + (0.3-0.2))/2


# Third alternative model: overall mean
summary_table
mu = mean(lab6.data$BHBA)
mu
Energy_high = (0.3333333 + 0.3)/2 - mu
Choline_yes = (0.2 + 0.3)/2 - mu
# ab_high_yes = 0.3 + mu - Energy_high - Choline_yes
##
all_comb = matrix(c(1,1,1,
                    1,-1,1,
                    1,1,-1,
                    1,-1,-1),4,3,byrow = T)
all_pred = c()
for (i in c(1:4)){
  all_pred[i] = sum(c(mu,Energy_high,Choline_yes)*all_comb[i,])
}
all_pred

#same predictions
table(round(model.no.interact$fitted.values,3))


# Run post hoc test
TukeyHSD(aov(BHBA ~ Choline+ Energy, data = lab6.data), conf.level=.99) 


  
  