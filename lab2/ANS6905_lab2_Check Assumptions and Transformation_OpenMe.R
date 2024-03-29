##===============================================================================##
##                            0.Before the War                                   ## 
##===============================================================================##
#rm(list=ls()) # Will "clear up" your environment. Prevents conflicts. But NOT recommended.
# load some supports (nvm, I composed beforehand for better demo, no need to check...)
source("Functions_Sources_Just_Ignore.R")
### Set up working directories(wd)
# setwd("where your data are & where your script will exist")
setwd("/Users/liulihe95/Desktop/ANS6905_ANS_Stats_2019/lab2")
getwd()
# files
list.files()
file.exists(c("lab2.1_BHBA_Glucose.csv"))
# Check the package requirments - run before analysis, because R relys on these pkgs.
mypkg = c("easypackages","tidyverse","faraway","ggplot2","car","ggpubr","MASS")
Check_pkg(mypkg)
libraries(mypkg)

##===============================================================================##
##                         1.Checking Assumptions                                ## 
##===============================================================================##
setwd("/Users/liulihe95/Desktop/ANS6905_ANS_Stats_2019/lab2")
Glucose_data_raw <- read.csv("lab2.1_BHBA_Glucose.csv",header = T)
library(tidyverse)
Glucose_data <- Glucose_data_raw %>% dplyr::select(BHBA_7_DIM,Glucose_7_DIM)
colnames(Glucose_data)
NOrow = dim(Glucose_data)[1]

### **1.Regression Analysis: Checking Assumptions **  

##  **1. Linear relationship: y ~ x **  
##  **2. Errors: Zero mean**  
##  **3. Errors: Constant variance σ2**  
##  **4. Errors: uncorrelated**  
##  **5. Errors: normality ε ~ Ν(0,σ2)**  
##Estimations and inferences FULLY based on these assumptions.And they need to be **CHECKED**   
##  1. Graphical  
##  2. Numerical    

# First we fit a linear model
lmod_glu <- lm(BHBA_7_DIM ~ Glucose_7_DIM, 
               data = Glucose_data )
summary(lmod_glu)

## Any departures from the assumptions on the model errors should appear in the residuals.

## 1.The most important diagnostic plot: ε_hat against y_hat,   
## because we can check  
## **zero mean** & **constant variance** & **relationship y~x** 
plot(fitted(lmod_glu),residuals(lmod_glu),col = "blue",ylim = c(-max(abs(residuals(lmod_glu))),max(abs(residuals(lmod_glu)))));abline(h = 0,lty = 4)

##2. Normal probability plot (Q-Q plot)  
## if the residuals are normally distributed, then the points should form a straight line**
qqnorm(residuals(lmod_glu), pch = 1, frame = FALSE);qqline(residuals(lmod_glu), col = "steelblue", lwd = 2)
## with confidence band
library(car)
qqPlot(residuals(lmod_glu)) 
library(ggpubr,quietly = T)
ggqqplot(residuals(lmod_glu))

##3. Checking normality using formal statistical tests.  
## H0: Data is normally distributed  
## H1: Data is not normally distributed  

# Shapiro–Wilk test
shapiro.test(residuals(lmod_glu))
# Kolmogorov–Smirnov test
ks.test(residuals(lmod_glu), "pnorm")

### Notes
# small samples most often pass normality tests (accept H0) 
# large samples generally reject H0 

## 4. Types of residuals  
##  1. Raw Residuals 
##  2. Standarized Residuals**  
##  3. (internally) Studentized Residuals**  
##  4. (externally) Studentized Residuals / Jackknife residuals**  
library(MASS,quietly = T)
#### Raw Residuals
# get from the lm()
raw_res = residuals(lmod_glu)
# by hand
raw_res = Glucose_data$BHBA_7_DIM - predict(lmod_glu)
# of course they are the same
all.equal(raw_res,(Glucose_data$BHBA_7_DIM - predict(lmod_glu)))

#### Standarized Residuals
# by hand
y_hat = predict(lmod_glu)
SS_res = sum((Glucose_data$BHBA_7_DIM - y_hat)^2)
MSE = SS_res/(NOrow - 2)# an unbiased estimator of σ2
stan_res = residuals(lmod_glu)/sqrt(MSE)
#(summary(lmod_glu))$sigma
# Seems not identical, the thres is near 1.5e-8, very strengent, also function has more massages...
# recommend use stdres() funcion,
all.equal(stdres(lmod_glu),stan_res)
stan_res = stdres(lmod_glu)

## Before we jump to studentized residuals, we have to put together some pieces of code  
## Then we have our **self-contained** functions to achieve any calculations.  
## Sure, feel free to take this piece to get studentized residuals in your dataset,
ModGet_CustomizeStudentRes = function(lmod){
  Studres_int = numeric(length(lmod$residuals))
  Studres_ext = numeric(length(lmod$residuals))
  Customized_MSE = numeric(length(lmod$residuals))
  message("Trying to remove each obs once at a time for a Customized MSE!")
  message("Preparing Hat matrix...")
  X <- model.matrix(lmod)
  H <- X %*% solve(t(X) %*% X) %*% t(X);message("Hat matrix done!")
  H_diag = diag(H)
  for (i in seq_along(lmod$residuals)){
    obverve_val = as.vector(predict(lmod)+residuals(lmod))[-i]
    y_hat = predict(lmod)[-i]
    SS_res = sum((obverve_val - y_hat)^2)
    Customized_MSE[i] = SS_res/((length(y_hat)) - length(lmod$coefficients))
    Studres_int[i] = lmod$residuals[i]/(summary(lmod)$sigma *sqrt(1 - H_diag[i]))
    Studres_ext[i] = lmod$residuals[i]/(sqrt(Customized_MSE[i])*sqrt(1 - H_diag[i]))
  }
  message("Custumization 1 done: ",length(Studres_int)," Internal studentized residuals were generated!");message("Custumization 2 done: ",length(Studres_int)," External studentized residuals were generated!")
  Results = list(StudentRes_internal = Studres_int, StudentRes_external= Studres_ext)
  return(Results)
}
# studres(lmod) # Jackknife
# rstudent(lmod) #Jackknife

#### (Internally/External) Studentized Residuals
StuRes = ModGet_CustomizeStudentRes(lmod_glu)
Internal_StuRes = StuRes$StudentRes_internal
External_StuRes = StuRes$StudentRes_external

plot_simple_lm = ggplot(data = Glucose_data,aes(x = Glucose_7_DIM , y = BHBA_7_DIM)) + geom_point(color='blue')
Glucose_data_pred <- add_column(Glucose_data, my_prediction = predict(lmod_glu))
plot_simple_lm + geom_line(color='red',data = Glucose_data_pred ,aes(x = Glucose_7_DIM , y= my_prediction))

## Outlier**: Data point with a large residual  
##  High Leverage**: Data point with a large hat value  
## Influential**:It has an unusual X-value with an unusual Y-value given its X-value  

#######
# Outlier: large residuals (external studentized residuals)
#Bonferroni critical value - t_critic1
t_critic1 = qt(0.05/(2*NOrow),(NOrow -length(lmod_glu$coefficient)-1))
# find the outliers
rstudent(lmod_glu)[which((abs(rstudent(lmod_glu)) - abs(t_critic1))>= 0)]
# let's get grapical!
data.frame(fitted = lmod_glu$fitted.values,
           External_StuRes = rstudent(lmod_glu)) %>%  
  ggplot(aes(x = fitted,y = External_StuRes))+ 
  geom_point(color='blue')+
  geom_hline(yintercept=t_critic1,color='red',linetype="dashed",size=.5)+
  geom_hline(yintercept=-t_critic1,color='red',linetype="dashed",size=.5)
######

#lmtest::bptest(lmod_glu) #Breusch-Pagan test
#car::ncvTest(lm)  # Breusch-Pagan test

######
# high leverage: data point with a large hat_value
# set thres and select
h_critic1 = (2*length(lmod_glu$coefficients))/NOrow
all_hat = hatvalues(lmod_glu)
all_hat[which(all_hat - h_critic1 >= 0)]
# graphical
data.frame(fitted = lmod_glu$fitted.values,
           Hat_values = hatvalues(lmod_glu)) %>%  
  ggplot(aes(x = fitted,y = Hat_values))+ 
  geom_point(color='blue')+
  geom_hline(yintercept=h_critic1,color='red',linetype="dashed",size=.5)+
  geom_hline(yintercept=-h_critic1,color='red',linetype="dashed",size=.5)
######

#######
# Influential: data point that greatly affects the estimates of the model
# Cook's Distance
library(car)
all_cook = cooks.distance(lmod_glu)
# set thres
ck_critic1 = 4/(NOrow -length(lmod_glu$coefficients))
ck_index = which((all_cook-ck_critic1)>=0);attr(ck_index,"names")=NULL
all_cook[ck_index]
# plots
data.frame(fitted = lmod_glu$fitted.values,
           Cooks = all_cook) %>%  
  ggplot(aes(x = fitted,y = Cooks))+ 
  geom_point(color='blue')+
  geom_hline(yintercept=ck_critic1 ,color='red',linetype="dashed",size=.5)+
  geom_hline(yintercept=-ck_critic1 ,color='red',linetype="dashed",size=.5)

# Now try to remove influential points that have considerable influence on the model
# Try to fit a new model ( %>% is the pipe operator, can pass objects down, ohhh thanks tidyverse!!!)
lmod_glu_rm_ck <- Glucose_data %>% 
  mutate(External_StuRes = rstudent(lmod_glu),
         All_hatvalues = all_hat,
         All_cooks_distance = all_cook) %>% 
  filter(All_cooks_distance<ck_critic1) %>% 
  lm(BHBA_7_DIM ~ Glucose_7_DIM,data =.)
summary(lmod_glu_rm_ck)
summary(lmod_glu)

# integrate removal information
Glucose_data_pred_rm_ck <- add_column(Glucose_data_pred,
                                      Influ = ifelse(rownames(Glucose_data_pred)%in%ck_index,"Yes","No"))
#
plot_simple_lm_rm_ck = ggplot(data = Glucose_data_pred_rm_ck,
                              aes(x = Glucose_7_DIM,y = BHBA_7_DIM,color = Influ)) + 
  geom_point()+
  geom_abline(intercept=coef(lmod_glu)[1],
              slope=coef(lmod_glu)[2],color = "blue") + #model with Influential points
  geom_abline(intercept=coef(lmod_glu_rm_ck )[1],
              slope=coef(lmod_glu_rm_ck )[2],color = "red") #model without Influential points
plot_simple_lm_rm_ck

##--------------------------------##
##  Time to play with the codes.  ##
##--------------------------------##
# Can you plot the new lines, without OUTLIER and High Leverage point, respectively???
## Hint, first try to get an index that has the position infomration of the targets to remove
## Then fit a model without these points.observations
## when plotting, use the index information, R will fill in different colors based on this.


##===============================================================================##
##                            2.Transformation                                 ## 
##===============================================================================##
## Now we try some transformation.
library(MASS)
lmod_glu <- lm(BHBA_7_DIM ~ Glucose_7_DIM, 
               data = Glucose_data)
# check data before run boxcox
summary(Glucose_data) # a. NO negative values b. max/min ratio is ratively high
boxcox(lmod_glu,plotit = T,lambda = seq(-2,2,by =0.2))

# lambda = - 0.5
lmod_glu_bc_trans = lm(1/sqrt(BHBA_7_DIM) ~ Glucose_7_DIM, 
                       data = Glucose_data)
summary(lmod_glu_bc_trans)
# For visualization: transform back to original scale
# e.g. we predicted value is (1/sqrt(BHBA_7_DIM)) = 5
# then the actually value in original scale is BHBA_7_DIM = (1/5)^2
plot_simple_lm_bc_trans = ggplot(data = Glucose_data,aes(x = Glucose_7_DIM , y = BHBA_7_DIM)) +
  geom_point(color='blue')+
  geom_line(color='red',data = Glucose_data_pred ,aes(x = Glucose_7_DIM , y= my_prediction))+
  geom_line(aes(Glucose_data$Glucose_7_DIM,(1/predict(lmod_glu_bc_trans))^2))
plot_simple_lm_bc_trans
##

# ggplot can fit the line perfectly, but involves much complexity...
# Surely, it is good to know that but this may be too advanced...
library(ggplot2)
ggplot(Glucose_data, aes(Glucose_7_DIM,BHBA_7_DIM)) + geom_smooth() + geom_point()

##--------------------------------##
##  Time to play with the codes.  ##
##--------------------------------##
# Can you try boxcox and back transfromation, WITHOUT the influential points?
# Remove the influential points we detected and then try to transform and trans back. 
# Good luck.


Sys.getenv("R_LIBS_USER")
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)
