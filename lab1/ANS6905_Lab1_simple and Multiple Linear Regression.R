##===============================================================================##
##                            0.Before the War                                   ## 
##===============================================================================##
rm(list=ls()) # Will "clear up" your environment. Prevents conflicts. But NOT recommended.
# load some supports (nvm, I composed beforehand for better demo, no need to check...)
source("Functions_Sources_Just_Ignore.R")
### Set up working directories(wd)
# setwd("where your data are & where your script will exist")
setwd("/Users/liulihe95/Desktop/ANS6905_ANS_Stats_2019/lab1")
getwd()
# files
list.files()
file.exists(c("BodyWeight.csv",
              "DataItalianRestaurantsNewYork.txt"
              ))
# Check the package requirments - run before analysis, because R relys on these pkgs.
mypkg = c("easypackages","tidyverse","faraway","ggplot2","car")
Check_pkg(mypkg)
libraries(mypkg)

##===============================================================================##
##                         1.Simple Linear Regression                            ## 
##===============================================================================##
# load raw data for analysis
BodyWeightData <- read.csv("BodyWeight.csv",sep = ",",header=T,as.is=F)
str(BodyWeightData)
summary(BodyWeightData)

# subsetting your data IF necessary
BodyWeightData_sub <- subset(BodyWeightData,select = c("DMI","BW")) # Just for demo
str(BodyWeightData_sub)
dim(BodyWeightData_sub) # NO.row goes first
head(BodyWeightData_sub,5) # check/show the first 5 (put anything you like) lines
message("s\nhah")
# get to know your dataset in a graphical way (very ituitive)
plot_simple_lm = ggplot(data = BodyWeightData_sub,aes(x = BW , y = DMI)) + geom_point(color='blue')
plot_simple_lm
dev.off()
# Fit a module, recall our formula (Y = ß0 + ß1 * x + ε)
BW_simple_lm <- lm(DMI ~ BW, # regression formula (R style syntax)
                   data= BodyWeightData_sub) # speficy your dataset; R will search/match the variable names
summary(BW_simple_lm)
# succinct version 1
sumary(BW_simple_lm)
# succinct version 2
coef(BW_simple_lm)

# Get back to graphical - because we believe in what we see
BodyWeightData_sub <- add_column(BodyWeightData_sub, my_prediction = predict(BW_simple_lm)) # dataframe massage
plot_simple_lm + geom_line(color='red',data = BodyWeightData_sub,aes(x = BW, y= my_prediction)) # remember what you were expecting
dev.off()

# let check what's happening behind
x = model.matrix(BW_simple_lm) # Get your model Design Matrix
y = BodyWeightData_sub$DMI # Vector of Responses
# x'x
t(x) %*% x 
# (x'x)^(-1)
xtxi = solve(t(x) %*% x)
xtxi
# x'y 
t(x) %*% y

# Least Squares Estimator (slides)
# Parameter Estimation: β1 / β0
Beta_hat = xtxi %*% t(x) %*% y 
Beta_hat
coef(BW_simple_lm)

# Estimation of Residual Variance
y_hat = BodyWeightData_sub$my_prediction
SS_res = sum((y - y_hat)^2)
SS_res
MSE = SS_res/(48 - 2);
MSE # an unbiased estimator of σ2
sqrt(MSE)
sumary(BW_simple_lm)

# Variance-Covariance Matrix β^hat
var_cov = xtxi * MSE
var_cov
sqrt(diag(var_cov))
sumary(BW_simple_lm)

# Hypothesis Testing 1: {t-test}
# Testing significance of regression --- Ho:β1 = 0 ; H1:β1 != 0
# Note that here can assume all the assumptions stand! (will be covered in the future)
## t-test
se_beta1_hat = sqrt(diag(var_cov))[2]
se_beta1_hat
t0 = Beta_hat[2,1]/se_beta1_hat
t0
summary(BW_simple_lm)

# Hypothesis Testing 2: {F test}
# Analysis of variance
# SS_T = SS_reg + SS_res
SS_reg = sum((predict(BW_simple_lm)-mean(y))^2)
SS_res = sum((y - y_hat)^2)
F0 = ((SS_reg)/(2-1))/MSE
F0
summary(BW_simple_lm)


# Coefficient of Determination (R^2)
R2 = SS_reg/(SS_reg + SS_res)
R2
R2_adj= 1-((SS_res/(48-2))/((SS_reg + SS_res)/(48-1)))
R2_adj
summary(BW_simple_lm)

##===============================================================================##
##                         2.multiple Linear Regression P1                       ## 
##===============================================================================##
# load raw data for analysis
ItaRestaurants = read.csv("DataItalianRestaurantsNewYork.txt",sep = "",header=T,as.is=F)
str(ItaRestaurants)
summary(ItaRestaurants)
head(ItaRestaurants,5) # check/show the first 5 (put anything you like) lines

# get to know your dataset in a graphical way (very ituitive)
plot(ItaRestaurants)
dev.off()

# Fit a module, recall our formula (Y = ß0 + ß1 * x + ε)
ItaRes_multi_lm <- lm(Price ~ Food + Decor + Service, # regression formula (R style syntax)
                      data = ItaRestaurants) # speficy your dataset; R will search/match the variable names
summary(ItaRes_multi_lm)
# succinct version 1
sumary(ItaRes_multi_lm)
# succinct version 2
coef(ItaRes_multi_lm)

# Contour plot: For better illustration, we just drop one of the predictors
ItaRes_coutour = ggplot(ItaRes_multi_lm, aes(Food, Decor, z = Price)) +
geom_density_2d(binwidth = 0.001)
ItaRes_coutour
dev.off()

# let check what's happening behind
x_Ita = model.matrix(ItaRes_multi_lm) # Get your model Design Matrix
y_Ita = ItaRestaurants$Price # Vector of Responses
x_Ita
# x'x
t(x_Ita) %*% x_Ita 
# (x'x)^(-1)
solve(t(x_Ita) %*% x_Ita ) 
# x'y 
t(x_Ita) %*% y_Ita

# Least Squares Estimator (slides)
xtxi_Ita = solve(t(x_Ita) %*% x_Ita) # 
xtxi_Ita

# all the estimates!! # Parameter Estimation: β1 / β0
Beta_hat_Ita = xtxi_Ita %*% t(x_Ita) %*% y_Ita
Beta_hat_Ita
coef(ItaRes_multi_lm)

# Estimation of Residual Variance
ItaRestaurants <- add_column(ItaRestaurants, my_prediction = predict(ItaRes_multi_lm)) # dataframe massage
y_hat_Ita = ItaRestaurants$my_prediction
SS_res_Ita = sum((y_Ita - y_hat_Ita)^2)
SS_res_Ita
MSE_Ita = SS_res_Ita/(168 - 4);
MSE_Ita # an unbiased estimator of σ2
sqrt(MSE_Ita)
summary(ItaRes_multi_lm)

# Variance-Covariance Matrix β^hat
var_cov_Ita = xtxi_Ita * MSE_Ita
var_cov_Ita
sqrt(diag(var_cov_Ita))
summary(ItaRes_multi_lm)
# Tip: to get the var-cov matrix we can simply use the function vcov() that comes with base R
identical(round(vcov(ItaRes_multi_lm),3),round(var_cov_Ita,3))
# we got the same matrix

# Hypothesis Testing 1: {t-test} for individual regression coefficient
# Testing significance of regression --- Ho:β1 = 0 ; H1:β1 != 0
# Note that here can assume all the assumptions stand! (will be covered in the future)
## t-test (e.g. Food)
se_beta1_hat_food = sqrt(diag(var_cov_Ita))[2] # Food
se_beta1_hat_food # Food
t0_food = Beta_hat_Ita[2,1]/se_beta1_hat_food
t0_food
sumary(ItaRes_multi_lm)

# Confidence Interval
Beta_hat_Ita[2,1]+ c(-1,1)*1.974535*se_beta1_hat_food # By hand, but have to get critical value 1.974535 prior to calculations
Confint(ItaRes_multi_lm,level=0.95) # Or simply try to use the function


# Hypothesis Testing 2: {F test} for significance of regression
# Analysis of variance
# SS_T = SS_reg + SS_res
SS_reg_Ita = sum((predict(ItaRes_multi_lm)-mean(y_Ita))^2)
SS_res_Ita = sum((y_Ita - y_hat_Ita)^2)
F0_ItaRes = (SS_reg_Ita/(4-1))/MSE_Ita
F0_ItaRes
summary(ItaRes_multi_lm)

# Coefficient of Determination (R^2)
R2_Ita = SS_reg_Ita/(SS_reg_Ita + SS_res_Ita)
R2_Ita
R2_adj_Ita= 1-((SS_res_Ita/(168-4))/((SS_reg_Ita + SS_res_Ita)/(168-1)))
R2_adj_Ita
summary(ItaRes_multi_lm)

# get well-calculated variances using anova()
anova(BW_simple_lm)
anova(ItaRes_multi_lm)
 
##===============================================================================##
##                         3.multiple Linear Regression P2                       ## 
##===============================================================================##
# Regression models using qualitative/categorical variables (dummy/indicator/binary variable)
                  
                           ### Continue with our Cow BW Case ###

# load raw data for analysis
BodyWeightData <- read.csv("BodyWeight.csv",sep = ",",header=T,as.is=F)
str(BodyWeightData)

# Remerber to run this data message, otherwise Trt will not be treated as categorical variable #
BodyWeightData <- BodyWeightData %>% mutate_at("Trt", funs(factor(.)))
# check dataset 
str(BodyWeightData)
summary(BodyWeightData) # Diet - HC & LC - two levels (not sure about the biological meaning...)
                        # Trt - 0/0.5/1 - three levels totally

# Fit a module, recall our formula (Y = ß0 + ß1 * x1 + ß2 * x2 + ß3 * x3  + ε)
BW_categ_lm_full <- lm(DMI ~ BW + Diet + Trt, # regression formula (R style syntax)
                   data= BodyWeightData) # speficy your dataset; R will search/match the variable names
summary(BW_categ_lm_full)
# succinct version 1
sumary(BW_categ_lm_full)
# succinct version 2
coef(BW_categ_lm_full)

# partial F test: (extra-sum-of-squares method)
BW_categ_lm_nested <- lm(DMI ~ BW + Trt , # regression formula (R style syntax)
                       data= BodyWeightData) # speficy your dataset; R will search/match the variable names
summary(BW_categ_lm_nested)
# succinct version 1
sumary(BW_categ_lm_nested)
# succinct version 2
coef(BW_categ_lm_nested)

# Partial F test - for individual predictor
SS_reg_diff = sum((predict(BW_categ_lm_full) - mean(BodyWeightData$DMI))^2) - sum((predict(BW_categ_lm_nested) - mean(BodyWeightData$DMI))^2)
SS_res_full = sum((BodyWeightData$DMI - predict(BW_categ_lm_full))^2)
F0_p = (SS_reg_diff/(2-1))/(SS_res_full/(48 - 5))
sqrt(F0_p)
# OR using anova funciton: put two models into one comparison-function anova()
anova(BW_categ_lm_full,BW_categ_lm_nested)
#
summary(BW_categ_lm_full)$coefficients # Get model coefficients
abs(summary(BW_categ_lm_full)$coefficients[3,3])# locate the t value of DIetLC
sqrt(anova(BW_categ_lm_full,BW_categ_lm_nested)$'F') # check the square root of the F value of the partial F test, and it's the same as t value



# check analysis of variance
anova(BW_categ_lm_full)
anova(BW_categ_lm_nested)

# How to reset the reference group???
# Pay attention to the name of every predictor in summary table
summary(BW_categ_lm_full)
BodyWeightData_new_ref <- within(BodyWeightData,Diet <-relevel(Diet,ref = "LC"))
BW_categ_lm_new_ref <- lm(DMI ~ BW  + Diet + Trt, # regression formula (R style syntax)
                          data= BodyWeightData_new_ref) # speficy your dataset; R will search/match the variable names
summary(BW_categ_lm_new_ref)


# Since we are familiar with the theory behind
# here i just demostrate how to get marginal sum of squares 
Anova(BW_categ_lm_full, type=3)
anova(BW_categ_lm_full)

Anova(ItaRes_multi_lm, type=3)
anova(ItaRes_multi_lm)


# Take home game
