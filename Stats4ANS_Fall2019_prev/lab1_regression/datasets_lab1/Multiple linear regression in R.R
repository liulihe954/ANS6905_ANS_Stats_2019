##### Multiple linear regression #################################
coef(lm(y ~ x + x2 + x3 -1 ))  # Good to show the intercept and regressors

###
ey <- resid(lm(y ~ x2 + x3 - 1)) #  Taking out the residual of regressor x removed from the predictor
ex <- resid(lm(x ~ x2 + x3 - 1)) # Taking out the residual of regressor x removed from the outcome
x_coef <- sum(ey * ex) / sum(ex ^ 2)
x_coef

## 
#The beauty of multiple linear regression is to fit factor variables as predictors
# We can build accurate prediction model


