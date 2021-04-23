set.seed(123)
n <- 100; x <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
y <- x + x2 + x3 + rnorm(n, sd = .1)
y
e <- function(a, b) a -  sum( a * b ) / sum( b ^ 2) * b
ey <- e(e(y, x2), e(x3, x2))
ex <- e(e(x, x2), e(x3, x2))
sum(ey * ex) / sum(ex ^ 2)

coef(lm(y ~ x + x2 + x3 -1 ))


## The least squares estimate for the coefficient of a multivariate regression model 
##is exactly regression through the origin with the 
##linear relationships with the other regressors removed from both the regressor and outcome 
##by taking residuals.

ey <- resid(lm(y ~ x2 + x3 - 1)) # regressor x removed from the predictor
length(ey)
ex <- resid(lm(x ~ x2 + x3 - 1)) # regressor x remved from the outcome
ex
length(ex)
sum(ey * ex) / sum(ex ^ 2)

sum(ey)
sum(ex)
sum(ex^2)

10.74 * 9.77 / 81.92



ey <- resid(lm(y ~ x + x2 - 1)) # regressor x removed from the predictor
length(ey)
ex <- resid(lm(x3 ~ x2 + x - 1)) # regressor x remved from the outcome
ex
length(ex)
sum(ey * ex) / sum(ex ^ 2)

sum(ey)
sum(ex)
sum(ex^2)



