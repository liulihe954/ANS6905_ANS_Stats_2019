data(gala,package="faraway")
library(MASS)
lmod <- lm(burntime ~ nitrogen + chlorine + potassium, leafburn)
logtrans(lmod,plotit=TRUE,alpha=seq(-min(leafburn$burntime) + 0.001,0,by=0.01))


## Broken Stick Regression

lmod1 <- lm(sr ~ pop15, savings, subset= (pop15 < 35))
lmod2 <- lm(sr ~ pop15, savings, subset= (pop15 > 35))
plot( sr ~ pop15, savings , xlab = "pop n under 15", ylab = "Savings Rate")
abline(v=35,lty=5)
segments(20, lmod1$coef[1]+lmod$coef[2]*20,35,lmod1$coef[1] + lmod1$coef[2]*35)
segments(48, lmod2$coef[1]+lmod$coef[2]*48,35,lmod2$coef[1] + lmod1$coef[2]*35)


## 
lhs <- function(x) ifelse (x < 35, 35-x,0)
rhs <- function(x) ifelse ( x < 35, 0, x-35)
lmod <- lm(sr ~ lhs(pop15))


## 
data(savings,package="faraway")
lmod <- lm(sr ~ poly(ddpi,4), savings) # To create 4th order polynomials
summary(lmod)


lmod <- lm(sr ~ polym(pop15,ddpi,degree=2),savings)
lmod

##
funky <- function(x) sin(2*pi*x^3)^3
x <- seq(0,1,by=0.01)
y <- funky(x) + 0.1*rnorm(101)
matplot(x,cbind(y,funky(x)),type="pl",ylab="y",pch=20, lty=1, col=1)

g4 <- lm(y ~ poly(x,4))
g12 <- lm(y ~ poly(x,12))
matplot(x,cbind(y,g4$fit,g12$fit),type="pll", ylab="y",lty=c(1,2),
          pch=20, col=1)

install.packages("splines")
require(splines)
knots <- c(0,0,0,0,0.2,0.4,0.5,0.6,0.7,0.8,0.85,0.9,1,1,1,1)
bx <- splineDesign(knots,x)
lmodb <- lm(y ~ bx -1)
matplot(x, bx, type="l", col=1)
matplot(x, cbind(y,lmodb$fit), type="pl", ylab="y", pch=20,lty=1,
          col=1)




##
g4 <- lm(y ~ poly(x,4))
g12 <- lm(y ~ poly(x,12))
> matplot(x,cbind(y,g4$fit,g12$fit),type="pll", ylab="y",lty=c(1,2),
          pch=20, col=1)


##
install.packages("splines")
library(splines)
bx <- splineDesign(knots,x)
lmodb <- lm(y ~ bx -1)







