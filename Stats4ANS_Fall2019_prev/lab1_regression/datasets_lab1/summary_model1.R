rm(list=ls())
with(faithful,plot(eruptions,waiting))
title(main="Old faithful Geyser data") # Annotation to the plot in R

all <- lm(Fertility ~., swiss)
all
summary(all)


# For every 1 unit change in the predictor, what is the unit change in the response varaibel


## choose the relationship of the predictors
cor(swiss$Examination,swiss$Education)

efit <- lm(Fertility ~ . + ec, swiss)

## show this in the class
fit <- lm(count ~ spray, InsectSprays)
summary(fit)$coef[,]


##
summary(InsectSprays[,2])
summary(InsectSprays[,2])
sapply(InsectSprays,class) # to find out the classes 


##
# The regression coefficient in that case corresponds to 
# a difference between two grouos rather than the slipe of an actual line

# Regressor can be any order of varaibles in the model
model.matrix(pemax~height+weight, data= cystfibr)
