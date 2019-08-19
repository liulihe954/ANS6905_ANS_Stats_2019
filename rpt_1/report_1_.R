rm(list=ls())
#setwd("C:/Users/liulihe/Desktop/applied_statitics_in_ANS/report")
setwd("C:/Users/lihe.liu/Desktop/applied_statitics_in_ANS/report")
install.packages(pkgs='leaps')
install.packages(pkgs='car')
install.packages(pkgs='MASS')
install.packages(pkgs='gvlma')
library(leaps)
library(car)
library(MASS)
library(gvlma)

my_data<-read.csv("DataReportOne.txt",header=T,as.is=T,sep="",colClasses = c("factor","factor","factor","numeric","numeric","numeric"))
head(my_data);dim(my_data)
str(my_data)


my_model_1<-lm(SCC~Farm+Parity+Holstein+THI+NDF,data=my_data)
summary(my_model_1)
anova(my_model_1)



##1.AIC method
step <- stepAIC(my_model_1,direction="both")
step$anova # display results

#2.All Subset Regression( Mallows Cp )
leaps<-regsubsets(SCC~Farm+Parity+Holstein+THI+NDF,data=my_data,nbest=2)
subsets(leaps,statistic = "cp",main="Cp plot for All Subset Regression")
abline(a=0,b=1,h=1,v=1,lty=2,col="red")
dev.off()
subsets(leaps,statistic = "cp",main="Cp plot for All Subset Regression",xlim=c(4,8),ylim=c(4,7.5))
abline(a=0,b=1,h=1,v=1,lty=2,col="red")
dev.off()
subsets(leaps,statistic = "cp",main="Cp plot for All Subset Regression",xlim=c(5.5,7.8),ylim=c(5.8,7.5))
abline(a=0,b=1,h=1,v=1,lty=2,col="red")
dev.off()
subsets(leaps,statistic = "cp",main="Cp plot for All Subset Regression",xlim=c(5.6,7.5),ylim=c(5.7,7.0))
abline(a=0,b=1,h=1,v=1,lty=2,col="red")
dev.off()

#3.All Subset Regression(adjr2)
leaps<-regsubsets(SCC~Farm+Parity+Holstein+THI+NDF,data=my_data,nbest=2)
plot(leaps,scale="adjr2",xlim=c(0.2,0.33))
dev.off()


###final:"SCC ~ Farm + Holstein + THI + NDF"
###however, some levels need to be merged

#reset levels
levels(my_data$Holstein)[c(1,2)]<-0
levels(my_data$Farm)[c(1,2)]<-"C_H"
str(my_data)
my_model_2<-lm(SCC~Farm+Holstein+THI+NDF,data=my_data)
summary(my_model_2)
anova(my_model_2);Anova(my_model_2,type = 3)



###cHECKING _2

par(mfrow=c(2,2))
plot(my_model_2)
dev.off()


#assumptoins checking
#o mean; constant variance; error uncorrelated;error normality

#checking error normality
#qq plot
par=(mfrow=c(1,1))
qqnorm(my_model_2$residuals,ylab ="Raw Residuals", xlab="Theoretical Quantiles",id.method="identity")
qqline(my_model_2$residuals, col=2,lwd =2)
dev.off()
# Shapiro_Wilk test
shapiro.test(my_model_2$residuals)
#histogram of residuals
residplot<-function(fit,nbreaks){
  z<-rstudent(fit)
  hist(z,breaks=nbreaks,freq=F,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x,mean=mean(z),sd=sd(z)),
        add=T,col="blue",lwd=2)
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("Normal Curve","Kernel Density Curve"),
         lty=1:2,col=c("blue","red"),cex=.7)
}
residplot(my_model_2,75)
dev.off()

#error uncorrelated
durbinWatsonTest(my_model_2)
#non-signicicance means uncorrelated

#x_y linear
crPlots(my_model_2)
dev.off()

#constant Vatiance
ncvTest(my_model_2)#signif means non-constant
spreadLevelPlot(my_model_2)


#zern mean
mean(residuals(my_model_2))


#checking for multicollinearity
any(vif(my_model_2)[,1]>4)
cor(my_data[,c(4:6)])
scatterplotMatrix(my_data)




# unusual data diagnostics

#outlier
outlierTest(my_model_2)
#NO.178 is an outlier

#highleverage
hii_1 <- data.frame(hatvalues(my_model_2)) 
head(hii_1)
which(hii_1>(2*7)/196)
#22  64  86 178 184


hat.plot<-function(fit){
  p<-length(coefficients(fit))
  n<-length(fitted(fit))
  plot(hatvalues(fit),main="Index plot of Hat Values")
  abline(h=c(2,3)*p/n,col="red",lty=2)
  identify(1:n,hatvalues(fit),names(hatvalues(fit)))
}

hat.plot(my_model_2)
dev.off()

# Cook's distance(influential points)
cd1 <- data.frame(cooks.distance(my_model_2))
plot(cooks.distance(my_model_2))
which(round(head(cd1),4)>4/(nrow(my_data)-length(my_model_2$coefficients)))
# No.2 is influential


# Removing unusual observations
my_model_3 <- lm(SCC~Farm+Holstein+THI+NDF,
                 subset = (1:nrow(my_data))[-c(2)],data = my_data) 
summary(my_model_3) # No influential points, just an outlier
Anova(my_model_3, type = "III")

#,22,64,86,178,184
#############################

# Box-cox transformation
# ML Methods
boxcox(my_model_2) 
any(my_data[,c(4:6)]<0)
#lambda = 0 then yt = log(y)

#logtransformed model
my_model_4 <- lm(log(SCC)~Farm+Holstein+THI+NDF,
                 subset = (1:nrow(my_data))[-c(2)],data = my_data)#,22,64,86,178,184
summary(my_model_4)
anova(my_model_4)
Anova(my_model_4, type = "III")

#my_data<-read.csv("DataReportOne.txt",header=T,as.is=T,sep="",colClasses = c("factor","factor","factor","numeric","numeric","numeric"))

##########################

###cHECKING _4
par(mfrow=c(2,2))
plot(my_model_4)
dev.off()

#assumptoins checking
#o mean; constant variance; error uncorrelated;error normality

#checking error normality
#qq plot
par=(mfrow=c(1,1))
qqnorm(my_model_4$residuals,ylab ="Raw Residuals", xlab="Theoretical Quantiles",id.method="identity")
qqline(my_model_4$residuals, col=2,lwd =2)
dev.off()
# Shapiro_Wilk test
shapiro.test(my_model_4$residuals)#
#histogram of residuals
residplot<-function(fit,nbreaks){
  z<-rstudent(fit)
  hist(z,breaks=nbreaks,freq=F,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x,mean=mean(z),sd=sd(z)),
        add=T,col="blue",lwd=2)
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("Normal Curve","Kernel Density Curve"),
         lty=1:2,col=c("blue","red"),cex=.7)
}
residplot(my_model_4,60)
dev.off()

  #error uncorrelated
durbinWatsonTest(my_model_4)#non-signicicance means uncorrelated

#x_y linear
crPlots(my_model_4)

#constant Vatiance
ncvTest(my_model_4)#signif means non-constant
spreadLevelPlot(my_model_4)

plot(fitted(my_model_4), rstudent(my_model_4), col = "red")
abline(h = 0, lty = 2)
max(abs(rstudent(my_model_2)))
which.max(abs(rstudent(my_model_2)))

#zern mean
mean(residuals(my_model_4))

#checking for multicollinearity
any(vif(my_model_2)[,1]>4)
cor(my_data[,c(4:6)])
scatterplotMatrix(my_data)


#overall checking
gvmodel <- gvlma(my_model_4)
summary(gvmodel)

###############################################
dev.off()
par(mfrow=c(3,2))
plot(fitted(my_model_2),residuals(my_model_2),col = "red", ylab ="Raw Residuals", xlab= "Fitted Values")
abline(h = 0,lty = 4)
plot(fitted(my_model_4),residuals(my_model_4),col = "red", ylab =" Raw_Residuals", xlab = "Fitted Values" )
abline(h = 0,lty = 4)
qqnorm (my_model_2$residuals,ylab ="Raw Residuals", xlab="Theoretical Quantiles")
qqline (my_model_2$residuals, col=2,lwd =2)
qqnorm (my_model_4$residuals,ylab ="Raw Residuals", xlab="Theoretical Quantiles")
qqline (my_model_4$residuals, col=2,lwd =2)
residplot(my_model_2,80)
residplot(my_model_4,80)

