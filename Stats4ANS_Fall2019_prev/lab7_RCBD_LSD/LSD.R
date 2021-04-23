# Latin Square Design in R

Rows <- c(rep("R1",1),rep("R2",1),rep("R3",1),rep("R4",1))

Col <- c(rep("C1",4),rep("C2",4),rep("C3",4),
         rep("C4",4))

seed <- c("C","B","D","A","D","A","C","B","B","C","A","D","A","D",
          "B","C")

freq <- c(10.2,11.2,7.80,11.5,9.50,12.2,11.9,12.4,11.8,10.3,11.3,7.90,12.9,9.40,13.5,10.1)


mydata <- data.frame(Col,Rows,seed,freq)
mydata

table_franc <- matrix(mydata$freq,4,4)
table_franc
class(table_franc)

mean(mydata$freq)

with(mydata, tapply(freq, Rows, mean))  

mean(mydata$freq) # 10.8675

### ANOVA function in R

myfit <- aov(mydata$freq ~ mydata$Col + mydata$Rows + mydata$seed )
summary(myfit)

model.tables(myfit,"means")
model.tables(myfit,"effects")



#############################

DF <- within(mydata,mydata$Col <-relevel(Col,ref = "C4"),mydata$Rows <- relevel(Rows,ref = "R4"),
             mydata$seed <- relevel(seed,ref = "D") )


y <- c(9.3,  9.4,  9.2,  9.7,  9.3,  9.4,  9.6,  9.4,  
       +     9.5, 10.0, 9.6, 9.8, 10.2, 9.7, 9.9, 10.0)

# Latin square:

operators <- as.factor(rep(1:4, each=4))
coupons <- as.factor(rep(1:4, times=4))
tips <- as.factor(c("A", "B", "C", "D", "B", "A", "D", "C", "C", "D", "A", "B", "D", "C", "B", "A"))

data <- data.frame(y, operators, coupons, tips)
data

g <- lm(y~tips + operators + coupons)
anova(g)

treatment.means <- vector()
for (j in c("A","B","C","D")) treatment.means[j] <- mean(y[tips==j])

treatment.means

y.d <- vector()
for(j in c("A","B","C","D")) y.d[tips==j]<- abs(y[tips==j]-median(y[tips==j]))

g.d <- lm(y.d ~ tips)
anova(g.d)

# Graeco-Latin square:


days <- as.factor(c(1,4,2,3, 2,3,1,4, 3,2,4,1, 4,1,3,2))
data <- cbind(y, operators, coupons, tips, days)
h <- lm(y~tips + operators + coupons + days)
anova(h)


View(DF)

DMI.mod3 <- lm(DF$,data = DF)
summary(DMI.mod3)



summary(DMI.mod3)
anova(DMI.mod3)

#####

model1 <- lm(mydata$freq ~ 1+ mydata$Col + mydata$Rows + mydata$seed)
summary(model1)

### Graeco-Latin Square Designs
## Chemical Yield Example
chemical <- data.frame(yield=c(26,16,19,16,13,    18,21,18,11,21,
                               20,12,16,25,13,    15,15,22,14,17,    10,24,17,17,14),
                       time=factor(c("A","B","C","D","E",  "B","C","D","E","A",
                                     "C","D","E","A","B",  "D","E","A","B","C",
                                     "E","A","B","C","D")),
                       catalyst=factor(c("a","b","c","d","e",  "c","d","e","a","b",
                                         "e","a","b","c","d",  "b","c","d","e","a",
                                         "d","e","a","b","c")),
                       batch=gl(5,5), acid=gl(5,1,25))

mean(chemical$yield)

View(chemical)
summary( chem.lm <- lm(yield ~ -1 + ., data=chemical) )




sapply(chemical,class)
save.image()



# nested latin square design in R
result = aov(time~tool+participant:replic+process+replic);


# Larry Winner Replicated Latin square desig in R
student <- rep(1:12,each=4)
color <- c(1:4,1:4,3,1,4,2,
           3,4,2,1,1:4,3,2,4,1,
           1,3,4,2,1:4,3,4,1,2,
           1,4,3,2,3,4,1,2,1,2,4,3)
square <- c(rep(2,4),rep(3,8),rep(2,4),
            rep(1,4),rep(3,4),rep(2,4),
            rep(1,8),rep(3,4),rep(1,4),rep(2,4))
state <- c(1:4,1:4,1:4,4,3,1,2,
           1:4,4,1,2,3,4,1,2,3,4,1,2,3,
           1:4,4,1,2,3,4,1,2,3,3,4,1,2)

dist <- c(350,166,395,407,364,164,308,434,
          378,172,392,400,400,390,386,184,
          390,176,490,450,420,364,192,429,
          395,351,111,435,392,360,180,393,
          350,170,405,238,408,371,217,595,
          385,385,180,420,305,334,377,167)

Y <- rep(0,48)

for (i in 1:48) {
  if (state[i]==1) Y[i] <- abs(100*(dist[i]-377.92)/377.92)
  if (state[i]==2) Y[i] <- abs(100*(dist[i]-183.51)/183.51)
  if (state[i]==4) Y[i] <- abs(100*(dist[i]-347.93)/347.93)
  if (state[i]==3) Y[i] <- abs(100*(dist[i]-394.39)/394.39)
}

ls.data <- data.frame(student,color,dist,square,Y)
attach(ls.data)

color <- factor(color,levels=1:4,labels=c("purple","pink","orange","green"))
state <- factor(state,levels=1:4,labels=c("ME","VTNH","MS","WY"))
student <- factor(student)
square <- factor(square)

table(state,color)

tapply(Y,student,mean)
tapply(Y,state,mean)
tapply(Y,color,mean)
tapply(Y,square,mean)

color.mod1 <- aov(Y~state+square/student+color)
summary(color.mod1)
TukeyHSD(color.mod1,"color")



















