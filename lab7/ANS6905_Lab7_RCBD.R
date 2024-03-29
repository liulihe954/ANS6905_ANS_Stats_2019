install.packages("asbio")
library(asbio)
library(ggplot2)
library(tidyverse)
## Randomized Complete Block Design #################
#####################################################
setwd("/Users/liulihe95/Desktop/ANS6905_ANS_Stats_2019/lab7/")
# read the data from a file
data.block.design <- read.csv("Lab7_CRBD_feedlot_data.csv",header = T)
str(data.block.design)
data.block.design <- dplyr::select(data.block.design,-pen)
# Mean of each treatmnet level
with(data.block.design,tapply(Cumm_DMI,Diet,mean))
# Mean of each block
with(data.block.design,tapply(Cumm_DMI,Blk,mean))
# show data
xtabs(Cumm_DMI ~ Diet + Blk,
       data = data.block.design) # R.C.B.D

# play
data.block.design.test = data.block.design[-c(3,12,17,20),]
xtabs(Cumm_DMI ~ Diet + Blk,
      data = data.block.design.test) # R.C.B.D


# Degree of freedom
df_blk = 5 - 1
df_diet = 5 - 1
df_res = df_blk*df_diet

# fit a Anova model 
model_rcbd <- aov(Cumm_DMI ~ Diet + Blk , data = data.block.design)
summary(model_rcbd)
# Tables of effects
model.tables(model_rcbd) # alternative model: y = mu + τ + α + ε

# Change the order of fitting 
model_rcbd_rev <- aov(Cumm_DMI ~  Blk + Diet  , data = data.block.design)
summary(model_rcbd_rev);summary(model_rcbd) # Balanced design and orthigonal predictors
# t test 
summary(lm(Cumm_DMI ~., data = data.block.design))

# Interaction plot
data.block.design %>% 
  group_by(Blk,Diet) %>% 
  ggplot() +
  aes(x = Diet, y = Cumm_DMI, color = Blk) +
  geom_line(aes(group = Blk)) +
  geom_point() -> interaction.plot
interaction.plot
dev.off()

# Key assumption: No interaction between block and treatment
#We are testing the null hypothesis that there is no treatment effect in any block. 
#H0: Main effects and blocks are additive 
#H1: Main effects and blocks are non-additive.
#install.packages("asbio")
library(asbio)
with(data.block.design, tukey.add.test(Cumm_DMI, Diet, Blk)) # accept the null

# compare treatment means: involving contrats
# tell R which groups to compare
#c1 <- c(1,-0.5,-0.5)  # comparing the average effects of the factor with control
c1 <- c(1,0,-1,0,0) 
c2 <- c(0,1,-1,0,0) # Canola vs Carinata
contrasts(data.block.design$Diet) <- cbind(c1,c2)
moddel.contrast <- aov(Cumm_DMI~.,data.block.design)
# Make sure to use summary.aov here or 'split' might not work
summary.aov(moddel.contrast, 
            split=list(Diet=list("1 VS 3" =1,"2 VS 3" = 2))) 

# se compare: with and without blk

data.block.design <- read.csv("Lab7_CRBD_feedlot_data.csv",header = T)
str(data.block.design)
data.block.design <- dplyr::select(data.block.design,-pen)

summary(lm(Cumm_DMI ~., data = data.block.design))$coefficient[1:5,]
summary(lm(Cumm_DMI ~.-Blk, data = data.block.design))$coefficient


