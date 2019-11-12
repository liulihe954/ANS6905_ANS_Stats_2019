 library(tidyverse)
 library(car)
 ##======================================###
 ##===   single Latin Square Design   ===###
 ##======================================###
setwd("/Users/liulihe95/Desktop/ANS6905_ANS_Stats_2019/Lab8")
Milk_raw <- read.csv("LAB8_LSD_MilkYield.csv",header =T,as.is = T) 
names(Milk_raw)
str(Milk_raw)
# massage: Pipe all the changes you need
Milk_data = Milk_raw %>% 
  dplyr::mutate_at(c("Period","Trt","Cow"), funs(as.factor(.))) %>% 
  dplyr::mutate_at(c("Cow"), funs(dplyr::recode(.,`1`= "A", `2`= "B",`3`= "C",`4`= "D")))
# data ready to go
str(Milk_data)

# View
xtabs(cbind(MilkYield,Trt)~ Cow + Period ,
      data = Milk_data) # 1-2-3-4, is A B C D in cows
# show experimental unit assignment
ftable(Period ~  Trt + Cow, data = Milk_data)

# fit anova
LSD_data_milk = aov(MilkYield ~ Cow + Period + Trt, data = Milk_data)
summary(LSD_data_milk)
summary(lm(MilkYield ~.,Milk_data))

# type 3 anova
anova(LSD_data_milk)
Anova(LSD_data_milk, type="III")

# 
TukeyHSD(LSD_data_milk,"Trt")


##======================================###
##=== Replicated Latin Square Design ===###
##======================================###
setwd("/Users/liulihe95/Desktop/ANS6905_ANS_Stats_2019/Lab8")
# read in raw
ADG_raw <- read.csv("LAB8_Duplicate_LatinSquare.csv",header =T)
str(ADG_raw)
table(ADG_raw$TRT)
# massage
ADG_data = ADG_raw %>% 
  dplyr::mutate_at(c("Pen","Square","Period","TRT"), funs(as.factor(.))) %>% 
  dplyr::mutate_at(c("Square"), funs(dplyr::recode(.,"1" = "A","2"= "B"))) %>% 
  dplyr::mutate_at(c("TRT"), funs(dplyr::recode(.,"0" = "a",
                                                  "10"= "b",
                                                  "20"= "c",
                                                  "30"= "d")))
# date ready to go
str(ADG_data)

# View the data
xtabs(cbind(ADG,Pen)~ Pen + Period + Square,
      data = ADG_data) 
# show experimental unit assignment
ftable(Period ~ Square + TRT +Pen,data = ADG_data)

# Model
ADG_mod <- aov(ADG ~  Square + Period + Square/Pen + TRT, data = ADG_data)# Pen nested in Square
summary(lm(ADG ~TRT + Square + Period + Square/Pen,data = ADG_data))
summary(ADG_mod)

# orthogonal: SS remain teh same
ADG_mod_2 <- aov(ADG ~TRT + Square + Period + Square/Pen, data = ADG_data) 
summary(ADG_mod_2)
ADG_mod_3 <- aov(ADG ~  Period + TRT + Square/Pen + Square, data = ADG_data)
summary(ADG_mod_3)


# check for interactions
ADG_mod_SxT <- aov(ADG ~  Square:TRT + Square + Period + Square/Pen + TRT , data = ADG_data)
summary(ADG_mod_SxT)

# treatment effect
TukeyHSD(ADG_mod,"TRT")


