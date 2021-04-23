library(faraway)
data(fat)
attach(fat)
plot(neck ~ knee)
plot(hip ~ wrist)
# Use PC

(cfat <- fat[,9:18])
prfat <- prcomp(cfat)
dim(prfat$)
