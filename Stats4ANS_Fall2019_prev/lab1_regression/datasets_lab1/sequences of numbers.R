?`:`
 seq(1,100,by=2) 
 
my_seq <- seq(5,10,length=30)
my_seq
 
length(my_seq)

1:length(my_seq)
seq(along.with = my_seq)
#Simple approaches that involve less typing are generally best

# replicate the values as many times as you want

rep(0,times=40)
rep(c(0, 1, 2), times = 10)

rep(1:5,times=10) # you can repeat a vector 10 times in R

rep(1:5,each=10)
