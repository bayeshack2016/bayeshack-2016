#x is the wage at each percentile
x<- c(9.07, 11.27, 17.40, 28.32, 44.29)
cum.p <- c(.1, .25, .5, .75, .9)
prob <- c( cum.p[1], diff(cum.p), .1)


freq <- 10000 
# range of values beyond x to sample from
init <- -(abs(min(x)) + 5)
fin  <- 2*abs(max(x)) + 5

ival <- c(init, x, fin) # generate the sequence to take pairs from
len <- 100 # sequence of each pair
s <- sapply(2:length(ival), function(i) {
  seq(ival[i-1], ival[i], length.out=len)
})
# sample from s, total of 10000 values with probabilities calculated above
out <- sample(s, freq, prob=rep(prob, each=len), replace = T)
hist(out)
