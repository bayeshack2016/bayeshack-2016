library(ggplot2)
library(reshape2)
library(plyr)

#x is the wage at each percentile
x<- c(9.07, 11.27, 17.40, 28.32, 44.29)
y<- c(8.33,  9.73,	14.61,	22.59, 33.23)

distribution_generator<- function(x){  
  cum.p <- c(.1, .25, .5, .75, .9)
  prob <- c( cum.p[1], diff(cum.p), .1)
  freq <- 10000 
  len<-100
  # range of values beyond x to sample from
  range <- c(0, x, 2*max(x))
  s <- sapply(2:length(range), function(i) {
    seq(range[i-1], range[i], length.out=len)
  })
  # sample from s, total of freq values, with probabilities from prob 
  out <- sample(s, freq, prob=rep(prob, each=len), replace = T)
  return(out)  
}

dist_x<- distribution_generator(x)
dist_y<- distribution_generator(y)
dat<- melt(data.frame(dist_x, dist_y))

mean_dat <- ddply(dat, "variable", summarise, wage.mean=mean(value))

ggplot(dat, aes(x = value, fill = variable)) + 
  geom_histogram(xlim = c(0, 3*max(max(dist_x), max(dist_y))/4), binwidth = 0.75, alpha = 0.5)+
  labs(title = "Distribtuion of Wages", x = "Wage", y= "Frequency")+
  geom_vline(data=mean_dat, aes(xintercept=wage.mean,  colour=variable))
