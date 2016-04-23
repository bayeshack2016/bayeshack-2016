#x is the wage at each percentile
x<- c(9.07, 11.27, 17.40, 28.32, 44.29)

histgen<- function(x){  
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
  
  histogram<- qplot(out, geom="histogram", xlim = c(0, 3*max(out)/4), 
                    fill = I("blue"), binwidth = 0.75,
                    main = "Distribution of Wages",  xlab = "Hourly Wage", ylab = "Frequency")
  return(histogram)  
}
