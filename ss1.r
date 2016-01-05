ss <- function(prevalence, precision, alpha){
  p=prevalence; d=precision;
  # formula
  # std.normal variate value for 95% CI
  z <- (qnorm((1-alpha)/2))^2
  n <- (z*p*(1-p))/d^2
  return(n)
}

# sample size for prevalences from 40% to 60%
sampleSize <- ss(prevalence = seq(0.1, 0.9, by=0.01), 
   precision = 0.05, 
   alpha=0.95)

# plot sample size as a function of varying prevalences

plot(seq(0.1, 0.9, by=0.01), sampleSize, 
     xlab="Prevalences", 
     ylab="Sample Size", 
     xlim= c(0,1),
     ylim=c(100, 500),
     type="l",
     col="blue", 
     lwd=2)
grid()
abline(v=0.5)
