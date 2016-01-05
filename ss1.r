ss <- function(prevalence, precision, alpha){
  p=prevalence; d=precision;
  z <- (qnorm((1-alpha)/2))^2
  n <- (z*p*(1-p))/d^2
  n
}