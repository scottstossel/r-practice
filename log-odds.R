log.odds.to.prob <- function(x) {
  p = exp(x)/(1+exp(x))
  return(p)
}

log.odds.to.prob(10)
log.odds.to.prob(-10)
log.odds.to.prob(2)

loglikelihood <- function(pi) {
  data <- c(1, 0, 1)
  return(sum(data==1) * log(pi) + (sum(data==0)*))
}

