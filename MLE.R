sum.y <- 4
n <- 10
#different values of pi

pi <- c(0.2,0.3,0.35,0.39,0.4,0.41,0.5)
Lik <- pi^sum.y*(1-pi)^(n-sum.y)
data.frame(pi, Lik)

#likelihood function plot
curve(expr = x^sum.y*(1-x)^(n-sum.y), xlim = c(0,1),
      xlab = expression(pi), ylab = "Likelihood
      function", main = "Likelihood Function of Binomial Probability Model")
