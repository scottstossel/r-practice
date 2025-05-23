par(mfrow = c(1,2))

beta0 <- 1
beta1 <- 0.5
curve(expr = exp(beta0 + beta1*x)/(1 + exp(beta0 + beta1*x)),
  xlim = c(-15,15), col = 'black', main = expression(pi
  == frac(e^{1+0.5*x[1]}, 1+e^{1+0.5*x[1]})), xlab = 
  expression(x[1]), ylab = expression(pi))

beta0 <- 1
beta1 <- -0.5
curve(expr = exp(beta0 + beta1*x)/(1 + exp(beta0 + beta1*x)),
  xlim = c(-15,15), col = 'black', main = expression(pi
  == frac(e^{1+0.5*x[1]}, 1+e^{1+0.5*x[1]})), xlab = 
  expression(x[1]), ylab = expression(pi))

####Question 2.3.4
beta0 <- 1
beta1 <- 0.8
curve(expr = exp(beta0 + beta1*x)/(1 + exp(beta0 + beta1*x)),
  xlim = c(-15,15), col = 'black', main = expression(pi
  == frac(e^{1+0.5*x[1]}, 1+e^{1+0.5*x[1]})), xlab = 
  expression(x[1]), ylab = expression(pi))

beta0 <- 1
beta1 <- 0.2
curve(expr = exp(beta0 + beta1*x)/(1 + exp(beta0 + beta1*x)),
  xlim = c(-15,15), col = 'black', main = expression(pi
  == frac(e^{1+0.5*x[1]}, 1+e^{1+0.5*x[1]})), xlab = 
  expression(x[1]), ylab = expression(pi))
