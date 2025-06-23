set.seed(898)
sigma_w = 10
beta0 = 1
beta1 = 0.5
t = seq(1,500)
w <- rnorm(500,0,sigma_w)
x2 <- beta0 + beta1*t + w
cbind(t, x2, w)
summary(x2)
mean(x2)
sd(x2)

set.seed(898)
beta1 <- 0.8
x4 <- w <- rnorm(500,0,sigma_w)

for (t in 2:500) x4[t] <- w[t] + beta1*w[t-1]
summary(x4)
sd(x4)

rho <- function(k, alpha) alpha^k
plot(0:10, rho(0:10, 0.7), type = "b", main = "rho=0.7")
plot(0:10, rho(0:10, -0.7), type = "b", main = "rho=-0.7")
