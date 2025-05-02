pi <- 0.157
alpha <- 0.05
n <- 40
w <- 0:n
pi.hat <- w/n
pmf <- dbinom(x = w, size = n, prob = pi)

var.wald <- pi.hat*(1 - pi.hat)/n
lower <- pi.hat - qnorm(p = 1-alpha/2) * sqrt(var.wald)
upper <- pi.hat + qnorm(p = 1-alpha/2) * sqrt(var.wald)
save <- ifelse(test = pi>lower, yes = ifelse(test = pi<upper,
  yes = 1, no = 0), no = 0)

data.frame(w, pi.hat, round(data.frame(pmf, lower, upper), 4),
  save)[1:13,]

sum(save*pmf)
sum(dbinom(x = 4:11, size = n, prob = pi))
