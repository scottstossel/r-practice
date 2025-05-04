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

numb.bin.samples <- 1000 #binomial samples of size n

set.seed(4516)
w <- rbinom(n = numb.bin.samples, size = n, prob = pi)
pi.hat <- w/n
var.wald <- pi.hat*(1 - pi.hat)/n
lower <- pi.hat - qnorm(p = 1-alpha/2) * sqrt(var.wald)
upper <- pi.hat + qnorm(p = 1-alpha/2) * sqrt(var.wald)

data.frame(lower, upper)[1:10,]
save[1:10]
mean(save)


pi1 <- 0.2
pi2 <- 0.4
n1 <- 10
n2 <- 10

#possible combinations of w1 and w2
w.all <- expand.grid(w1 = 0:n1, w2 = 0:n2)

#all combinations of pi.hat1 and pi.hat2
pi.hat1 <- (0:n1)/n1
pi.hat2 <- (0:n2)/n2
pi.hat.all <- expand.grid(pi.hat1 = pi.hat1, pi.hat2 = pi.hat2)

#joint probability for w1 and w2
prob.w1 <- dbinom(x = 0:n1, size = n1, prob = pi1)
prob.w2 <- dbinom(x = 0:n2, size = n2, prob = pi2)
prob.all <- expand.grid(prob.w1 = prob.w1, prob.w2 = prob.w2)
pmf <- prob.all$prob.w1*prob.all$prob.w2

head(data.frame(w.all, pmf = round(pmf, 4)))

#calculate true confidence
var.wald <- pi.hat.all[,1] * (1-pi.hat.all[,1]) / n1 +
  pi.hat.all[,2] * (1-pi.hat.all[,2]) /n2
lower <- pi.hat.all[,1] - pi.hat.all[,2] - qnorm(p = 1 - alpha/2) * sqrt(var.wald)
upper <- pi.hat.all[,1] - pi.hat.all[,2] + qnorm(p = 1 - alpha/2) * sqrt(var.wald)
save <- ifelse(test = pi-pi2 > lower, yes = ifelse(test = 
  pi1-pi2 < upper, yes = 1, no = 0), no = 0)
sum(save*pmf)
data.frame(w.all, round(data.frame(pmf, lower, upper), 4),
  save)[1:15,]
