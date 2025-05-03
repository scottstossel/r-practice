c.table <- array(data = c(251, 48, 34, 5), dim = c(2,2),
  dimnames = list(First = c("made", "missed"), Second = 
  c("made", "missed")))
c.table
rowSums(c.table)
pi.hat.table <- c.table/rowSums(c.table)
pi.hat.table

alpha <- 0.05
pi.hat1 <- pi.hat.table[1,1]
pi.hat2 <- pi.hat.table[2,1]

#Wald
var.wald <- pi.hat1*(1-pi.hat1)/sum(c.table[1,]) + pi.hat2*(1-pi.hat2)/sum(c.table[2,])

####95% Wald confidence interval
pi.hat1 - pi.hat2 + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.wald)

###Agresti-Caffo
pi.tilde1 <- (c.table[1,1] + 1) / (sum(c.table[1,]) + 2)
pi.tilde2 <- (c.table[2,1] + 1) / (sum(c.table[2,]) + 2)
var.AC <- pi.tilde1*(1-pi.tilde1) / (sum(c.table[1,]) + 2) +
  pi.tilde2*(1-pi.tilde2) / (sum(c.table[2,]) + 2)
pi.tilde1 - pi.tilde2 + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.AC)
### 95% Agresti-Caffo CI is -0.10353254 < pi_1 - pi_2 <  0.07781192