pi.ij <- c(0.2,0.3,0.2,0.1,0.1,0.1)
pi.table <- array(data = pi.ij, dim = c(2,3), dimnames = 
  list(X = 1:2, Y = 1:3))
pi.table

set.seed(9812)
save <- rmultinom(n = 1, size = 1000, prob = pi.ij)
c.table1 <- array(data = save, dim = c(2,3), dimnames = 
  list(X = 1:2, Y = 1:3))
c.table1

c.table1/sum(c.table1)