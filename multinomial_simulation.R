pi.j <- c(0.25,0.35,0.2,0.1,0.1)
set.seed(2195)
n.j <- rmultinom(n = 1, size = 1000, prob = pi.j)
data.frame(n.j, pihat.j = n.j/1000, pi.j)

#suppose there are m = 5 separate sets of n =1000 trials
set.seed(9182)
n.j <- rmultinom(n = 5, size = 1000, prob = pi.j)
data.frame(n.j, pihat.j = n.j/1000, pi.j)
