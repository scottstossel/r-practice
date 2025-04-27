dbinom(x = 1, size = 5, prob = 0.6)
dbinom(x = 0:5, size = 5, prob = 0.6)

pmf <- dbinom(x = 0:5, size = 5, prob = 0.6)
pmf.df <- data.frame(w = 0:5, prob = round(x = pmf, digits = 4))
pmf.df

###plot results
plot(x = pmf.df$w, y = pmf.df$prob, type = "h", xlab = "w",
     ylab = "P(W=w)", main = "Plot of a binomial PMF for n=5, pi=0.6",
     panel.first = grid(col = "gray", lty = "dotted"),
     lwd = 2)
abline(h = 0)

pmf_2 <- dbinom(x = 0:10, size = 10, prob = 0.2)
pmf_2.df <- data.frame(w = 0:10, prob = round(x = pmf_2, digits = 4))
pmf_2.df

###plot results
plot(x = pmf_2.df$w, y = pmf_2.df$prob, type = "h", xlab = "w",
     ylab = "P(W=w)", main = "Plot of a binomial PMF for n=10, pi=0.2",
     panel.first = grid(col = "gray", lty = "dotted"),
     lwd = 2)
abline(h = 0)

pmf_3 <- dbinom(x = 0:10, size = 10, prob = 0.8)
pmf_3.df <- data.frame(w = 0:10, prob = round(x = pmf_3, digits = 4))
pmf_3.df

###plot results
plot(x = pmf_3.df$w, y = pmf_3.df$prob, type = "h", xlab = "w",
     ylab = "P(W=w)", main = "Plot of a binomial PMF for n=10, pi=0.8",
     panel.first = grid(col = "gray", lty = "dotted"),
     lwd = 2)
abline(h = 0)