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
