stoplight <- read.csv(file  = "stoplight.csv")
head(stoplight)

mean(stoplight$vehicles)
var(stoplight$vehicles)
table(stoplight$vehicles)

rel.freq <- table(stoplight$vehicles)/length(stoplight$vehicles)
rel.freq2 <- c(rel.freq, rep(0, times = 7))
y <- 0:15
prob <- round(dpois(x = y, lambda = mean(stoplight$vehicles)), 4)
data.frame(y, prob, rel.freq = rel.freq2)

alpha <- 0.05
n <- length(stoplight$vehicles)

mu.hat <- mean(stoplight$vehicles)
mu.hat + qnorm(p = c(alpha/2, 1 - alpha/2))*sqrt(mu.hat/n)

exp(log(mu.hat) + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(1/(mu.hat*n)))
(mu.hat + qnorm(p = c(alpha/2, 1 - alpha/2))/(2*n)) + 
  qnorm(p = c(alpha/2, 1 - alpha/2)) * sqrt((mu.hat + qnorm(p = 1 - alpha/2)/(4*n))/n)

# Plot
dev.new(width = 7, height = 6, pointsize = 12)
# pdf(file = "c:\\figures\\Figure4.1color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
plot(x = y - 0.1, y = prob, type = "h", ylab = "Probability", xlab = "Number of vehicles", lwd = 2,
     xaxt = "n")
axis(side = 1, at = 0:15)
lines(x = y + 0.1, y = rel.freq2, type = "h", lwd = 2, lty = "solid", col = "red")
abline(h = 0)
legend(x = 9, y = 0.15, legend = c("Poisson", "Observed"), lty = c("solid", "solid"), lwd = c(2,2), col = c("black", "red"), bty = "n")
# dev.off()  # Create plot for book
