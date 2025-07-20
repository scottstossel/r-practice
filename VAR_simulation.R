library(ggplot2)
library(mvtnorm)
library(GGally)

cov.mat <- matrix(c(1,0.8,0.8,1), nr=2)
w <- rmvnorm(1000, sigma = cov.mat)
dim(w)
head(w)
tail(w)


layout(c(1,1))
plot(w, col="navy",
     main = "Simulated Observations from a Bivariate Normal Distribution")

cov(w)
wx <- w[,1]
wy <- w[,2]

ccf(wx,wy, main = "Cross-Correlation Function of Bivariate White Noise")

# simulate a VAR(1) process
x <- y <- rep(0,1000)
# initial value used in the recursive formula
x[1] <- wx[1]
y[1] <- wy[1]
for (i in 2:1000) {
  x[i] = 0.4*x[i-1] + 0.3*y[i-1] + wx[i]
  y[i] = 0.2*x[i-1] + 0.1*y[i-1] + wy[i]
}

df <- data.frame(cbind(x,y))
str(df)
ccf(x,y, main="Cross-Correlation Function of a VAR(1) Process")

ggpairs(df)
dev.off()

layout(1:1)
ts.plot(ts(x), ts(y),
        gpars = list(
          xlab="Simulated Time Period",
          ylab="Series Values",
          lty=c(1:3), pch=c(1,4),
          col=c("blue","black","navy")
        ))
title("Simulated Bivariate Time Series")
leg.txt <- c('x','y')
legend('topright', leg.txt, lty = 1, col = c('blue', 'black'), bty = 'n', cex = .75)
dev.off()

par(mfrow=c(2,2))
acf(x); acf(y)
pacf(x); pacf(y)
dev.off()

ccf(x,y, main="Cross-Correlation of X and Y")
dev.off()

# estimate a VAR(1) model
xy.ar <- ar(cbind(x,y))
xy.ar$ar[, , ]

