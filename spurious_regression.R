set.seed(14)

x <- y <- rnorm(1000)
cor(x,y)
head(cbind(x,y))
mean(x-y)
sd(x-y)

y<- rnorm(1000)
cor(x,y)

for (i in 2:1000) {
  x[i] <- x[i-1] + rnorm(1)
  y[i] <- y[i-1] + rnorm(1)
}

cor(x,y)
head(cbind(x,y), 10)

par(mfrow=c(2,2))
plot.ts(x); title("Time-Series Plot of X")
plot.ts(y); title("Time-Series Plot of Y")
ts.plot(ts(x),ts(y)); title("Time-Series Plot of X and Y")
plot(x,y); title("X vs Y")
dev.off()

par(mfrow=c(2,2))
acf(x, main=""); title("ACF of X")
acf(y, main=""); title("ACF of Y")
pacf(x, main=""); title("PACF of X")
pacf(y, main=""); title("PACF of Y")
dev.off()

