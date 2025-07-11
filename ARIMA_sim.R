set.seed(898)
x1 <- w <- rnorm(100)
for (i in 3:100) x1[i] <- 0.5*x1[i-1] + x1[i-1] -0.5*x1[i-2] + w[i] + 0.3*w[i-1]
str(x1)
summary(x1)

plot.ts(x1, main = "Simulated Time Series x1", ylab = "x1")

diffx1 <- diff(x1)
plot.ts(diffx1, main = "Simulated Time Series x1", ylab = "x1")

acf(x1)
pacf(x1)
acf(diffx1)
pacf(diffx1)

fit1 <- arima(x1, order = c(1,1,1))
summary(fit1)

plot.ts(fit1$residuals)
hist(fit1$residuals)
acf(fit1$residuals)
pacf(fit1$residuals)

ljung_box(fit1$residuals)

