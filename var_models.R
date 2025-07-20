library(ggplot2)
library(mvtnorm)
library(GGally)
library(vars)
library(astsa)

x = cbind(cmort, tempr, part)
str(x)
head(x)
time(cmort)
plot.ts(x, main="", xlab="")

summary(VAR(x, p=1, type = "both"))
VARselect(x, lag.max = 10, type = "both")
summary(fit <- VAR(x, p=2, type="both"))

acf(resid(fit), 52)
serial.test(fit, lags.pt = 12, type="PT.adjusted")

(fit.pr = predict(fit, n.ahead = 24, ci = 0.95))
fanchart(fit.pr)

(fit.pr = predict(fit, n.ahead = 48, ci = 0.95))
fanchart(fit.pr)
