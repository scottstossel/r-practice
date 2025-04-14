library(MASS)
library(ISLR2)

head(Boston)
?Boston

lm.fit <- lm(medv ~ lstat, data = Boston)
attach(Boston)
lm.fit <- lm(medv ~ lstat)

lm.fit
summary(lm.fit)

names(lm.fit)
coef(lm.fit)

confint(lm.fit)

predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "confidence")
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
        interval = "prediction")
