library(ISLR2)
names(Smarket)

dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[, -9])

attach(Smarket)
plot(Volume)

glm.fits <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Smarket , family = binomial )
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]

glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Direction)
