wheat <- read.csv(file = 'Wheat.csv', stringsAsFactors = TRUE)
head(wheat, n = 3)
tail(wheat, n = 3)

levels(wheat$type)
library(package = nnet)
mod.fit <- multinom(formula = type ~ class + density + hardness + size + weight + moisture,
  data = wheat)

summary(mod.fit)
class(mod.fit)
methods(class = multinom)

summary(wheat)

sd.wheat <- apply(X = wheat[,-c(1,7,8)], MARGIN = 2, FUN = sd)
c.value <- c(1, sd.wheat)
round(c.value, 2)

beta.hat2 <- coefficients(mod.fit)[1, 2:7]
beta.hat3 <- coefficients(mod.fit)[2, 2:7]

#OR for j = 2 (scab vs healthy)
round(exp(c.value*beta.hat2), 2)
round(1/exp(c.value*beta.hat2), 2)

#OR for j = 3 (sprout vs healthy)
round(exp(c.value*beta.hat3), 2)
round(1/exp(c.value*beta.hat3), 2)

class.coef <- coefficients(mod.fit)[, "classsrw"]
sprout.to.scar.or <- unname(exp(class.coef["Sprout"] - class.coef["Scab"]))
sprout.to.scar.or

conf.beta <- confint(object = mod.fit, level = 0.95)
round(conf.beta, 2)

ci.OR2 <- exp(c.value*conf.beta[2:7,1:2,1])
ci.OR3 <- exp(c.value*conf.beta[2:7,1:2,2])
round(data.frame(low = ci.OR2[,1], up = ci.OR2[,2]), 2)

round(data.frame(low = 1/ci.OR2[,2], up = 1/ci.OR2[,2]), 2)

round(data.frame(low = ci.OR3[,1], up = ci.OR3[,2]), 2)
round(data.frame(low = 1/ci.OR3[,2], up = 1/ci.OR3[,2]), 2)

library(car)
Anova(mod.fit.nom)

levels(wheat$type)
wheat$type.order <- factor(wheat$type, levels = c("Scab", "Sprout", "Healthy"))
levels(wheat$type.order)

library(MASS)
mod.fit.ord <- polr(formula = type.order ~ class + density + hardness
                    + size + weight + moisture, data = wheat, method = "logistic")
summary(mod.fit.ord)

Anova(mod.fit.ord)

pi.hat.ord <- predict(object = mod.fit.ord, type = "probs")
head(pi.hat.ord)

head(predict(object = mod.fit.ord, type = "class"))

head(wheat)

round(exp(c.value*(-mod.fit.ord$coefficients)), 2)
round(1/exp(c.value*(-mod.fit.ord$coefficients)), 2)

conf.beta2 <- confint(object = mod.fit.ord, level = 0.95)
conf.beta2

c.value*(-conf.beta2)
c.value[2]*(-conf.beta2[2,])

ci <- exp(c.value*(-conf.beta2))
round(data.frame(low = ci[,2], up = ci[,1]), 2)
round(data.frame(low = 1/ci[,1], up = 1/ci[,2]), 2)

##with 95% confidence, the odds of a scab instead of a sprout or healthy response changes by
#3.87 to 9.36 times when density is decreased by 0.13, holding the other variables constant