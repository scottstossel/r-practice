placekick <- read.table(file = 'Placekick.csv', header = TRUE, sep = ",")
str(placekick)
head(placekick)

mod.fit.Ha <- glm(formula = good ~ distance + wind +
  distance:wind, family = binomial(link = logit), data = 
  placekick)
summary(mod.fit.Ha)

#Likelihood-Ratio Test
mod.fit.Ho <- glm(formula = good ~ distance + wind, family 
  = binomial(link = logit), data = placekick)
anova(mod.fit.Ho, mod.fit.Ha, test = "Chisq")

library(car)
Anova(mod.fit.Ha, test = "LR")


with.interaction<-function(dmin = 20, dmax = 60) {
  wind0<-function(d) {
    new.data <- data.frame("distance" = d, "wind" = 0)
    pred.prob <- predict(mod.fit.Ha, newdata = new.data, type = "response")
    return(pred.prob)
  }
  wind1<-function(d) {
    new.data <- data.frame("distance" = d, "wind" = 1)
    pred.prob <- predict(mod.fit.Ha, newdata = new.data, type = "response")
    return(pred.prob)
  }
  curve(wind0, from = dmin, to = dmax, xlab = "Distance", ylab = "Est Prob(Good)", lwd = 2,
        col = "tomato2", main = "With Interaction")
  curve(wind1, from = dmin, to = dmax, xlab = "Distance", ylab = "Est Prob(Good)", lwd = 2,
        col = "steelblue3", add = T)
}

with.interaction()

round(data.frame(wind = wind, OR.hat = 1/OR.dist, OR.low
  = 1/exp(ci.log.OR.up), OR.up = 1/exp(ci.log.OR.low)), 2)

##quadratic term
mod.fit.distsq <- glm(formula = good ~ distance + I(distance^2), family = binomial(link = logit),
  data = placekick)
summary(mod.fit.distsq)

Anova(mod.fit.distsq)

#plot quadratic vs non quadratic
glm.model.linear <- glm(formula = good ~ distance, family = binomial(link = logit), data = placekick)
glm.model.quad <- glm(formula = good ~ distance + I(distance^2), family = binomial(link = logit),
  data = placekick)
quad.comp<-function(dmin = 20, dmax = 60) {
  without.quad<-function(d) {
    new.data <- data.frame("distance" = d)
    pred.prob <- predict(glm.model.linear, newdata = new.data, type = "response")
    return(pred.prob)
  }
  with.quad<-function(d) {
    new.data <- data.frame("distance" = d)
    pred.prob <- predict(glm.model.quad, newdata = new.data, type = "response")
    return(pred.prob)
  }
  curve(without.quad, from = dmin, to = dmax, xlab = "Distance", ylab = "Est Prob(Good)", lwd = 2,
        col = "tomato2", main = "With Interaction")
  curve(with.quad, from = dmin, to = dmax, xlab = "Distance", ylab = "Est Prob(Good)", lwd = 2,
        col = "steelblue3", add = T)
}

par(mfrow=c(1,1))
quad.comp()
