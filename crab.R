crab <- read.csv(file = "HorseshoeCrabs.csv")
str(crab)
summary(crab)

head(crab)

mod.fit <- glm(formula = Sat ~ Width, data = crab,
  family = poisson(link = log))
summary(mod.fit)

#when shell width is 23, a one unit increase leads to 17.8% increase in satellites

ci.mu <- function(newdata, mod.fit.obj, alpha) {
  lin.pred.hat <- predict(object = mod.fit.obj, newdata = newdata,
    type = "link", se = TRUE)
  lower <- exp(lin.pred.hat$fit - qnorm(1 - alpha/2) *
    lin.pred.hat$se)
  upper <- exp(lin.pred.hat$fit + qnorm(1 - alpha/2) *
    lin.pred.hat$se)
  list(lower = lower, upper = upper)
}

Browse[1]> ci.mu(newdata = data.frame(Width = 23), mod.fit.obj = mod.fit, alpha = 0.05)

library(mcprofile)
K <- matrix(c(0, 1), nrow = 1)

linear.combo <- mcprofile(object = mod.fit, CM = K)
ci.logmu.profile <- confint(object = linear.combo, level = 0.95)
ci.logmu.profile
