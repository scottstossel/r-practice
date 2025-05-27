set1 <- data.frame(x1 = c(1,2,3,4,5,6,7,8,9,10),
  y = c(0,0,0,0,0,1,1,1,1,1))
set1

mod.fit.1 <- glm(formula = y ~ x1, data = set1, family = binomial(link = logit),
  trace = TRUE)
mod.fit.1$coefficients

