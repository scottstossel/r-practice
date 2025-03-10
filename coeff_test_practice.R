library(tidyverse)
library(sandwich)
library(lmtest)

d <- data.frame(
  x_1 = runif(1000),
  x_2 = runif(1000),
  x_3 = runif(1000),
  x_4 = runif(1000)) %>%
  mutate(
    y = -1 + 1*x_1 + 2*x_2 + 0*x_3 + 0*x_4 + rnorm(1000)
  )

model_short <- lm(y ~ 1, data = d)
model_long_a <- lm(y ~ 1 + x_1 + x_2, data=d)
model_long_b <- lm(y ~ 1 + x_1 + x_2 + x_3 + x_4, data=d)

coeftest(model_short, vcov. =  vcovHC(model_short))

sqrt(diag(vcovHC(model_long_a)))
coeftest(model_long_a, vcov. =  vcovHC(model_long_a))

coeftest(model_long_b, vcov. =  vcovHC(model_long_b))
