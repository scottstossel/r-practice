df <- read.table(file = 'Placekick.csv', header = TRUE, sep = ",")
str(df)

table(df$good)
prop.table(table(df$good))

head(df)

mod.fit <- glm(formula = good ~ distance, family = binomial(link = logit), data = df)
mod.fit

names(mod.fit)
length(mod.fit$coefficients)
mod.fit$coefficients
mod.fit$coefficients[1]
mod.fit$coefficients[2]

summary(object = mod.fit)

class(mod.fit)
methods(class = glm)

summary(df$distance)

curve(expr = exp(mod.fit$coefficients[1] + mod.fit$coefficients[2]*x) /
      (1 + exp(mod.fit$coefficients[1] + mod.fit$coefficients[2]*x)),
    col = "red", xlim = c(18,66), ylab = expression(hat(pi)), xlab = "Distance",
  main = "Estimated probability of success for a placekick",
  panel.first = grid())

mod.fit2 <- glm(formula = good ~ change + distance, family = binomial(link = logit), data = df)
summary(mod.fit2)

curve(expr = exp(mod.fit2$coefficients[1] + mod.fit2$coefficients[2]*x) /
        (1 + exp(mod.fit2$coefficients[1] + mod.fit2$coefficients[2]*x)),
      col = "red", xlim = c(18,66), ylab = expression(hat(pi)), xlab = "Distance",
      main = "Estimated probability of success for a placekick",
      panel.first = grid())

###Variance-Covariance Matrix
vcov(mod.fit2)


