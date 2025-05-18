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

###Likelihood Ratio Test
library(car)
Anova(mod = mod.fit2, test = "LR")

mod.fit.H0 <- glm(formula = good ~ distance, family = binomial(link = logit), data = df)
anova(mod.fit.H0, mod.fit2, test = "Chisq")

dframe <- mod.fit.H0$df.residual-mod.fit2$df.residual

stat <- mod.fit.H0$deviance - mod.fit2$deviance
pvalue <- 1 - pchisq(q = stat, df = dframe)
data.frame(H0.resid.dev = mod.fit.H0$deviance,
  Ha.resid.dev = mod.fit2$deviance, df = dframe, stat = 
  round(stat, 4), pvalue = round(pvalue, 4))

###Odds ratios
exp(mod.fit$coefficients[2])

1/exp(10*mod.fit$coefficients[2])

##interpretation: estimated odds of success change by 3.16 times for every 10 yd decrease in distance

# accounting for variability in OR
beta.ci <- confint(object = mod.fit, parm = "distance", level = 0.95)

beta.ci
rev(1/exp(beta.ci*10))
as.numeric(rev(1/exp(beta.ci*10)))
#95% profile likelihood ratio CI is 2.69 < OR < 3.74


###Estimate a logistic regression model from the data

placekick <- read.table(file = 'Placekick.csv', header = TRUE, sep = ",")
w <- aggregate(x = good ~ distance, data = placekick, FUN = sum)
n <- aggregate(x = good ~ distance, data = placekick, FUN = length)
w.n <- data.frame(distance = w$distance, success = w$good,
  trials = n$good, proportion = round(w$good/n$good, 4))
head(w.n)

plot(x = w$distance, y = w$good/n$good, main = "Estimated Prob vs Distance",
  xlab = "Distance (yards)", ylab = "Estimated probability",
  panel.first = grid(col = "gray", lty = "dotted"))

##bubble plot
symbols(x = w$distance, y = w$good/n$good, circles = sqrt(n$good),
  inches = 0.5, main = "Estimated Prob vs Distance",
  xlab = "Distance (yards)", ylab = "Estimated probability",
  panel.first = grid(col = "gray", lty = "dotted"))
curve(expr = predict(object = mod.fit, newdata = 
  data.frame(distance = x), type = "response", col = "blue",
  add = TRUE, xlim = c(18,66)))
