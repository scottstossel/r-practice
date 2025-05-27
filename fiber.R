diet <- read.csv(file = 'Fiber.csv')
head(diet)

#match order given at DASL
diet$fiber <- factor(x = diet$fiber, levels = c("none", "bran", "gum", "both"))
diet$bloat <- factor(x = diet$bloat, levels = c("none", "low", "medium", "high"))

diet.table <- xtabs(formula = count ~ fiber + bloat, data = diet)
diet.table

#test independence w/ Pearson Chi-squared test
ind.test <- chisq.test(x = diet.table, correct = FALSE)
ind.test

library(package = vcd)
assocstats(x = diet.table)

ind.test$expected

library(nnet)
mod.fit.nom <- multinom(formula = bloat ~ fiber, weights =
  count, data = diet)

summary(mod.fit.nom)