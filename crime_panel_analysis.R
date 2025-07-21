library(wooldridge)

data("crime2")
head(crime2)

hist(crime2$crmrte, xlab = "Crime Rate", ylab = "Count", main = "", breaks = 30)

model <- lm(crmrte ~ unem, data = crime2)

par(mfrow = c(2,2))
plot(model)

crime.82 <- crime2[crime2$year == 82,]
model.82 <- lm(crmrte ~ unem, data = crime.82)
summary(model.82)
plot(model.82)

crime.87 <- crime2[crime2$year == 87,]
model.87 <- lm(crmrte ~ unem, data = crime.87)
summary(model.87)

plot(model.87)

pooled.ols.fit <- lm(crmrte ~ d87+unem, data = crime2)
summary(pooled.ols.fit)
