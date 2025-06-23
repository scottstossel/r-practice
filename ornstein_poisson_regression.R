library(car)
library(Hmisc)

str(Ornstein)
head(Ornstein)

summary(Ornstein)
describe(Ornstein)

round(with(Ornstein, tapply(interlocks, nation, mean)), 1)
round(with(Ornstein, tapply(interlocks, sector, mean)), 1)

tab <- xtabs(~ interlocks, data = Ornstein)
str(tab)
class(tab)
nrow(tab)
names(tab)
tab

x <- as.numeric(names(tab))

plot(x, tab, xlab = "", ylab = "")
title("Frequency Distribution of Interlocks", xlab = "Number of Interlocks",
      ylab = "Frequency")

plot(x, tab, main = "Number of Interlocks Maintained by 248 Canadian
     Corporations", xlab = "Number of Interlocks", ylab = "Frequency")
points(x, tab, pch=16)

with(Ornstein, hist(assets, breaks="FD", col = "navy",
                    main = "Distribution of Assets ($millions)"))

box()
with(Ornstein, hist(log2(assets), breaks="FD", col = "navy",
                    main = "Distribution of Assets ($millions)"))

scatterplotMatrix(~ assets + nation + sector, data = Ornstein)
scatterplotMatrix(~ log2(assets) + nation + sector, data = Ornstein)

Boxplot(assets ~ nation, data = Ornstein, main = "Assets by Nation",
        col.axis = "blue")
Boxplot(assets ~ sector, data = Ornstein, main = "Assets by Sector",
        col.axis = "blue")

summary(Ornstein$assets)
quantile(Ornstein$assets, 0.95)

Boxplot(assets[assets<20000] ~ nation[assets<20000], data = Ornstein,
        main = "Assets by Nation (Assets < $20B")

poisson.mod1 <- glm(interlocks ~ log2(assets) + nation + sector,
                    family = poisson, data = Ornstein)
summary(poisson.mod1)
with(Ornstein, table(nation, sector))

#analysis of deviance
Anova(poisson.mod1)

#interpretation of the coefficients
exp(coef(poisson.mod1))
#us holds 46% as many interlocks as a Canadian firm

library(effects)
plot(allEffects(poisson.mod1, default.levels=50))

library(car)
residualPlots(poisson.mod1, layout=c(2,2))
