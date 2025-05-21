library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy = FALSE)

# Start with a clean R environment
rm(list = ls())
# Load libraries
## Load a set of packages inclusing: broom, cli, crayon, dbplyr , dplyr, dtplyr, forcats,
## googledrive, googlesheets4, ggplot2, haven, hms, httr, jsonlite, lubridate , magrittr,
## modelr, pillar, purrr, readr, readxl, reprex, rlang, rstudioapi, rvest, stringr, tibble,
## tidyr, xml2
if(!"tidyverse"%in%rownames(installed.packages())) {install.packages("tidyverse")}
library(tidyverse)
## to load glow500 from "Applied Logistic Regression" by D.W. Hosmer, S. Lemeshow and R.X. Sturdivant (3rd ed., 2013)
if(!"aplore3"%in%rownames(installed.packages())) {install.packages("aplore3")}
library(aplore3)
## provides many functions useful for data analysis, high-level graphics, utility operations like describe()
if(!"Hmisc"%in%rownames(installed.packages())) {install.packages("Hmisc")}
library(Hmisc)
## to work with "grid" graphics
if(!"gridExtra"%in%rownames(installed.packages())) {install.packages("gridExtra")}
library(gridExtra)
## To generate regression results tables and plots
if(!"finalfit"%in%rownames(installed.packages())) {install.packages("finalfit")}
library(finalfit)
## To produces LaTeX code, HTML/CSS code and ASCII text for well-formatted tables
if(!"stargazer"%in%rownames(installed.packages())) {install.packages("stargazer")}
library(stargazer)

df_0 <- data.frame(y = c(0,0,0,0,1,1,1,1), x1 = c(1,2,3,3,5,6,10,11), x2 = c(3,2,-1,-1,2,4,1,0))
#plot(df_0$x1, df_0$y)

mod.logit.complete<- glm(y~ x1+x2, family=binomial(link = logit), data = df_0)
summary(mod.logit.complete)

df = glow500 %>%
  dplyr::select(fracture, age, priorfrac, premeno, raterisk, smoke, bmi)
head(df) %>%
  knitr::kable()
df %>%
  count(fracture) %>%
  mutate(prop = round(prop.table(n),2)) %>%
  kable(col.names = c('Fracture', 'N', "Proportion"))
df %>%
  ggplot(aes(x= fracture, y = ..prop.., group = 1)) +
  geom_bar(fill = 'DarkBlue', color = 'black') +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  xlab("Fracture") +
  ylab("Proportion") +
  ylim(0,1)

dependent <- "fracture"
explanatory <- c("bmi","age", "priorfrac", "premeno", "raterisk", "smoke")
df %>%
  summary_factorlist(dependent, explanatory, add_dependent_label = TRUE) %>%
  knitr::kable()

mod.logit.1 <- glm(fracture ~ df$bmi + df$age, family=binomial(link = logit),
  data = df)

summary(mod.logit.1)
exp(mod.logit.1$coefficients[3]*10)

mod.logit.2 <- glm(fracture ~ df$bmi + df$age + df$priorfrac + df$raterisk
  , family=binomial(link = logit), data = df)
levels(df$priorfrac)
levels(df$raterisk)

df$priorfrac<-relevel(df$priorfrac, ref="No")
df$raterisk<-relevel(df$raterisk, ref="Less")
summary(mod.logit.2)

odds_priorfracYes_No <- exp(coef(mod.logit.2))
odds_priorfracYes_No 
#oods_rateriskSame_Less <- # uncomment and replace with your code
#oods_rateriskSame_Less # uncomment
#oods_rateriskGreater_Less <- # uncomment and rep

mod.logit.3 <- glm(fracture ~ df$bmi + df$age + df$priorfrac + df$raterisk +
  df$age:df$priorfrac + df$age:df$raterisk, 
  family=binomial(link = logit), data = df)
summary(mod.logit.3)

beta.hat <- mod.logit.3$coefficients
beta.hat
c <- 10
prior_fracture <- c(0,1)
log.OR.age <- c(beta.hat[2]+beta.hat[7]*prior_fracture)
OR.age <- exp(log.OR.age)
data.frame(prior_fracture = prior_fracture, OR.hat = OR.age)


mod.logit.4 <- glm(fracture ~ age + priorfrac + raterisk + age:priorfrac, family = binomial(link = logit),
  data = df)

anova(mod.logit.4, mod.logit.3, test = "Chisq")

beta.hat4 <- mod.logit.4$coefficients
c <- 1
age <- seq(from = 55, to = 85, by = 10)

log.OR.prior_fracture <- c * (beta.hat4[3] + beta.hat4[6] * age)
OR.prior_fracture <- exp(log.OR.prior_fracture)

cov.mat <- vcov(mod.logit.4)
var.log.OR <- c^2 * (cov.mat[3,3] + age^2 * cov.mat[6,6] + 2 * age * cov.mat[3,6])

ci.log.OR.low <- exp(log.OR.prior_fracture - qnorm(p = 0.975,)*sqrt(var.log.OR))
ci.log.OR.up <- exp(log.OR.prior_fracture + qnorm(p = 0.975,)*sqrt(var.log.OR))

round(data.frame(age = age, OR.hat = OR.prior_fracture, OR.low = ci.log.OR.low, OR.up = ci.log.OR.up), 2)
