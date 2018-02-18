## Load pkg
library(broom)
library(dplyr)
library(tidyr)

# Subset model ------------------------------------------------------------

pregint <- read.csv("data/pregint.csv")
source("R/relevel.R")

subGroup <- !is.na(pregint$pregFeel)
subdata <- pregint[subGroup, ]
rm(pregint)

attach(subdata)
source("R/relevel.R")

## Models
modeldf <- cbind.data.frame(age, as.numeric(childnum), sex, hispanic,
    idealCrit, avoidPreg, pregPlan, pregFeel, currentSit, race, relationship,
    incCat)

names(modeldf) <- c("Age", "childnum", "sex", "Hispanic", "idealCrit",
    "avoidPreg", "pregPlan", "pregFeel", "currentSit", "Race", "Relationship",
    "IncomeCat")

fit1 <- glm(pregFeel ~ ., data = modeldf, family = "binomial")

(posneg <- tidy(fit1) %>%
    mutate(estimate = round(exp(estimate), 2)) %>%
    cbind.data.frame(round(exp(confint(fit1)), 1), row.names = NULL) %>%
    dplyr::rename(lowCI = `2.5 %`, upCI = `97.5 %`) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything()) %>%
    mutate(p.value = format.pval(pv = p.value, digits = 3, eps = 0.001)) %>%
    unite("95% CI", c("lowCI", "upCI"), sep = " - ") %>%
        dplyr::rename(beta = "estimate", variable = "term")
)

posneg
write.csv(posneg, "results/regFeel.csv")

# Models using full data --------------------------------------------------


## Models
modeldf <- cbind.data.frame(age, as.numeric(childnum), sex, educ,
    idealCrit, avoidPreg, pregPlan, avoidControl)

names(modeldf) <- c("Age", "childnum", "sex", "Education", "idealCrit",
    "avoidPreg", "pregPlan", "avoidControl")

fit2 <- glm(avoidControl ~ ., data = modeldf, family = "binomial")

(avoid <- tidy(fit2) %>%
    mutate(estimate = round(exp(estimate), 2)) %>%
    cbind.data.frame(round(exp(confint(fit2)), 1), row.names = NULL) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything()) %>%
    mutate(p.value = format.pval(pv = p.value, digits = 2, eps = 0.001)) %>%
    unite("95% CI", c("2.5 %", "97.5 %"), sep = " - ") %>%
        rename(beta = "estimate", variable = "term")
)

modeldf <- cbind.data.frame(as.numeric(childnum), sex, hispanic,
    idealCrit, avoidPreg, pregPlan, becomeControl)

names(modeldf) <- c("childnum", "sex", "hispanic", "idealCrit",
    "avoidPreg", "pregPlan", "becomeControl")

fit3 <- glm(becomeControl ~ ., data = modeldf, family = "binomial")

(become <- tidy(fit3) %>%
    mutate(estimate = round(exp(estimate), 2)) %>%
    cbind.data.frame(round(exp(confint(fit3)), 1), row.names = NULL) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything()) %>%
    mutate(p.value = format.pval(pv = p.value, digits = 3, eps = 0.001)) %>%
    unite("95% CI", c("2.5 %", "97.5 %"), sep = " - ") %>%
        rename(beta = "estimate", variable = "term")
)


avoid
write.csv(avoid, "results/regAvoid.csv")

become
write.csv(become, "results/becomeReg.csv")

