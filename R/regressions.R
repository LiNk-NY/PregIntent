## Load pkg
library(broom)

## Models
modeldf <- cbind.data.frame(age, as.numeric(childnum), gender, hispanic,
    idealCrit, avoidPreg, pregPlan, pregFeel)

names(modeldf) <- c("age", "childnum", "gender", "hispanic", "idealCrit",
    "avoidPreg", "pregPlan", "pregFeel")

fit1 <- glm(pregFeel ~ ., data = modeldf, family = "binomial")

posneg <- tidy(fit1) %>%
    mutate(estimate = exp(estimate)) %>%
    cbind.data.frame(exp(confint(fit1)), row.names = NULL) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything())

## Models
modeldf <- cbind.data.frame(age, as.numeric(childnum), gender, hispanic,
    idealCrit, avoidPreg, pregPlan, avoidControl)

names(modeldf) <- c("age", "childnum", "gender", "hispanic", "idealCrit",
    "avoidPreg", "pregPlan", "avoidControl")

fit2 <- glm(avoidControl ~ ., data = modeldf, family = "binomial")

avoid <- tidy(fit2) %>%
    mutate(estimate = exp(estimate)) %>%
    cbind.data.frame(exp(confint(fit2)), row.names = NULL) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything())

modeldf <- cbind.data.frame(age, as.numeric(childnum), gender, hispanic,
    idealCrit, avoidPreg, pregPlan, becomeControl)

names(modeldf) <- c("age", "childnum", "gender", "hispanic", "idealCrit",
    "avoidPreg", "pregPlan", "becomeControl")

fit3 <- glm(becomeControl ~ ., data = modeldf, family = "binomial")

become <- tidy(fit3) %>%
    mutate(estimate = exp(estimate)) %>%
    cbind.data.frame(exp(confint(fit3)), row.names = NULL) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything())

posneg
write.csv(posneg, "data/regFeel.csv")

avoid
write.csv(avoid, "data/regAvoid.csv")

become
write.csv(become, "data/becomeReg.csv")

