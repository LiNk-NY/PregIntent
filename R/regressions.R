## Load pkg
library(broom)

## Models
modeldf <- cbind.data.frame(age, as.numeric(childnum), gender, hispanic,
    idealCrit, avoidPreg, pregPlan, pregFeel)

names(modeldf) <- c("Age", "childnum", "Gender", "Hispanic", "idealCrit",
    "avoidPreg", "pregPlan", "pregFeel")

fit1 <- glm(pregFeel ~ ., data = modeldf, family = "binomial")

(posneg <- tidy(fit1) %>%
    mutate(estimate = round(exp(estimate), 2)) %>%
    cbind.data.frame(round(exp(confint(fit1)), 1), row.names = NULL) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything()) %>%
    mutate(p.value = format.pval(pv = p.value, digits = 3, eps = 0.001)) %>%
    unite("95% CI", c("2.5 %", "97.5 %"), sep = " - ") %>%
        rename(beta = "estimate", variable = "term")
)

## Models
modeldf <- cbind.data.frame(age, as.numeric(childnum), gender, educ,
    idealCrit, avoidPreg, pregPlan, avoidControl)

names(modeldf) <- c("Age", "childnum", "Gender", "Education", "idealCrit",
    "avoidPreg", "pregPlan", "avoidControl")

fit2 <- glm(avoidControl ~ ., data = modeldf, family = "binomial")

(avoid <- tidy(fit2) %>%
    mutate(estimate = round(exp(estimate), 2)) %>%
    cbind.data.frame(round(exp(confint(fit2)), 1), row.names = NULL) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything()) %>%
    mutate(p.value = format.pval(pv = p.value, digits = 3, eps = 0.001)) %>%
    unite("95% CI", c("2.5 %", "97.5 %"), sep = " - ") %>%
        rename(beta = "estimate", variable = "term")
)

modeldf <- cbind.data.frame(as.numeric(childnum), gender, hispanic,
    idealCrit, avoidPreg, pregPlan, becomeControl)

names(modeldf) <- c("childnum", "gender", "hispanic", "idealCrit",
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

posneg
write.csv(posneg, "data/regFeel.csv")

avoid
write.csv(avoid, "data/regAvoid.csv")

become
write.csv(become, "data/becomeReg.csv")

