## Results
library(broom)

## Models
modelDF <- cbind.data.frame(age, as.numeric(childnum), gender, hispanic,
    idealCrit, avoidPreg, pregPlan, pregFeel)

names(modelDF) <- c("age", "childnum", "gender", "hispanic", "idealCrit",
    "avoidPreg", "pregPlan", "pregFeel")

pregFeel <- factor(pregFeel)
contrasts(pregFeel)

t1fit <- glm(pregFeel ~ ., data = modelDF, family = "binomial")


restab <- tidy(t1fit) %>% mutate(estimate = exp(estimate)) %>%
    cbind.data.frame(as.data.frame(exp(confint(t1fit))), row.names = NULL) %>%
    select(-std.error, -statistic)
