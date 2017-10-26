## Models
modelDF <- cbind.data.frame(age, as.numeric(childnum), gender, hispanic,
    idealCrit, avoidPreg, pregPlan, pregFeel)
names(modelDF) <- c("age", "childnum", "gender", "hispanic", "idealCrit",
    "avoidPreg", "pregPlan", "pregFeel")
glm(pregFeel ~ ., data = modelDF, family = "binomial")
