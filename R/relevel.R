## Relevel variables

# recoding and releveling -------------------------------------------------

idealCrit <- factor(idealCrit)
idealCrit <- relevel(idealCrit, ref = "no")

avoidPreg <- relevel(avoidPreg, ref = "No")
pregPlan <- factor(pregPlan)
pregFeel <- factor(pregFeel)

educLev <- c("LT/some HS", "HS diploma/GED", "Some college",
    "College degree/Some Grad", "Grad degree")
levels(educ[[1L]]) <- educLev
educ <- factor(educ[[1L]], ordered = TRUE)

currentSit <- factor(currentSit)
currentSit <- relevel(currentSit, "don't want you/partner to become pregnant soon")

race <- relevel(race, ref = "White")
relationship <- relevel(relationship, ref = "single")
