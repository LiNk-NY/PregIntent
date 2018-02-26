## Poster analysis

## Load pkg
library(broom)
library(dplyr)
library(tidyr)

# Table 1 Sociodemographics x Feelings about Pregnancy --------------------
source("R/table-helpers.R")

## load data
pregint <- read.csv("data/pregint.csv")
source("R/relevel.R")

## include proper exclusion
exclusionCriteria <- pregint$Q3.25..Q3.26 %in%
    c("you/partner is pregnant", "you/partner can't get pregnant") |
        pregint$Q1.9a..Q1.9c == "no" | pregint$Q1.9b..Q1.9d == "no" |
        is.na(pregint$pregFeel)

subdata <- pregint[!exclusionCriteria, ]

# Table 1 -----------------------------------------------------------------

tab1 <- .comparisonTable(age, childnum, sex, hispanic, educ, idealCrit,
    avoidPreg, pregPlan, currentSit, race2, incCat, relationship,
    outcome = pregFeel,
    headerRow = c("Age in years M (SD)", "No. of Children M (SD)", "Sex",
        "Hispanic", "Educational attainment", "Ideal Criteria",
        "Avoid Pregnancy", "Pregnancy can be planned", "Current situation",
        "Race", "Income category", "Relationship status"),
    data = subdata)

tab1 <- do.call(rbind, tab1)

rownames(tab1) <- simpleCap(rownames(tab1))

write.csv(tab1, file = "results/table1.csv")


# Table 2 -----------------------------------------------------------------

tab2 <- .comparisonTable(age, childnum, sex, hispanic, educ, idealCrit,
    avoidPreg, pregPlan, currentSit, race2, incCat, relationship,
    outcome = avoidControl,
    headerRow = c("Age in years M (SD)", "No. of Children M (SD)", "Sex",
        "Hispanic", "Educational attainment", "Ideal Criteria",
        "Avoid Pregnancy", "Pregnancy can be planned", "Current situation",
        "Race", "Income category", "Relationship status"),
    data = subdata)

## AVOID CONTROL // BECOME CONTROL
tab2 <- do.call(rbind, tab2)

rownames(tab2) <- simpleCap(rownames(tab2))

write.csv(tab2, "results/table2.csv")

# Regression --------------------------------------------------------------

modeldf1 <- subdata[, c("age", "childnum", "sex", "hispanic", "idealCrit",
    "avoidPreg", "pregPlan", "pregFeel", "currentSit", "race", "relationship",
    "incCat")]

fit1 <- glm(pregFeel ~ ., data = modeldf1, family = "binomial")

(posneg <- tidy(fit1) %>%
    mutate(estimate = round(exp(estimate), 2)) %>%
    cbind.data.frame(round(exp(confint(fit1)), 1), row.names = NULL) %>%

    dplyr::rename(lowCI = `2.5 %`, upCI = `97.5 %`) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything()) %>%
    mutate(p.value = format.pval(pv = p.value, digits = 3, eps = 0.001)) %>%
    unite("95% CI", c("lowCI", "upCI"), sep = " - ") %>%
        dplyr::rename(OR = "estimate", variable = "term")
)

posneg
write.csv(posneg, "results/regFeel.csv")

# Models using full data --------------------------------------------------

## Models
modeldf2 <- subdata[, c("age", "childnum", "sex", "educ", "idealCrit",
    "avoidPreg", "pregPlan", "avoidControl")]

fit2 <- glm(avoidControl ~ ., data = modeldf2, family = "binomial")

(avoid <- tidy(fit2) %>%
    mutate(estimate = round(exp(estimate), 2)) %>%
    cbind.data.frame(round(exp(confint(fit2)), 1), row.names = NULL) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything()) %>%
    mutate(p.value = format.pval(pv = p.value, digits = 2, eps = 0.001)) %>%
    unite("95% CI", c("2.5 %", "97.5 %"), sep = " - ") %>%
        rename(OR = "estimate", variable = "term")
)

modeldf3 <- subdata[, c("childnum", "sex", "hispanic", "idealCrit",
    "avoidPreg", "pregPlan", "becomeControl")]

fit3 <- glm(becomeControl ~ ., data = modeldf3, family = "binomial")

(become <- tidy(fit3) %>%
    mutate(estimate = round(exp(estimate), 2)) %>%
    cbind.data.frame(round(exp(confint(fit3)), 1), row.names = NULL) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything()) %>%
    mutate(p.value = format.pval(pv = p.value, digits = 3, eps = 0.001)) %>%
    unite("95% CI", c("2.5 %", "97.5 %"), sep = " - ") %>%
        rename(OR = "estimate", variable = "term")
)

avoid
write.csv(avoid, "results/regAvoid.csv")

become
write.csv(become, "results/becomeReg.csv")
