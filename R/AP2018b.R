# APHA Abstract
library(broom)
library(dplyr)
library(tidyr)

# read data
pregint <- read.csv("data/pregint.csv")
source("R/table-helpers.R")
source("R/relevel.R")

# Main exclusion of those with "other" status
pregint <- pregint[pregint$relationship != "other", ]
pregint$relationship <- droplevels(pregint$relationship)

## Exclude (OLD)
# exclu3 <- pregint$Q3.25..Q3.26 %in%
#     c("you/partner is pregnant", "you/partner can't get pregnant") |
#         is.na(pregint$pregFeel) | pregint$relationship == "other"

## Exclude
## 1. you/partner is pregnant Q3.25 & 3.26 (should be n=1,755)
exclu3 <- pregint$Q3.25..Q3.26 == "you/partner is pregnant"

preg3 <- pregint[!exclu3, ]

## drop levels after exclusion
preg3$currentSit <- droplevels(preg3$currentSit)

## Able to get pregnant
preg3$ablepreg <- ifelse(preg3$Q1.9a..Q1.9c == "no" |
    preg3$Q1.9b..Q1.9d == "no" | preg3$Q3.25..Q3.26 ==
        "you/partner can't get pregnant", "No", "Yes")

## Load annotations data.frame
source("R/annotations.R")

avoidC <- .comparisonTable(sex, childnum, regionOrg, age, educ, race2,
    hispanic, relationship, underPovLevel, ablepreg, idealCrit,
    Q2.2_1..Q2.7_1, pregPlan, Q2.2_3..Q2.7_3, Q2.2_2..Q2.7_2, Q2.2_5..Q2.7_5,
    currentSit,
    outcome = avoidControl,
    data = preg3,
    headerFrame = annotations)

avoidCont <- do.call(rbind, avoidC)

rownames(avoidCont) <- simpleCap(rownames(avoidCont))


becomeC <- .comparisonTable(sex, childnum, regionOrg, age, educ, race2,
    hispanic, relationship, underPovLevel, ablepreg, idealCrit, Q2.2_1..Q2.7_1,
    pregPlan, Q2.2_3..Q2.7_3, Q2.2_2..Q2.7_2, Q2.2_5..Q2.7_5, currentSit,
    outcome = becomeControl,
    data = preg3,
    headerFrame = annotations)

becomeCont <- do.call(rbind, becomeC)

rownames(becomeCont) <- simpleCap(rownames(becomeCont))

if (!dir.exists("results/AP2018b"))
    dir.create("results/AP2018b", recursive = TRUE)

avoidbecome <- cbind(avoidCont, becomeCont)
write.csv(avoidbecome, file = "results/AP2018b/avoidbecomeControl.csv")

# Multivariable Logistic Regression ---------------------------------------

preg3$age5 <- preg3$age/5


# Control over avoiding pregnancy -----------------------------------------
## How much control would you say you have over avoiding a (partner’s)
## pregnancy?

modelavoid <- preg3[, c("sex", "childnum", "age5", "educ", "race2",
"relationship", "underPovLevel", "ablepreg", "idealCrit", "Q2.2_1..Q2.7_1",
"pregPlan", "Q2.2_3..Q2.7_3", "Q2.2_2..Q2.7_2", "Q2.2_5..Q2.7_5", "currentSit",
"avoidControl")]

fitA <- glm(avoidControl ~ ., data = modelavoid, family = "binomial")

(pavoid <- tidy(fitA) %>%
    mutate(estimate = round(exp(estimate), 2)) %>%
    cbind.data.frame(round(exp(confint(fitA)), 1), row.names = NULL) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything()) %>%
    mutate(p.value = format.pval(pv = p.value, digits = 2, eps = 0.001)) %>%
    unite("95% CI", c("2.5 %", "97.5 %"), sep = " - ") %>%
        rename(OR = "estimate", variable = "term")
)

write.csv(pavoid, "results/AP2018b/regressAvoid.csv")
sink(file = "results/AP2018b/regressAvoid.txt")
pavoid
sink()

# Control over becoming pregnant ------------------------------------------
## If you wanted (a partner) to become pregnant, how much control would you say
## you have over (her) becoming pregnant?

modelbecome <- preg3[, c("sex", "childnum", "educ", "idealCrit",
    "Q2.2_1..Q2.7_1", "pregPlan", "Q2.2_3..Q2.7_3",
    "becomeControl")]

fitB <- glm(becomeControl ~ ., data = modelbecome, family = "binomial")

(pbecome <- tidy(fitB) %>%
    mutate(estimate = round(exp(estimate), 2)) %>%
    cbind.data.frame(round(exp(confint(fitB)), 1), row.names = NULL) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything()) %>%
    mutate(p.value = format.pval(pv = p.value, digits = 2, eps = 0.001)) %>%
    unite("95% CI", c("2.5 %", "97.5 %"), sep = " - ") %>%
        rename(OR = "estimate", variable = "term")
)

write.csv(pbecome, "results/AP2018b/regressBecome.csv")

sink("results/AP2018b/regressBecome.txt")
pbecome
sink()
