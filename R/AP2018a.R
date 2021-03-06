# APHA Abstract
library(broom)
library(dplyr)
library(tidyr)

# read data
pregint <- read.csv("data/pregint.csv")
source("R/table-helpers.R")
source("R/relevel.R")

# Main Outcome PregFeel ---------------------------------------------------

intentbyfeel <- table(pregint$Q3.25..Q3.26, pregint$pregFeel, useNA = "always")
colintent <- cbind(intentbyfeel, total = margin.table(intentbyfeel, 1L))
(intentbyfeeltotal <- rbind(colintent, total = margin.table(colintent, 2L)))

## Exclusions
## 1. you/partner is pregnant / you/partner can't get pregnant
## 2. "no" to physically possible to have a baby
## 3. any missing values in pregFeel
exclu1 <- pregint$Q3.25..Q3.26 %in%
    c("you/partner is pregnant", "you/partner can't get pregnant") |
        pregint$Q1.9a..Q1.9c == "no" | pregint$Q1.9b..Q1.9d == "no" |
        is.na(pregint$pregFeel)
exclu1[is.na(exclu1)] <- TRUE

# Emotions Related to Pregnancy -------------------------------------------

# pregnant partner
table(pregint$Q3.25..Q3.26)

preg1 <- pregint[!exclu1, ]

emos <- c("Q3.32_1..Q3.33_1", "Q3.32_2..Q3.33_2", "Q3.32_3..Q3.33_3",
    "Q3.32_4..Q3.33_4", "Q3.32_5..Q3.33_5", "Q3.32_11..Q3.33_11",
    "Q3.32_6..Q3.33_6", "Q3.32_7..Q3.33_7", "Q3.32_8..Q3.33_8",
    "Q3.32_9..Q3.33_9", "Q3.32_17..Q3.33_17", "Q3.32_10..Q3.33_10")

emoNames <- vapply(emos, function(x)
    as.character(na.omit(unique(preg1[, x]))), character(1L))

names(emos) <- emoNames

rest <- lapply(emos, function(varname) {
    actvar <- preg1[, varname]
    levels(actvar) <- c(levels(actvar), "Not selected")
    actvar[is.na(actvar)] <- "Not selected"

    cbind(.prop(actvar), .crossTab(actvar, preg1$pregFeel),
        .chitestPval(actvar, preg1$pregFeel))
})

outlevels <- rownames(contrasts(preg1$pregFeel))
headerRow <-
        list(matrix(
            c(paste0("N = ", dim(preg1)[[1]]),
            paste0("n = ", table(preg1$pregFeel)[outlevels]), ""), nrow = 1L,
            dimnames = list("Condition", c("n (%)",
                paste("pregFeel", outlevels, sep = "-"), "p.value"))
        ))
rest <- c(headerRow, rest)

feeltab <- do.call(rbind, rest)

if (!dir.exists("results/AP2018a"))
    dir.create("results/AP2018a", recursive = TRUE)

write.csv(feeltab, "results/AP2018a/feeltab.csv")

# Modeling Feelings about Pregnancy ---------------------------------------

## Exclusions
## 1. those with an "other" relationship status
preg2 <- pregint[pregint$relationship != "other", ]
preg2$relationship <- droplevels(preg2$relationship)

## 2. you/partner is pregnant
## 3. any missing values in pregFeel
exclu2 <- preg2$Q3.25..Q3.26 == "you/partner is pregnant" |
    is.na(preg2$pregFeel)

preg2 <- preg2[!exclu2, ]
preg2$idealCrit <- droplevels(preg2$idealCrit)
preg2$currentSit <- droplevels(preg2$currentSit)
preg2$Q3.25..Q3.26 <- droplevels(preg2$Q3.25..Q3.26)

## Able to get pregnant
preg2$ablepreg <- ifelse(preg2$Q1.9a..Q1.9c == "no" |
    preg2$Q1.9b..Q1.9d == "no" | preg2$Q3.25..Q3.26 ==
        "you/partner can't get pregnant", "No", "Yes")

## Load annotations data.frame
source("R/annotations.R")

tres <- .comparisonTable(sex, childnum, regionOrg, age, educ, race2, hispanic,
    relationship, underPovLevel, ablepreg, idealCrit, Q2.2_1..Q2.7_1,
    pregPlan, Q2.2_3..Q2.7_3, Q2.2_2..Q2.7_2, Q2.2_5..Q2.7_5, Q2.12_1..Q2.13_1,
    Q3.12_1..Q3.13_1, currentSit,
    outcome = pregFeel,
    data = preg2, headerFrame = annotations)

tablefeels <- do.call(rbind, tres)

rownames(tablefeels) <- simpleCap(rownames(tablefeels))

write.csv(tablefeels, file = "results/AP2018a/emotionspreg.csv")

# Multivariable Logistic Regression ---------------------------------------

modelfr <- preg2[, c("sex", "childnum", "regionOrg", "age", "race2", "hispanic",
    "relationship", "underPovLevel", "ablepreg", "idealCrit", "Q2.2_1..Q2.7_1",
    "pregPlan", "Q2.2_3..Q2.7_3", "Q2.2_5..Q2.7_5", "Q2.12_1..Q2.13_1",
    "Q3.12_1..Q3.13_1",
    "currentSit",
    "pregFeel")]

fit0 <- glm(pregFeel ~ ., data = modelfr, family = "binomial")

(pfeel <- tidy(fit0) %>%
    mutate(estimate = round(exp(estimate), 2)) %>%
    cbind.data.frame(round(exp(confint(fit0)), 3), row.names = NULL) %>%
    select(-statistic, -std.error) %>% select(-p.value, everything()) %>%
    mutate(p.value = format.pval(pv = p.value, digits = 2, eps = 0.001)) %>%
    unite("95% CI", c("2.5 %", "97.5 %"), sep = " - ") %>%
        rename(OR = "estimate", variable = "term")
)

write.csv(pfeel, "results/AP2018a/mvpfeel.csv")
