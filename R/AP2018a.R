# APHA Abstract
# read data
pregint <- read.csv("data/pregint.csv")
source("R/table-helpers.R")

exclusionCriteria <- pregint$Q3.25..Q3.26 %in%
    c("you/partner is pregnant", "you/partner can't get pregnant") |
        pregint$Q1.9a..Q1.9c == "no" | pregint$Q1.9b..Q1.9d == "no" |
        is.na(pregint$pregFeel)

# Emotions Related to Pregnancy -------------------------------------------

# pregnant partner
table(pregint$Q3.25..Q3.26)

preg1 <- pregint[!exclusionCriteria, ]

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
do.call(rbind, rest)


# Modeling Feelings about Pregnancy ---------------------------------------

## Exclusions
## 1. you/partner is pregnant / you/partner can't get pregnant
## 2. don't (ever) want me / partner pregnant
exclu2 <- pregint$Q3.25..Q3.26 %in%
    c("you/partner is pregnant", "you/partner can't get pregnant") |
    pregint$idealCrit == "don't want me/partner pregnant"

preg2 <- pregint[!exclu2, ]
preg2$idealCrit <- droplevels(preg2$idealCrit)

## Able to get pregnant
preg2$ablepreg <- ifelse(preg2$Q1.9a..Q1.9c == "no" |
    preg2$Q1.9b..Q1.9d == "no" | preg2$Q3.25..Q3.26 ==
        "you/partner can't get pregnant", "No", "Yes")

avoidPregs <- sort(grep("Q2\\.2.*\\.\\.*", names(preg2), value = TRUE))

preg2[, avoidPregs] <- lapply(avoidPregs, function(varname) {
    actvar <- preg2[, varname]
    levels(actvar) <- c(levels(actvar), "Not selected")
    actvar[is.na(actvar)] <- "Not selected"
    actvar
})

.comparisonTable(sex, childnum, regionOrg, age, educ, race, hispanic,
    relationship, underPovLevel, ablepreg, idealCrit, Q2.2_1..Q2.7_1,
    pregPlan, Q2.2_3..Q2.7_3, Q2.2_2..Q2.7_2, Q2.2_5..Q2.7_5, Q2.12_1..Q2.13_1,
    Q3.12_1..Q3.13_1,
    outcome = pregFeel,
    data = preg2)