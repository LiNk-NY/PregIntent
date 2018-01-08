# Table 1 Sociodemographics x Feelings about Pregnancy --------------------
# Totals by group
rm(list = ls())
pregint <- read.csv("data/pregint.csv")
subGroup <- !is.na(pregint$pregFeel)
subdata <- pregint[subGroup, ]
rm(pregint)

attach(subdata)
source("R/table-helpers.R")
currentSit <- factor(currentSit) # drop levels

# Table 1 -----------------------------------------------------------------

tab1 <- .comparisonTable(age, childnum, gender, hispanic, educ, idealCrit,
    avoidPreg, pregPlan, currentSit, race, incCat, relationship,
    outcome = pregFeel,
    headerRow = c("Age in years M (SD)", "No. of Children M (SD)", "Sex",
        "Hispanic", "Educational attainment", "Ideal Criteria",
        "Avoid Pregnancy", "Pregnancy can be planned", "Current situation",
        "Race", "Income category", "Relationship status"))

tab1 <- do.call(rbind, tab1)
## Fix caps in categories
simpleCap <- function(x) {
    unname(vapply(x, function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep="", collapse=" ")
    }, character(1L)))
}

rownames(tab1) <- simpleCap(rownames(tab1))
detach(subdata)

write.csv(tab1, file = "data/table1.csv")

# Table 2 - Control over Pregnancy ----------------------------------------

rm(list = ls())
pregint <- read.csv("data/pregint.csv")
attach(pregint)
## Totals by group
table(avoidControl)

nums0 <- cbind(
    rbind(.groupMeans(age, avoidControl), .groupMeans(childnum, avoidControl)),
    rbind(.ttestPval(age, avoidControl), .ttestPval(childnum, avoidControl))
    )

categorical0 <- rbind(.crossTab(gender, avoidControl), .crossTab(regionOrg, avoidControl),
        .crossTab(hispanic, avoidControl), .crossTab(educ, avoidControl),
        .crossTab(idealCrit, avoidControl), .crossTab(avoidPreg, avoidControl),
        .crossTab(pregPlan, avoidControl), .crossTab(currentSit, avoidControl))

chitestres0 <- rbind(.chitestPval(gender, avoidControl),
    .chitestPval(regionOrg, avoidControl), .chitestPval(hispanic, avoidControl),
    .chitestPval(educ, avoidControl), .chitestPval(idealCrit, avoidControl),
    .chitestPval(avoidPreg, avoidControl), .chitestPval(pregPlan, avoidControl),
    .chitestPval(currentSit, avoidControl))

cats0 <- cbind(categorical0, chitestres0)

leftside <- rbind(nums0, cats0)

## Total by group
table(becomeControl)

nums1 <- cbind(
    rbind(.groupMeans(age, becomeControl), .groupMeans(childnum, becomeControl)),
    rbind(.ttestPval(age, becomeControl), .ttestPval(childnum, becomeControl))
    )

categorical1 <- rbind(.crossTab(gender, becomeControl), .crossTab(regionOrg, becomeControl),
        .crossTab(hispanic, becomeControl), .crossTab(educ, becomeControl),
        .crossTab(idealCrit, becomeControl), .crossTab(avoidPreg, becomeControl),
        .crossTab(pregPlan, becomeControl), .crossTab(currentSit, becomeControl))

chitestres1 <- rbind(.chitestPval(gender, becomeControl),
    .chitestPval(regionOrg, becomeControl), .chitestPval(hispanic, becomeControl),
    .chitestPval(educ, becomeControl), .chitestPval(idealCrit, becomeControl),
    .chitestPval(avoidPreg, becomeControl), .chitestPval(pregPlan, becomeControl),
    .chitestPval(currentSit, becomeControl))

cats1 <- cbind(categorical1, chitestres1)

rightside <- rbind(nums1, cats1)

## AVOID CONTROL // BECOME CONTROL
(tab2 <- cbind(leftside, rightside))

rownames(tab2) <- simpleCap(rownames(tab2))

write.csv(tab2, "data/table2.csv")
