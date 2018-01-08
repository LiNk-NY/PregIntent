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
source("R/table-helpers.R")

attach(pregint)
## Totals by group
table(avoidControl)

tab2 <- .comparisonTable(age, childnum, gender, hispanic, educ, idealCrit,
    avoidPreg, pregPlan, currentSit, race, incCat, relationship,
    outcome = avoidControl,
    headerRow = c("Age in years M (SD)", "No. of Children M (SD)", "Sex",
        "Hispanic", "Educational attainment", "Ideal Criteria",
        "Avoid Pregnancy", "Pregnancy can be planned", "Current situation",
        "Race", "Income category", "Relationship status"))

## AVOID CONTROL // BECOME CONTROL
tab2 <- do.call(rbind, tab2)

rownames(tab2) <- simpleCap(rownames(tab2))

write.csv(tab2, "data/table2.csv")
