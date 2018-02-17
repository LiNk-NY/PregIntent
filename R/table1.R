# Table 1 Sociodemographics x Feelings about Pregnancy --------------------
source("R/table-helpers.R")

# Totals by group
pregint <- read.csv("data/pregint.csv")

## include proper exclusion
exclusionCriteria <- pregint$Q3.25..Q3.26 %in%
    c("you/partner is pregnant", "you/partner can't get pregnant") |
        pregint$Q1.9a..Q1.9c == "no" | pregint$Q1.9b..Q1.9d == "no" |
        is.na(pregint$pregFeel)

pregsub <- pregint[!exclusionCriteria, ]

pregsub$currentSit <- factor(pregsub$currentSit) # drop levels

# Table 1 -----------------------------------------------------------------

tab1 <- .comparisonTable(age, childnum, sex, hispanic, educ, idealCrit,
    avoidPreg, pregPlan, currentSit, race, incCat, relationship,
    outcome = pregFeel,
    headerRow = c("Age in years M (SD)", "No. of Children M (SD)", "Sex",
        "Hispanic", "Educational attainment", "Ideal Criteria",
        "Avoid Pregnancy", "Pregnancy can be planned", "Current situation",
        "Race", "Income category", "Relationship status"),
    data = pregsub)

tab1 <- do.call(rbind, tab1)

rownames(tab1) <- simpleCap(rownames(tab1))
detach(subdata)

write.csv(tab1, file = "data/table1.csv")
