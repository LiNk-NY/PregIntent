# Table 2 - Control over Pregnancy ----------------------------------------

pregint <- read.csv("data/pregint.csv")
source("R/table-helpers.R")

attach(pregint)
## Totals by group
table(avoidControl)

tab2 <- .comparisonTable(age, childnum, sex, hispanic, educ, idealCrit,
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
