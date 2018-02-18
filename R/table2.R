# Table 2 - Control over Pregnancy ----------------------------------------
source("R/table-helpers.R")

## load data
pregint <- read.csv("data/pregint.csv")
source("R/relevel.R")

tab2 <- .comparisonTable(age, childnum, sex, hispanic, educ, idealCrit,
    avoidPreg, pregPlan, currentSit, race, incCat, relationship,
    outcome = avoidControl,
    headerRow = c("Age in years M (SD)", "No. of Children M (SD)", "Sex",
        "Hispanic", "Educational attainment", "Ideal Criteria",
        "Avoid Pregnancy", "Pregnancy can be planned", "Current situation",
        "Race", "Income category", "Relationship status"),
    data = pregint)

## AVOID CONTROL // BECOME CONTROL
tab2 <- do.call(rbind, tab2)

rownames(tab2) <- simpleCap(rownames(tab2))

write.csv(tab2, "results/table2.csv")
