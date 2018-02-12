# Table 1 Sociodemographics x Feelings about Pregnancy --------------------
# Totals by group
pregint <- read.csv("data/pregint.csv")
subGroup <- !is.na(pregint$pregFeel)
subdata <- pregint[subGroup, ]
rm(pregint)

attach(subdata)
source("R/table-helpers.R")
currentSit <- factor(currentSit) # drop levels

# Table 1 -----------------------------------------------------------------

tab1 <- .comparisonTable(age, childnum, sex, hispanic, educ, idealCrit,
    avoidPreg, pregPlan, currentSit, race, incCat, relationship,
    outcome = pregFeel,
    headerRow = c("Age in years M (SD)", "No. of Children M (SD)", "Sex",
        "Hispanic", "Educational attainment", "Ideal Criteria",
        "Avoid Pregnancy", "Pregnancy can be planned", "Current situation",
        "Race", "Income category", "Relationship status"))

tab1 <- do.call(rbind, tab1)

rownames(tab1) <- simpleCap(rownames(tab1))
detach(subdata)

write.csv(tab1, file = "data/table1.csv")
