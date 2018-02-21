# recoding and releveling -------------------------------------------------

## Recode NA to not selected
avoidPregs <- sort(grep("Q2\\.2.*\\.\\.*", names(pregint), value = TRUE))

pregint[, avoidPregs] <- lapply(avoidPregs, function(varname) {
    actvar <- pregint[, varname, drop = TRUE]
    levels(actvar) <- c(levels(actvar), "Not selected")
    actvar[is.na(actvar)] <- "Not selected"
    actvar <- relevel(actvar, ref = "Not selected")
    actvar
})

## Relevel variables
pregint$idealCrit <- relevel(pregint$idealCrit, ref = "no")
pregint$avoidPreg <- relevel(pregint$avoidPreg, ref = "No")
pregint$becomeControl <- relevel(pregint$becomeControl, ref = "Low control")
pregint$avoidControl <- relevel(pregint$avoidControl, ref = "Low control")

pregint$currentSit <- relevel(pregint$currentSit,
    "don't want you/partner to become pregnant soon")

pregint$race <- relevel(pregint$race, ref = "White")
pregint$relationship <- relevel(pregint$relationship, ref = "single")
pregint$incCat <- relevel(pregint$incCat, ref = "less than $20,000")
pregint$Q2.12_1..Q2.13_1 <- relevel(pregint$Q2.12_1..Q2.13_1, ref = "no control")
pregint$Q3.12_1..Q3.13_1 <- relevel(pregint$Q3.12_1..Q3.13_1, ref = "no control")
pregint$educ <- relevel(pregint$educ, ref = "HS diploma/GED")
pregint$sex <- relevel(pregint$sex, ref = "male")
pregint$currentSit <- relevel(pregint$currentSit,
    ref = "don't want you/partner to become pregnant soon")
