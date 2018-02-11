# APHA Abstract
# read data
pregint <- read.csv("data/pregint.csv")
source("R/table-helpers.R")

# pregnant partner
table(pregint$Q3.25..Q3.26)

exclusionCriteria <- pregint$Q3.25..Q3.26 %in%
    c("you/partner is pregnant", "you/partner can't get pregnant") |
        pregint$Q1.9a..Q1.9c == "no" | pregint$Q1.9b..Q1.9d == "no"

pregint <- pregint[!exclusionCriteria, ]

emos <- c("Q3.32_1..Q3.33_1", "Q3.32_2..Q3.33_2", "Q3.32_3..Q3.33_3",
    "Q3.32_4..Q3.33_4", "Q3.32_5..Q3.33_5", "Q3.32_11..Q3.33_11",
    "Q3.32_6..Q3.33_6", "Q3.32_7..Q3.33_7", "Q3.32_8..Q3.33_8",
    "Q3.32_9..Q3.33_9", "Q3.32_17..Q3.33_17", "Q3.32_10..Q3.33_10")

emoNames <- vapply(emos, function(x)
    as.character(na.omit(unique(pregint[, x]))), character(1L))

names(emos) <- emoNames

rest <- lapply(emos, function(varname) {
    actvar <- pregint[, varname]
    levels(actvar) <- c(levels(actvar), "Not selected")
    actvar[is.na(actvar)] <- "Not selected"

    cbind(.prop(actvar), .crossTab(actvar, pregint$pregFeel),
        .chitestPval(actvar, pregint$pregFeel))
})
do.call(rbind, rest)
