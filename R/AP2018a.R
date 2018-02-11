# APHA Abstract
# read data
pregint <- read.csv("data/pregint.csv")

# pregnant partner
table(pregint$Q3.25..Q3.26)

exclusionCriteria <- pregint$Q3.25..Q3.26 %in%
    c("you/partner is pregnant", "you/partner can't get pregnant") |
        pregint$Q1.9a..Q1.9c == "no" | pregint$Q1.9b..Q1.9d == "no"

pregint <- pregint[!exclusionCriteria, ]
