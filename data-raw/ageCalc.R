# Script to recreate Age vector from counts
library(readxl)

page <- read_excel("../data/PCP-age.xlsx")
page <- page[seq_len(nrow(page)-1), ]
page$COUNT <- as.integer(page[["COUNT"]])
page$Age <- as.integer(page[["Age"]])

AgeV <- apply(page, 1L, function(row) {
    rep(row[[1]], times = row[[2]])
})

AgeV <- unlist(AgeV)

mean(AgeV)
sd(AgeV)

age <- data.frame(Age = AgeV)
write.csv(age, file = "../data/ageVar.csv", row.names = FALSE)

