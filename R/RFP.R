## RFP Checks
pregint <- read.csv("data/pregint.csv")

youngsub <- pregint[pregint$age %in% 21:24, ]

table(youngsub$sex, useNA = "always")

table(youngsub$regionOrg, useNA = "always")

table(youngsub$race, useNA = "always")
table(youngsub$hispanic, useNA = "always")
table(youngsub$hispOrg, useNA = "always")
