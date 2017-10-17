## Descriptives

# Gender
gender <- .recodeFactors(pregint, splitFrames$Q1.2)
table(gender)
all.equal(sum(table(gender)), 2099)

residence <- .recodeFactors(pregint, splitFrames$Q110)
table(residence)
sum(table(residence))

## Check if any values are code 53 (do not reside in US)
any(pregint$Q110 == 53L)

age <- pregint$Q112
summary(age)

sexpref <- .recodeFactors(pregint, splitFrames$Q114)
table(sexpref)
sum(table(sexpref))

## Check raw table for values other than 1 & 3
table(pregint$Q114)


