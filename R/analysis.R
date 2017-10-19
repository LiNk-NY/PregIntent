## Descriptives
library(dplyr)
library(maps)
data("state.fips")
library(Hmisc)

## Initial checks
# Sexual preference
sexpref <- .recodeFactors(pregint, splitFrames$Q114)
table(sexpref)
all.equal(sum(table(sexpref)), 2099)

# Gender
gender <- .recodeFactors(pregint, splitFrames$Q1.2)
table(gender)
all.equal(sum(table(gender)), 2099)

## State of residence (all should reside in US)
stateOrg <- .recodeFactors(pregint, splitFrames$Q110)
table(stateOrg)
sum(table(stateOrg))

## Check if any values are code 53 (do not reside in US) or 40 (Puerto Rico)
any(pregint$Q110 %in% c(53, 40))

## Coding for Main Outcome
## Logical vectors for each gender
females <- gender == "female"
males <- gender == "male"

## Main outcome
codebook[rownames(codebook) == "Q3.34", ]
codebook[rownames(codebook) == "Q121", ]

pregFeel <- vector("character", 2099)
## "Ultimately, how would you feel about being pregnant right now?"
pregFeel[females] <- .recodeFactors(pregint, splitFrames$Q3.34)[females]
pregFeel[males] <- .recodeFactors(pregint, splitFrames$Q121)[males]
table(pregFeel)

## State of residence
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


