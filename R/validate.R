## Code for checking data integrity
codebook <- read.csv("docs/codebookCode.csv")
codebook <- split(codebook, codebook$codebname)
pregint <- read.csv("data/pregint.csv")

all.equal(sum(table(sexpref)), 2099)
all.equal(sum(table(sex)), 2099)

## Check if any values are code 53 (do not reside in US) or 40 (Puerto Rico)
any(pregint$Q110 %in% c(53, 40))

## Current Situation
codebook$Q3.25
table(pregint$Q1.2, pregint$Q3.26)
table(pregint$Q1.2, pregint$Q3.25)
table(pregint$Q1.2, pregint$Q3.25..Q3.26)

## totals
table(c(pregint$Q3.25, pregint$Q3.26))
table(pregint$Q3.25..Q3.26)

## biological ability to have children
codebook$Q1.9a
table(pregint$Q1.2, pregint$Q1.9a)
table(pregint$Q1.2, pregint$Q1.9c)
table(pregint$Q1.2, pregint$Q1.9a..Q1.9c)

codebook$Q1.9b
table(pregint$Q1.2, pregint$Q1.9b)
table(pregint$Q1.2, pregint$Q1.9d)
table(pregint$Q1.2, pregint$Q1.9b..Q1.9d)
