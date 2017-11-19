## Descriptives
library(dplyr)
library(tidyr)
library(maps)
data("state.fips")
library(Hmisc)

## Initial checks
# Sexual preference
sexpref <- recodeFactors(pregint, splitFrames$Q114)
all.equal(sum(table(sexpref)), 2099)

# Gender
gender <- recodeFactors(pregint, splitFrames$Q1.2)
all.equal(sum(table(gender)), 2099)

## State of residence (all should reside in US)
stateOrg <- recodeFactors(pregint, splitFrames$Q110)
sum(table(stateOrg))

## Check if any values are code 53 (do not reside in US) or 40 (Puerto Rico)
any(pregint$Q110 %in% c(53, 40))

## Coding for Main Outcome
## Logical vectors for each gender
females <- gender == "female"
males <- gender == "male"

## Main outcome
codebooktext[rownames(codebooktext) == "Q3.34", ]
codebooktext[rownames(codebooktext) == "Q121", ]

pregFeel <- vector("character", 2099)
## "Ultimately, how would you feel about being pregnant right now?"
pregFeel[females] <- recodeFactors(pregint, splitFrames$Q3.34)[females]
pregFeel[males] <- recodeFactors(pregint, splitFrames$Q121)[males]
table(pregFeel)
data.frame(AnsPreg = sum(table(pregFeel)), N = 2099,
    Perc = round((sum(table(pregFeel))/2099)*100, 2))

## Ideal criteria
idealCrit <- vector("character", 2099)
idealCrit[females] <- recodeFactors(pregint, splitFrames$Q3.3)[females]
idealCrit[males] <- recodeFactors(pregint, splitFrames$Q3.2)[males]
table(idealCrit)

## Number of children
childnum <- gsub("NO", "0", pregint$Q1.9, ignore.case = TRUE)
childnum[childnum %in% c("mm", "na")] <- NA
childnum <- as.integer(childnum)

## Reference dataset for regions
regionMap <- state.fips[!duplicated(state.fips$fips), c("fips", "region", "polyname")]
names(regionMap) <- c("fips", "region", "state")
regionMap$state <- gsub("\\:.*$", "", regionMap$state)
regionMap$region <- factor(regionMap$region, levels = 1:4,
    labels = c("North East", "MidWest", "South", "West"))
regionMap <- rbind(regionMap, data.frame(fips = c(2, 15), region = "West",
    state = c("alaska", "hawaii")))

## Region of residence
regionOrg <-
    regionMap$region[match(tolower(as.character(stateOrg[[1L]])), regionMap$state)]
## check proper merge
head(cbind(stateOrg, regionOrg))
## KEEP regionorg

## Race (select all that apply)
raceDF <- recodeFactors(pregint, splitFrames$Q1.8)

race <- vector("character", 2099)
singleRes <- rowSums(!is.na(raceDF)) == 1

race[singleRes] <- apply(raceDF[singleRes, ], 1L, na.omit)
## More than 1 gets coded as OTHER
race[!singleRes] <- "other"

## Hispanic
hispanic <- recodeFactors(pregint, splitFrames$Q1.6)
table(hispanic)
## Hispanic origin
hispOrg <- recodeFactors(pregint, splitFrames$Q1.7)

## Check to see if all who said "Yes" Hispanic answered origin question
## equal number of responses in both variables after subsetting by "yes"
## responses
sum(!apply(hispOrg[hispanic[[1L]] == "yes",], 1L,
    function(row) all(is.na(row))))

## Age
age <- pregint$Q112
summary(age)

## ageGroup
ageGroup <- Hmisc::cut2(age, cuts = c(25, 30, 35, 40))
levels(ageGroup) <- c("21-24", "25-29", "30-34", "35-39", "40-44")

## Modify labels to recode using function
educ <- recodeFactors(pregint, splitFrames$Q1.10)

## Relationshp recode
relationship <- recodeFactors(pregint, splitFrames$Q1.11)


## Derived variable for income – based on Poverty Threshold for 2016
## https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-thresholds.html
## Q1.12, Q1,13, Q1.14; above or below pov threshold
povThresh <- readxl::read_excel("data/thresh16.xls", sheet = 2)

povFrame <- do.call(rbind, lapply(as.character(0:8), function(x)
    data.frame(
        HHunder65 = c("yes", "yes", "no", "yes", "no", rep(NA, 7)),
        famUnit = c(1, 1, 1, 2, 2, 3:9),
        childUnder18 = as.numeric(x),
        povThresh = unlist(povThresh[, x])
    ))
)
povFrame <- povFrame[!is.na(povFrame$povThresh), ]
simpPov <- povFrame %>% group_by(famUnit, childUnder18) %>%
    dplyr::summarize(mPovThresh = mean(povThresh, na.rm = TRUE))
simpPov <- as.data.frame(simpPov)

simpPov$incGroup <- Hmisc::cut2(simpPov$mPovThresh,
    cuts = c(20000, 40000, 60000, 80000, 100000))
simpPov$incGroup <- factor(simpPov$incGroup, ordered = TRUE,
    levels = c(levels(simpPov$incGroup), "100000 or more"))
levels(simpPov$incGroup) <- splitFrames$Q1.14$response

simppov <- simpPov %>% unite(famch, famUnit, childUnder18)


## Get factor and order it
incCat <- recodeFactors(pregint, splitFrames$Q1.14)[[1L]]
levels(incCat) <- splitFrames$Q1.14$response
incCat <- factor(incCat, ordered = TRUE)
povData <- cbind.data.frame(famUn = pregint$Q1.12,
    childUn18 = pregint$Q1.13, incCat)
povData <- povData %>% unite(famch, famUn, childUn18)

povertyComp <- cbind.data.frame(povFromTable =
    simppov[match(povData$famch, simppov$famch), "incGroup"],
    povFromData = povData$incCat)
underPovLevel <-
    factor(povertyComp[["povFromData"]] <= povertyComp[["povFromTable"]],
        levels = c(TRUE, FALSE), labels = c("Yes", "No"))

# Pregnancy can be avoided
avoidPreg <- vector("character", 2099)

mavoid <- vector("character", 2099)[males]

newFrame <- recodeFactors(pregint, splitFrames$Q2.2)[males, ]
skip <- rowSums(!is.na(newFrame)) > 1
nonSkips <- sapply(newFrame[!skip, ], as.character)[!is.na(newFrame[!skip, ])]
finalSkips <- recodeFactors(pregint, splitFrames$Q2.5)[males, ][skip]
mavoid[skip] <- as.character(finalSkips)
mavoid[!skip] <- nonSkips

favoid <- vector("character", 2099)[females]

newFrame <- recodeFactors(pregint, splitFrames$Q2.7)[females, ]
skip <- rowSums(!is.na(newFrame)) > 1L
nonSkips <- sapply(newFrame[!skip, ], as.character)[!is.na(newFrame[!skip, ])]
finalSkips <- recodeFactors(pregint, splitFrames$Q2.10)[females, ][skip]
favoid[skip] <- as.character(finalSkips)
favoid[!skip] <- nonSkips

avoidPreg[males] <- mavoid
avoidPreg[females] <- favoid

avoidPreg <- factor(avoidPreg == "can be avoided",
    levels = c(TRUE, FALSE), labels = c("Yes", "No"))

pregPlan <- vector("character", 2099)
newFrame <- recodeFactors(pregint, splitFrames$Q3.5)
newFrame[newFrame == 2L] <- NA
nonSkips <- rowSums(!is.na(newFrame))  == 1L
finalSkips <- recodeFactors(pregint, splitFrames$Q122)[!nonSkips, ]
pregPlan[!nonSkips] <- as.character(finalSkips)
pregPlan[nonSkips] <- sapply(newFrame[nonSkips, ],
    as.character)[!is.na(newFrame[nonSkips, ])]
pregPlan[pregPlan %in% c("can be planned in advance",
    "can be planned in discussion with your partner",
    "can be planned to happen after one's ideal criteria are fulfilled")] <- "Yes"
pregPlan[pregPlan %in% c("'just happens'",
  "can be left to 'fate' or a higher power like God",
  "is a natural process that happens when it’s meant to be")] <- "No"
pregPlan[pregPlan %in% "other"] <- NA_character_

becomeControl <- vector("character", 2099)
becomeControl[males] <- as.character(recodeFactors(pregint,
    splitFrames$Q3.12)[males, ])
becomeControl[females] <- as.character(recodeFactors(pregint,
    splitFrames$Q3.13)[females, ])

becomeControl %<>%
    fct_collapse(`Low control` = c("no control", "a little control"),
        `High control` = c("complete control", "a lot of control"))

## Select all that apply question but not indicated in actual question
## recodeFactors(pregint, splitFrames$Q3.17a)

avoidControl <- vector("character", 2099)
avoidControl[males] <- as.character(recodeFactors(pregint,
    splitFrames$Q2.12)[males, ])
avoidControl[females] <- as.character(recodeFactors(pregint,
    splitFrames$Q2.13)[females, ])

avoidControl %<>%
    fct_collapse(`Low control` = c("no control", "a little control"),
        `High control` = c("complete control", "a lot of control"))

currentSit <- vector("character", 2099)
currentSit[males] <- as.character(recodeFactors(pregint, splitFrames$Q3.25)[males, ])
currentSit[females] <- as.character(recodeFactors(pregint, splitFrames$Q3.26)[females, ])

feelsDF <- as.data.frame(matrix(NA, nrow = 2099, ncol = 12,
    dimnames = list(NULL, splitFrames$Q3.33$response)))
feelsDF[males,] <- recodeFactors(pregint, splitFrames$Q3.32)[males, ]
feelsDF[females,] <- recodeFactors(pregint, splitFrames$Q3.33)[females, ]


# removal of extra obj ----------------------------------------------------

rm(povFrame, simppov, simpPov, povertyComp, povData, povThresh,
    newEduLabs, newRelLabs, state.fips, females, males, regionMap, raceDF,
    newFrame, skip, nonSkips, finalSkips, mavoid, favoid, sitRecode, educLev)
