## Descriptives
library(plyr)
library(dplyr)
library(tidyr)
library(maps)
library(Hmisc)
library(magrittr)
library(forcats)

data("state.fips")

# Sex
pregint$sex <- unlist(recodeFactors(pregint, codebook$Q1.2), use.names = FALSE)
males <- pregint$sex == "male"
females <- pregint$sex == "female"

## Outcome
## "Ultimately, how would you feel about being pregnant right now?"
pregint$pregFeel <- unlist(recodeFactors(pregint, codebook$Q3.34), use.names = FALSE)

data.frame(AnsPreg = sum(table(pregint$pregFeel)), N = 2099,
    Perc = round((sum(table(pregint$pregFeel))/2099)*100, 2))

## Ideal criteria
pregint$idealCrit <- unlist(recodeFactors(pregint, codebook$Q3.3), use.names = FALSE)

## Number of children
childnum <- gsub("NO", "0", pregint$Q1.9, ignore.case = TRUE)
childnum[childnum %in% c("mm", "na")] <- NA
pregint$childnum <- as.integer(childnum)

## Reference dataset for regions
regionMap <- state.fips[!duplicated(state.fips$fips), c("fips", "region", "polyname")]
names(regionMap) <- c("fips", "region", "state")
regionMap$state <- gsub("\\:.*$", "", regionMap$state)
regionMap$region <- factor(regionMap$region, levels = 1:4,
    labels = c("North East", "MidWest", "South", "West"))
regionMap <- rbind(regionMap, data.frame(fips = c(2, 15), region = "West",
    state = c("alaska", "hawaii")))

## State of residence (all should reside in US)
pregint$stateOrg <-
    unlist(recodeFactors(pregint, codebook$Q110), use.names = FALSE)

## Region of residence
pregint$regionOrg <-
    regionMap$region[match(tolower(as.character(pregint$stateOrg)),
        regionMap$state)]

## check proper merge
## head(cbind(stateOrg, regionOrg))

## Race (select all that apply)
raceDF <- recodeFactors(pregint, codebook$Q1.8)

race <- vector("character", 2099)
multiSelect <- rowSums(!is.na(raceDF)) > 1L

## Squash single select columns
race[!multiSelect] <- apply(raceDF[!multiSelect, ], 1L,  function(row) {
    if (all(is.na(row)))
        NA_character_
    else
    na.omit(row)
})

## More than 1 gets coded as OTHER
race[multiSelect] <- "other"

pregint$race <- race

## Hispanic
pregint$hispanic <- unlist(recodeFactors(pregint, codebook$Q1.6), use.names = FALSE)

## Hispanic origin
hispoDF <- recodeFactors(pregint, codebook$Q1.7)

hispOrg <- vector("character", 2099)
multiSelect <- rowSums(!is.na(hispoDF)) > 1L

## Squash single select columns
hispOrg[!multiSelect] <- apply(hispoDF[!multiSelect, ], 1L, function(row) {
    if (all(is.na(row)))
        NA_character_
    else
        na.omit(row)
})
hispOrg[multiSelect] <- "other"
pregint$hispOrg <- hispOrg

## Check to see if all who said "Yes" Hispanic answered origin question
## equal number of responses in both variables after subsetting by "yes"
## responses
# sum(!apply(hispOrg[hispanic[[1L]] == "yes",], 1L,
#     function(row) all(is.na(row))))

## Age
pregint$age <- pregint$Q112

## ageGroup
pregint$ageGroup <- Hmisc::cut2(pregint$age, cuts = c(25, 30, 35, 40))
levels(pregint$ageGroup) <- c("21-24", "25-29", "30-34", "35-39", "40-44")

## Modify labels to recode using function
pregint$educ <-
    unlist(recodeFactors(pregint, codebook$Q1.10), use.names = FALSE)

## Relationshp recode
pregint$relationship <-
    unlist(recodeFactors(pregint, codebook$Q1.11), use.names = FALSE)

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
levels(simpPov$incGroup) <- codebook$Q1.14$response

simppov <- simpPov %>% unite(famch, famUnit, childUnder18)


## Get factor and order it
pregint$incCat <-
    unlist(recodeFactors(pregint, codebook$Q1.14), use.names = FALSE)
levels(pregint$incCat) <- codebook$Q1.14$response
pregint$incCat <- factor(pregint$incCat, ordered = TRUE)
povData <- cbind.data.frame(famUn = pregint$Q1.12,
    childUn18 = pregint$Q1.13, incCat = pregint$incCat)
povData <- povData %>% unite(famch, famUn, childUn18)

povertyComp <- cbind.data.frame(povFromTable =
    simppov[match(povData$famch, simppov$famch), "incGroup"],
    povFromData = povData$incCat)
pregint$underPovLevel <-
    factor(povertyComp[["povFromData"]] <= povertyComp[["povFromTable"]],
        levels = c(TRUE, FALSE), labels = c("Yes", "No"))

# Pregnancy can be avoided
avoidPreg <- vector("character", 2099)

mavoid <- vector("character", 2099)[males]

newFrame <- recodeFactors(pregint, codebook$Q2.2)[males, ]
skip <- rowSums(!is.na(newFrame)) > 1
nonSkips <- sapply(newFrame[!skip, ], as.character)[!is.na(newFrame[!skip, ])]
finalSkips <- recodeFactors(pregint, codebook$Q2.5)[males, ][skip]
mavoid[skip] <- as.character(finalSkips)
mavoid[!skip] <- nonSkips

favoid <- vector("character", 2099)[females]

newFrame <- recodeFactors(pregint, codebook$Q2.7)[females, ]
skip <- rowSums(!is.na(newFrame)) > 1L
nonSkips <- sapply(newFrame[!skip, ], as.character)[!is.na(newFrame[!skip, ])]
finalSkips <- recodeFactors(pregint, codebook$Q2.10)[females, ][skip]
favoid[skip] <- as.character(finalSkips)
favoid[!skip] <- nonSkips

avoidPreg[males] <- mavoid
avoidPreg[females] <- favoid

pregint$avoidPreg <- factor(avoidPreg == "can be avoided",
    levels = c(TRUE, FALSE), labels = c("Yes", "No"))

pregPlan <- vector("character", 2099)
newFrame <- recodeFactors(pregint, codebook$Q3.5)
nonSkips <- rowSums(newFrame != "No")  == 1L
finalSkips <- recodeFactors(pregint, codebook$Q122)[!nonSkips, ]
pregPlan[!nonSkips] <- as.character(finalSkips)
pregPlan[nonSkips] <- sapply(newFrame[nonSkips, ], as.character)[
    newFrame[nonSkips, ] != "No"]
pregPlan[pregPlan %in% c("can be planned in advance",
    "can be planned in discussion with your partner",
    "can be planned to happen after one's ideal criteria are fulfilled")] <- "Yes"
pregPlan[pregPlan %in% c("'just happens'",
  "can be left to 'fate' or a higher power like God",
  "is a natural process that happens when it’s meant to be")] <- "No"
pregPlan[pregPlan %in% "other"] <- NA_character_
pregint$pregPlan <- pregPlan

becomeControl <-
    unlist(recodeFactors(pregint, codebook$Q3.12), use.names = FALSE)

becomeControl %<>%
    fct_collapse(`Low control` = c("no control", "a little control"),
        `High control` = c("complete control", "a lot of control"))
pregint$becomeControl <- becomeControl

## Select all that apply question but not indicated in actual question
# recodeFactors(pregint, codebook$Q3.17a)

avoidControl <-
    unlist(recodeFactors(pregint, codebook$Q2.12), use.names = FALSE)

avoidControl %<>%
    fct_collapse(`Low control` = c("no control", "a little control"),
        `High control` = c("complete control", "a lot of control"))
pregint$avoidControl <- avoidControl

pregint$currentSit <-
    unlist(recodeFactors(pregint, codebook$Q3.25), use.names = FALSE)

## Keep only desired variable
rm(list = ls()[!ls() %in%
    c("pregint", "codebooktext", "recodeFactors", "codebook",
      "adjustVarVal", "cleanBlock", "addQText")])
