## Descriptives
library(dplyr)
library(tidyr)
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
data.frame(AnsPreg = sum(table(pregFeel)), N = 2099,
    Perc = round((sum(table(pregFeel))/2099)*100, 2))

## Ideal criteria
idealCrit <- vector("character", 2099)
splitFrames$Q3.2[4, "response"] <- "don't want me/partner pregnant"
splitFrames$Q3.3[4, "response"] <- "don't want me/partner pregnant"
idealCrit[females] <- .recodeFactors(pregint, splitFrames$Q3.3)[females]
idealCrit[males] <- .recodeFactors(pregint, splitFrames$Q3.2)[males]
table(idealCrit)

## Number of children
childnum <- gsub("NO", "0", pregint$Q1.9, ignore.case = TRUE)
childnum[childnum %in% c("mm", "na")] <- NA
childnum %>% table

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
raceDF <- .recodeFactors(pregint, splitFrames$Q1.8)

## Hispanic
hispanic <- .recodeFactors(pregint, splitFrames$Q1.6)
table(hispanic)
## Hispanic origin
hispOrg <- .recodeFactors(pregint, splitFrames$Q1.7)

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


## Education
splitFrames$Q1.10
## Modify labels to recode using function
newEduLabs <- c("LT/some HS", "LT/some HS", "HS diploma/GED", "Some college",
    "College degree/Some Grad", "College degree/Some Grad", "Grad degree")
splitFrames$Q1.10$response <- newEduLabs
educ <- .recodeFactors(pregint, splitFrames$Q1.10)
table(educ)

## Relationship
##  single
##  married or living together or committed rel
##  divorced/sep/wid
##  other
splitFrames$Q1.11
## Modify labels to recode using function
newRelLabs <- c("single", rep("married/living/commit", 3), "div/sep/wid", "other")
splitFrames$Q1.11$response <- newRelLabs
relationship <- .recodeFactors(pregint, splitFrames$Q1.11)


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
simpPov <- povFrame %>% dplyr::group_by(famUnit, childUnder18) %>%
    summarise(mPovThresh = mean(povThresh))
simpPov <- as.data.frame(simpPov)

simpPov$incGroup <- Hmisc::cut2(simpPov$mPovThresh,
    cuts = c(20000, 40000, 60000, 80000, 100000))
simpPov$incGroup <- factor(simpPov$incGroup, ordered = TRUE,
    levels = c(levels(simpPov$incGroup), "100000 or more"))
levels(simpPov$incGroup) <- splitFrames$Q1.14$response
simppov <- simpPov %>% unite(famch, famUnit, childUnder18)


## Get factor and order it
incCat <- .recodeFactors(pregint, splitFrames$Q1.14)[[1L]]
levels(incCat) <- levels(simppov$incGroup)
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
