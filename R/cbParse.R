# Package dependency
library(reshape2)

# Read text version of codebook
surv <- readLines("docs/Final_Pregnancy_Intentions_Survey.txt")

# Get index of lines with question numbers
starts <- grep("^Q[0-9]", surv, value = FALSE)
ends <- starts[-1]-1
starts <- starts[-length(starts)]

# Create ranges for valid chunks
ranges <- cbind(starts, ends)

# Read lines as chunks, one chunk per range/question
readChunks <- apply(ranges, 1L, function(x) {
    surv[seq(x[[1]], x[[2]])]
})

# Find chunks with close ended response values
withValues <- vapply(readChunks, function(x)
    any(grepl("\\([0-9]{1,2}\\)", x)), logical(1L))

# Take only chunks with response values
readChunks <- readChunks[withValues]

# Remove lines in chunks that are not responses
cleanChunks <- lapply(readChunks, function(x) {
    goodLines <- grep("^Q[0-9]|\\([0-9]{1,2}\\)", x, value = TRUE)
    grep("^Skip", goodLines, invert = TRUE, value = TRUE)
    })

QNums <- vapply(cleanChunks, function(x)
    vapply(strsplit(x[[1L]], " "), `[`, character(1L), 1L), character(1L))
names(cleanChunks) <- QNums

multipleCoded <- vapply(cleanChunks, function(x)
    any(grepl("^[A-Z]\\.", x)), logical(1L))

YesNoResponse <- cleanChunks[multipleCoded]
cleanChunks <- cleanChunks[!multipleCoded]

splitFrames <- lapply(cleanChunks, function(x) {
    x <- x[!(grepl("I would say I", x, fixed = TRUE) |
        grepl("I have...", x, fixed = TRUE))]
    cleanBlock(x)
})

Q122 <- lapply(YesNoResponse[2L], function(x) {
    x <- gsub("[A-Z]\\. ", "", x)
    cleanBlock(x)
})

Q3.5 <- lapply(YesNoResponse[1L], function(x) {
    responses <- grepl("^Yes|^No", x)
    x <- x[!responses]
    x <- gsub("[A-Z]\\. ", "", x)
    cleanBlock(x)
})
Q3.5 <- adjustVarVal(Q3.5, "Q3.5")

## Adjust for inconsistent names
splitFrames$Q3.17a$variable <- paste0(gsub("a", "", splitFrames$Q3.17a$variable),
    "_", splitFrames$Q3.17a$value)
splitFrames$Q3.17a$value <- rep(1L, length(splitFrames$Q3.17a$value))

## Make adjustments to odd variables
splitFrames <- adjustVarVal(splitFrames, c("Q1.7", "Q1.8", "Q2.2", "Q2.7"))

splitFrames <- c(splitFrames, Q122, Q3.5)

## PregFeel recode
splitFrames$Q3.2[4, "response"] <- "don't want me/partner pregnant"
splitFrames$Q3.3[4, "response"] <- "don't want me/partner pregnant"

## Education recode
splitFrames$Q1.10$response <- c("LT/some HS", "LT/some HS", "HS diploma/GED",
    "Some college", "College degree/Some Grad", "College degree/Some Grad",
    "Grad degree")

## Relationship recode
splitFrames$Q1.11$response <- c("single", rep("married/living/commit", 3),
    "div/sep/wid", "other")

## Rename variables in recoding scheme
splitFrames$Q3.12$variable <- "Q3.12_1"
splitFrames$Q3.13$variable <- "Q3.13_1"
splitFrames$Q2.12$variable <- "Q2.12_1"
splitFrames$Q2.13$variable <- "Q2.13_1"

## Current situation recode
sitRecode <- c("you/partner is pregnant",
    "would like you/partner to become pregnant soon",
    "don't want you/partner to become pregnant soon",
    "aren't trying, but would feel okay if you/partner became pregnant",
    "you/partner can't get pregnant",
    "other")

splitFrames$Q3.25$response <- sitRecode
splitFrames$Q3.26$response <- sitRecode

## if you/partner pregnant how would you feel?
splitFrames <- adjustVarVal(splitFrames, "Q3.32")
splitFrames <- adjustVarVal(splitFrames, "Q3.33")



## Clean variables except the needed one
rm(list = ls()[!ls() %in% c("splitFrames", "pregint", "codebook",
    "recodeFactors", "adjustVarVal")])
