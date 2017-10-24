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

## Make adjustments to odd variables
splitFrames <- adjustVarVal(splitFrames, c("Q1.7", "Q1.8", "Q2.2", "Q2.7"))

splitFrames <- c(splitFrames, Q122)

## Clean variables except the needed one
rm(list = ls()[!ls() %in% c("splitFrames", "pregint", "codebook",
    "recodeFactors", "adjustVarVal")])
