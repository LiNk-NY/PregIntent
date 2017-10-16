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

codingFrames <- lapply(cleanChunks, function(x) {
    x <- x[!(grepl("I would say I", x, fixed = TRUE) |
        grepl("I have...", x, fixed = TRUE))]
    x <- gsub("\\(([A-Za-z ]*)\\)", "\\1", x)
    x <- gsub("specify", "", x, ignore.case = TRUE)
    codeFormats <- lapply(strsplit(x[-1], " \\("), function(y) {
        res <- trimws(gsub("\\)|:|_|⊗", "", y))
        res[!grepl("specify", res, fixed = TRUE)]
    })
    codeScheme <- do.call(rbind, codeFormats)
    colnames(codeScheme) <- c("response", "value")
    data.frame(codeScheme, stringsAsFactors = FALSE)
})

codingFrames <- suppressMessages(melt(codingFrames))
codingFrames <- codingFrames[, c("L1", "value", "response")]
names(codingFrames) <- c("variable", "value", "response")
codingFrames[, "value"] <- as.integer(codingFrames[["value"]])

splitFrames <- split(codingFrames, codingFrames[["variable"]])

## Make adjustments to odd variables
valSeq <- splitFrames$Q1.7$value
splitFrames$Q1.7$variable <- paste0(splitFrames$Q1.7$variable, "_", valSeq)
splitFrames$Q1.7$value <- rep(1L, length(valSeq))

valSeq <- splitFrames$Q1.8$value
splitFrames$Q1.8$variable <- paste0(splitFrames$Q1.8$variable, "_", valSeq)
splitFrames$Q1.8$value <- rep(1L, length(valSeq))
