# Read text version of codebook

surv <- readLines("docs/Final_Pregnancy_Intentions_Survey.txt")
starts <- grep("^Q[0-9]", surv, value = FALSE)
ends <- starts[-1]-1
starts <- starts[-length(starts)]
ranges <- cbind(starts, ends)
readChunks <- apply(ranges, 1L, function(x) {
    surv[seq(x[[1]], x[[2]])]
})

withValues <- vapply(readChunks, function(x)
    any(grepl("\\([0-9]{1,2}\\)", x)), logical(1L))

readChunks <- readChunks[withValues]

cleanChunks <- lapply(readChunks, function(x) {
    goodLines <- grep("^Q[0-9]|\\([0-9]{1,2}\\)", x, value = TRUE)
    grep("^Skip", goodLines, invert = TRUE, value = TRUE)
    })
