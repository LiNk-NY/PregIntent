## Dependencies
library(plyr)

# Set factors from recoding data frames
.recodeFactors <- function(dataset, recodeFrame) {
    dataset <- as.data.frame(dataset, stringsAsFactors = FALSE)
    indicVar <- unique(recodeFrame[["variable"]])
    target <- dataset[, indicVar, drop = FALSE]
    if (S4Vectors::isSingleString(indicVar)) {
        target[] <- factor(plyr::mapvalues(target[[1L]],
            from = recodeFrame[["value"]], to = recodeFrame[["response"]]))
    } else {
        stopifnot(identical(length(unique(recodeFrame[["variable"]])),
            length(target)))
        target[] <- lapply(seq_along(target), function(i) {
            factor(plyr::mapvalues(target[[i]],
                from = recodeFrame[["value"]][i],
                    to = recodeFrame[["response"]][i]))
        })
    }
    target
}
