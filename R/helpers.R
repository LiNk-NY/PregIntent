# Set factors from recoding data frames
.setFactor <- function(dataset, recodeFrame) {
    dataset <- as.data.frame(dataset, stringsAsFactors = FALSE)
    indicVar <- unique(recodeFrame[["variable"]])
    target <- dataset[, indicVar, drop = FALSE]
    ## do for each value in recodeFrame rows
    factor(target, levels = recodeFrame[["value"]],
        labels = recodeFrame[["response"]])
}

