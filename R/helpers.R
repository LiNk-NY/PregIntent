## Dependencies
library(plyr)

# Set factors from recoding data frames
recodeFactors <- function(dataset, recodeFrame) {
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

adjustVarVal <- function(codingList, variables) {
    for (variable in variables) {
    values <- codingList[[variable]][["value"]]
    varNames <- codingList[[variable]][["variable"]]
    codingList[[variable]][["variable"]] <- paste0(varNames, "_", values)
    codingList[[variable]][["value"]] <- rep(1L, length(values))
    }
    codingList
}

cleanBlock <- function(block) {
    varName <- vapply(strsplit(block[[1L]], " "), `[`, character(1L), 1L)
    block <- gsub("\\(([A-Za-z ]*)\\)", "\\1", block)
    block <- gsub("specify", "", block, ignore.case = TRUE)
    codeFormats <- lapply(strsplit(block[-1], " \\("), function(y) {
        res <- trimws(gsub("\\)|:|_|⊗", "", y))
        res[!grepl("specify", res, fixed = TRUE)]
    })
    codeScheme <- do.call(rbind, codeFormats)
    colnames(codeScheme) <- c("response", "value")
    data.frame(variable = varName, codeScheme[, c(2, 1)],
        subset = "", stringsAsFactors = FALSE)
}
