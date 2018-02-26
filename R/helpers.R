## Dependencies
# Set factors from recoding data frames
recodeFactors <-
    function(dataset, recodeFrame, subsetVariable = "Q1.2",
        values = c(1, 2), labels = c("male", "female")) {
    dataset <- as.data.frame(dataset, stringsAsFactors = FALSE)
    indicVar <- as.character(unique(recodeFrame[["variable"]]))
    correspVar <- unique(recodeFrame[["corresponds"]])
    target <- as.data.frame(
        matrix(NA, nrow = nrow(dataset), ncol = length(indicVar)))

    stopifnot(identical(length(values), length(labels)))

    subvar <- dataset[[subsetVariable]]
    valueLogic <- vapply(values, function(x) subvar == x, logical(length(subvar)))
    subsetCat <- unique(recodeFrame[["subset"]])
    if (subsetCat != "none") {
        subValue <- values[match(subsetCat, labels)]
        ## only works for 2 categories
        varnames <- paste0(indicVar, "..", correspVar)
        indicLogic <- valueLogic[, values == subValue]
        target[indicLogic, ] <- dataset[indicLogic, indicVar, drop = FALSE]
        target[!indicLogic, ] <- dataset[!indicLogic, correspVar, drop = FALSE]
        names(target) <- varnames
    } else {
        target[] <- dataset[, indicVar, drop = FALSE]
        names(target) <- indicVar
    }
    if (S4Vectors::isSingleString(indicVar)) {
        target[] <- factor(plyr::mapvalues(target[[1L]],
            from = recodeFrame[["value"]], to = recodeFrame[["response"]]))
    } else {
        stopifnot(identical(length(indicVar), length(target)))
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

.findOptiWidth <- function(string, nrow, lengths = 1:10) {
    reslengths <- vapply(lengths, function(widths)
        length(strwrap(string, width = ceiling(nchar(string)/nrow)+widths)),
        integer(1L))
    optim <- lengths[which(reslengths == nrow)][[1L]]
    if (is.na(optim))
        stop("Increase lengths numbers")
    optim
}

.wrapItem <- function(block, descript, numrows = NULL) {
    descript <- unlist(descript)
    descript <- gsub("[ ]{2,}", "", descript)
    if (is.null(numrows))
        numrows <- nrow(block)
    if (!length(descript))
        return(rep("", times = numrows))
    lsnip <- nchar(descript)
    desc <- strwrap(descript, width = 40)
    remainder <- numrows - length(desc)
    if (remainder < 0L) {
        width <- .findOptiWidth(descript, numrows)
        return(strwrap(descript, width = ceiling(lsnip/numrows)+width))
    } else {
        append(desc, rep("", times = remainder))
    }
}

addQText <- function(idx, chunks, vardesctab) {
    varname <- names(chunks[idx])
    varname <- vapply(strsplit(varname, "_"), `[`, character(1L), 1L)
    availdesc <- vardesctab[ ,"item"] == varname
    if (all(!availdesc))
        return(rep("", nrow(chunks[[idx]])))
    descsnip <- vardesctab[, "description"][availdesc]
    .wrapItem(chunks[[idx]], descsnip)
}
