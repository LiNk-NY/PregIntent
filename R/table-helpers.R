## Helper functions for constructing results
.meansd <- function(numVar, na.rm = TRUE, varName = NULL, digits = 2) {
    if (is.null(varName)) {
    varName <- as.character(substitute(numVar))
    varName <- varName[[length(varName)]]
    }
    stopifnot(S4Vectors::isSingleString(varName))
    numVar <- as.numeric(numVar)
    m <- round(mean(numVar, na.rm = na.rm), digits)
    stddev <- round(sd(numVar, na.rm = na.rm), digits)
    matrix(paste0(m, " (", stddev, ")"), ncol = 1L,
        dimnames = list(varName, "M (SD)"))
}

.validperc <- function(numVar, digits = 2, varName = NULL) {
    if (is.null(varName)) {
    varName <- as.character(substitute(numVar))
    varName <- varName[[length(varName)]]
    }
    tally <- sum(!is.na(numVar))
    perc <- round((tally/length(numVar))*100, digits)
    matrix(paste0(tally, " (", perc, ")"), ncol = 1L,
        dimnames = list(varName, "valid (%)"))
}

.prop <- function(numVar, digits = 1) {
    if (is.data.frame(numVar))
        numVar <- numVar[[1L]]
    counts <- table(numVar)
    props <- round(prop.table(table(numVar))*100, digits)
    vals <- paste0(counts, " (", props, ")")
    matrix(vals, ncol = 1, dimnames = list(names(table(numVar)), "n (%)"))
}

.validsum <- function(numVar, digits = 1) {
    varName <- as.character(substitute(numVar))
    if (is.data.frame(numVar))
        numVar <- numVar[[1L]]
    totvalid <- sum(table(numVar))
    totwithna <- sum(table(numVar, useNA = "always"))
    perc <- round((totvalid/totwithna)*100, digits)
    vals <- paste0(totvalid, " (", perc, ")")
    nlev <- length(names(table(numVar)))
    matrix(vals, ncol = 1, dimnames = list("valid (%)", "n (%)"))
}

.crossTab <- function(var1, var2, digits = 2) {
    if (is.data.frame(var1))
        var1 <- var1[[1L]]
    if (is.data.frame(var2))
        var2 <- var2[[1L]]
    counts <- table(var1, var2)
    props <- round(prop.table(table(var1, var2), 1L)*100, digits)
    crossnames <- dimnames(counts)
    vals <- paste0(counts, " (", props, ")")
    matrix(vals, ncol = 2, dimnames = crossnames)
}

.groupMeans <- function(numVar, outcome, digits = 2) {
    varName <- as.character(substitute(numVar))
    varName <- varName[[length(varName)]]
    stopifnot(S4Vectors::isSingleString(varName))

    numVar <- as.numeric(numVar)
    splitSet <- split(numVar, outcome)
    ## Enforce levels
    splitSet <- splitSet[levels(outcome)]
    groupNames <- names(splitSet)
    res <- vapply(seq_along(splitSet), function(i, x) {
        m <- round(mean(x[[i]], na.rm = TRUE), digits)
        std <- round(sd(x[[i]], na.rm = TRUE), digits)
        paste0(m, " (", std, ")")
    }, character(1L), x = splitSet)
    matrix(res, nrow = 1L, dimnames = list(varName, groupNames))
}

.ttestPval <- function(numVar, outcome, varName = NULL) {
    if (is.null(varName))
    varName <- as.character(substitute(numVar))
    numVar <- as.numeric(numVar)
    matrix(
        Hmisc::format.pval(pv =  t.test(numVar ~ outcome)$p.value, digits = 2,
            eps = 0.001,  nsmall = 3), nrow = 1L,
            dimnames = list(varName, "p.value"))
}

.chitestPval <- function(catVar, outcome) {
    if (is.data.frame(catVar))
        catVar <- catVar[[1L]]
    labels <- names(table(catVar))
    lvls <- length(labels)
    matrix(c(Hmisc::format.pval(pv = chisq.test(catVar, outcome)$p.value,
        digits = 3, eps = 0.001, nsmall = 3), rep("", lvls-1)), ncol = 1L,
        dimnames = list(labels, "p.value"))
}

.fishertestPval <- function(catVar, outcome) {
    if (is.data.frame(catVar))
        catVar <- catVar[[1L]]
    labels <- names(table(catVar))
    lvls <- length(labels)
    matrix(c(Hmisc::format.pval(pv = fisher.test(catVar, outcome,
        workspace = 800000)$p.value, digits = 2, eps = 0.001, nsmall = 3),
        rep("", lvls-1)), ncol = 1L, dimnames = list(labels, "p.value"))
}

.comparisonTable <- function(..., outcome, data, headerRow = NULL,
    headerFrame = NULL, includerHeader = TRUE, deparse.level = 2, digits = 2)
{
    listvars <- as.list(substitute(list(...)))[-1L]
    nams <- vapply(listvars, function(x) {
        switch(deparse.level + 1L,
        "", if (is.symbol(x)) as.character(x) else "",
        gsub("\\w+\\$", "", deparse(x, nlines = 1L)[1L]))
        }, character(1L))

    outname <- rev(as.character(substitute(outcome)))[1L]
    if (!missing(data)) {
        args <- as.list(data[, nams])
        if (length(outname) == 1L)
            outcome <- data[, outname]
        else
            stop("Provide a single outcome")
    } else {
        args <- list(...)
    }
    if (!is.factor(outcome))
        outcome <- as.factor(outcome)
    ## code from table()
    lengthArgs <- seq_along(args)

    if (!is.null(headerRow))
        names(lengthArgs) <- names(args) <- headerRow
    else if (!is.null(headerFrame))
        names(lengthArgs) <- names(args) <- headerRow <-
            headerFrame[[2L]][match(nams, headerFrame[[1L]])]
    else
        names(lengthArgs) <- names(args) <- nams

    outlevels <- rownames(contrasts(outcome))
    headrow <- if (includerHeader) {
        list(matrix(
            c(paste0("N = ", dim(data)[[1]]),
            paste0("n = ", table(outcome)[outlevels]), ""), nrow = 1L,
            dimnames = list("Characteristic", c("n (%)",
                paste(outname, outlevels, sep = "-"), "p.value"))
        ))
    } else {
            NULL
    }

    numeric <- vapply(args, is.numeric, logical(1L))
    results <- lapply(lengthArgs, function(i, x, compVar) {
        vari <- x[[i]]
        if (is.data.frame(vari))
            vari <- vari[[1L]]
        if (is.character(vari))
            vari <- as.factor(vari)
        if (numeric[[i]]) {
            cbind(.meansd(vari, varName = names(x[i]), digits = digits),
                .groupMeans(vari, compVar, digits = digits),
                .ttestPval(vari, compVar, varName = names(x[i])))
        } else {
            fourth <- .chitestPval(vari, compVar)[[1L]]
            if (!is.null(headerRow)) {
                header <- matrix(c(rep("", 3L), fourth), nrow = 1L,
                    dimnames = list(headerRow[[i]], NULL))
                p.value <- rep("", length(levels(vari)))
            } else {
                header <- character(0L)
                p.value <- c(fourth, rep("", length(levels(vari))-1))
            }
            rbind(header,
            cbind(.prop(vari, digits = digits),
                .crossTab(vari, compVar, digits = digits),
                p.value))
        }
    }, compVar = outcome, x = args)
    c(headrow, results)
}

## Fix caps in categories
simpleCap <- function(x) {
    unname(vapply(x, function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep="", collapse=" ")
    }, character(1L)))
}
