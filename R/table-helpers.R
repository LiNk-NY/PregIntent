## Helper functions for constructing results
.meansd <- function(numVar, na.rm = TRUE, varName = NULL) {
    if (is.null(varName)) {
    varName <- as.character(substitute(numVar))
    varName <- varName[[length(varName)]]
    }
    stopifnot(S4Vectors::isSingleString(varName))
    numVar <- as.numeric(numVar)
    m <- round(mean(numVar, na.rm = na.rm), 2)
    stddev <- round(sd(numVar, na.rm = na.rm), 2)
    matrix(paste0(m, " (", stddev, ")"), ncol = 1L,
        dimnames = list(varName, "M (SD)"))
}

.prop <- function(numVar) {
    if (is.data.frame(numVar))
        numVar <- numVar[[1L]]
    counts <- table(numVar)
    props <- round(prop.table(table(numVar))*100, 1)
    vals <- paste0(counts, " (", round(props, 2), ")")
    matrix(vals, ncol = 1, dimnames = list(names(table(numVar)), "n (%)"))
}

.crossTab <- function(var1, var2) {
    if (is.data.frame(var1))
        var1 <- var1[[1L]]
    if (is.data.frame(var2))
        var2 <- var2[[1L]]
    counts <- table(var1, var2)
    props <- prop.table(table(var1, var2), 1L)
    crossnames <- dimnames(counts)
    vals <- paste0(counts, " (", round(props, 2), ")")
    matrix(vals, ncol = 2, dimnames = crossnames)
}

.groupMeans <- function(numVar, outcome) {
    varName <- as.character(substitute(numVar))
    varName <- varName[[length(varName)]]
    stopifnot(S4Vectors::isSingleString(varName))

    numVar <- as.numeric(numVar)
    splitSet <- split(numVar, outcome)
    ## Enforce alphabetical order
    splitSet <- splitSet[order(na.omit(unique(outcome)))]
    groupNames <- names(splitSet)
    res <- vapply(seq_along(splitSet), function(i, x) {
        m <- round(mean(x[[i]], na.rm = TRUE), 2)
        std <- round(sd(x[[i]], na.rm = TRUE), 2)
        paste0(m, " (", std, ")")
    }, character(1L), x = splitSet)
    resMat <- matrix(res, nrow = 1L, dimnames = list(varName, groupNames))
    resMat[, rev(groupNames), drop = FALSE]
}

.ttestPval <- function(numVar, outcome) {
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

.comparisonTable <- function(..., outcome, headerRow = NULL, outcomeOrder = NULL, deparse.level = 2) {
    listvars <- as.list(substitute(list(...)))[-1L]
    nams <- vapply(listvars, function(x) {
        switch(deparse.level + 1L,
        "", if (is.symbol(x)) as.character(x) else "",
        gsub("\\w+\\$", "", deparse(x, nlines = 1L)[1L]))
        }, character(1L))
    args <- list(...)
    names(args) <- nams
    numerics <- vapply(args, is.numeric, logical(1L))
    lapply(seq_along(args), function(i, x, compVar) {
        if (is.numeric(x[[i]])) {
            cbind(.meansd(x[[i]], varName = names(x[i])), .groupMeans(x[[i]], compVar))
        } else {
            cbind(.prop(x[[i]]), .crossTab(x[[i]], compVar))
        }
    }, compVar = outcome, x = args)
}
