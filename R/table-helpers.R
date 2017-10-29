## Helper functions for constructing results
.meansd <- function(var, na.rm = TRUE) {
    objName <- as.character(substitute(var))
    var <- as.numeric(var)
    m <- round(mean(var, na.rm = na.rm), 2)
    stddev <- round(sd(var, na.rm = na.rm), 2)
    matrix(paste0(m, " (", stddev, ")"), ncol = 1L,
        dimnames = list(objName, "M (SD)"))
}

.prop <- function(var) {
    if (is.data.frame(var))
        var <- var[[1L]]
    counts <- table(var)
    props <- round(prop.table(table(var))*100, 1)
    vals <- paste0(counts, " (", round(props, 2), ")")
    matrix(vals, ncol = 1, dimnames = list(names(table(var)), "n (%)"))
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
    numVar <- as.numeric(numVar)
    splitSet <- split(numVar, outcome)
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
