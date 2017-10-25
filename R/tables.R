.meansd <- function(var, na.rm = TRUE) {
    objName <- as.character(substitute(var))
    var <- as.numeric(var)
    vec <- round(c(n = sum(!is.na(var)),
    mean = mean(var, na.rm = na.rm),
    stddev = sd(var, na.rm = na.rm)), 2)
    matrix(vec, nrow = 1, dimnames = list(objName, names(vec)))
}

.prop <- function(var) {
    if (is.data.frame(var))
        var <- var[[1L]]
    counts <- matrix(table(var), ncol = 1,
        dimnames = list(names(table(var)), "n"))
    props <- matrix(round(prop.table(table(var))*100, 1), ncol = 1,
        dimnames = list(names(table(var)), "perc"))
    cbind.data.frame(counts, props)
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
## Bivariable Table 1
rbind(.meansd(age),
.meansd(childnum)
)
cbind(
rbind(.prop(gender),
.prop(regionOrg),
.prop(hispanic),
.prop(educ),
.prop(idealCrit),
.prop(avoidPreg),
.prop(pregControl)
),

rbind(.crossTab(gender, pregFeel),
      .crossTab(regionOrg, pregFeel),
      .crossTab(hispanic, pregFeel),
      .crossTab(educ, pregFeel),
      .crossTab(idealCrit, pregFeel),
      .crossTab(avoidPreg, pregFeel),
      .crossTab(pregControl, pregFeel)
)
)
