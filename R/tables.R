.meansd <- function(var, na.rm = TRUE) {
    objName <- as.character(substitute(var))
    var <- as.numeric(var)
    vec <- round(c(n = sum(!is.na(var)),
    mean = mean(var, na.rm = na.rm),
    stddev = sd(var, na.rm = na.rm)), 2)
    matrix(vec, nrow = 1, dimnames = list(as.character(substitute(var)), names(vec)))
}

.prop <- function(var) {
    if (is.data.frame(var))
        var <- var[[1L]]
    counts <- matrix(table(var), ncol = 1,
        dimnames = list(names(table(var)), "n"))
    props <- matrix(round(prop.table(table(var)), 2), ncol = 1,
        dimnames = list(names(table(var)), "perc"))
    cbind.data.frame(counts, props)
}
## Bivariable Table 1
rbind(.meansd(age),
.meansd(childnum)
)

rbind(.prop(gender),
.prop(regionOrg),
.prop(hispanic),
.prop(educ),
.prop(idealCrit),
.prop(avoidPreg)
)
