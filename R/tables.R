.meansd <- function(var, na.rm = TRUE) {
    var <- as.numeric(var)
    round(c(n = sum(!is.na(var)),
    mean = mean(var, na.rm = na.rm),
    stddev = sd(var, na.rm = na.rm)), 2)
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
sum(table(gender[[1]]))
table(gender[[1]])

.meansd(childnum)

.prop(regionOrg)

.meansd(age)

.prop(hispanic)
.prop(educ)
.prop(idealCrit)
.prop(avoidPreg)
