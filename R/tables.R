# Table 1 Sociodemographics x Feelings about Pregnancy --------------------

## Bivariable Table 1
numericPortion <- cbind(
    rbind(.meansd(age), .meansd(childnum) ),
    rbind(.groupMeans(age, pregFeel), .groupMeans(childnum, pregFeel))
    )
ttestres <- rbind(.ttestPval(age, pregFeel), .ttestPval(childnum, pregFeel))

## Bind means and p.vals
(nums <- cbind(numericPortion, ttestres ))

categorical <- cbind(
    rbind(.prop(gender), .prop(regionOrg), .prop(hispanic), .prop(educ),
        .prop(idealCrit), .prop(avoidPreg)),
    rbind(.crossTab(gender, pregFeel), .crossTab(regionOrg, pregFeel),
        .crossTab(hispanic, pregFeel), .crossTab(educ, pregFeel),
        .crossTab(idealCrit, pregFeel), .crossTab(avoidPreg, pregFeel),
        .crossTab(pregPlan, pregFeel))
    )

chitestres <- rbind(.chitestPval(gender, pregFeel),
    .chitestPval(regionOrg, pregFeel), .chitestPval(hispanic, pregFeel),
    .chitestPval(educ, pregFeel), .chitestPval(idealCrit, pregFeel),
    .chitestPval(avoidPreg, pregFeel), .chitestPval(pregPlan, pregFeel))

## Bind chisq tests and p.vals
(cats <- cbind(categorical, chitestres))

tab1 <- rbind(nums, cats)

## Fix caps in categories
simpleCap <- function(x) {
    unname(vapply(x, function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep="", collapse=" ")
    }, character(1L)))
}

rownames(tab1) <- simpleCap(rownames(tab1))

write.csv(tab1, file = "data/table1.csv")
