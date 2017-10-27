# Table 1 Sociodemographics x Feelings about Pregnancy --------------------
# Totals by group
table(pregFeel)

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
        .prop(idealCrit), .prop(avoidPreg), .prop(pregPlan)),
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

# Table 2 - Control over Pregnancy ----------------------------------------
## Totals by group
table(avoidControl)

nums0 <- cbind(
    rbind(.groupMeans(age, avoidControl), .groupMeans(childnum, avoidControl)),
    rbind(.ttestPval(age, avoidControl), .ttestPval(childnum, avoidControl))
    )

categorical0 <- rbind(.crossTab(gender, avoidControl), .crossTab(regionOrg, avoidControl),
        .crossTab(hispanic, avoidControl), .crossTab(educ, avoidControl),
        .crossTab(idealCrit, avoidControl), .crossTab(avoidPreg, avoidControl),
        .crossTab(pregPlan, avoidControl))

chitestres0 <- rbind(.chitestPval(gender, avoidControl),
    .chitestPval(regionOrg, avoidControl), .chitestPval(hispanic, avoidControl),
    .chitestPval(educ, avoidControl), .chitestPval(idealCrit, avoidControl),
    .chitestPval(avoidPreg, avoidControl), .chitestPval(pregPlan, avoidControl))

cats0 <- cbind(categorical0, chitestres0)

leftside <- rbind(nums0, cats0)

## Total by group
table(becomeControl)

nums1 <- cbind(
    rbind(.groupMeans(age, becomeControl), .groupMeans(childnum, becomeControl)),
    rbind(.ttestPval(age, becomeControl), .ttestPval(childnum, becomeControl))
    )

categorical1 <- rbind(.crossTab(gender, becomeControl), .crossTab(regionOrg, becomeControl),
        .crossTab(hispanic, becomeControl), .crossTab(educ, becomeControl),
        .crossTab(idealCrit, becomeControl), .crossTab(avoidPreg, becomeControl),
        .crossTab(pregPlan, becomeControl))

chitestres1 <- rbind(.chitestPval(gender, becomeControl),
    .chitestPval(regionOrg, becomeControl), .chitestPval(hispanic, becomeControl),
    .chitestPval(educ, becomeControl), .chitestPval(idealCrit, becomeControl),
    .chitestPval(avoidPreg, becomeControl), .chitestPval(pregPlan, becomeControl))

cats1 <- cbind(categorical1, chitestres1)

rightside <- rbind(nums1, cats1)

## AVOID CONTROL // BECOME CONTROL
tab2 <- cbind(leftside, rightside)

rownames(tab2) <- simpleCap(rownames(tab2))

write.csv(tab2, "data/table2.csv")
