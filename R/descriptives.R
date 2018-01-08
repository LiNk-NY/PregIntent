## Load pkg
library(broom)
library(dplyr)
library(tidyr)

pregint <- read.csv("data/pregint.csv")

ignoreVars <- c("ResponseID", "StartDate", "EndDate", "Finished",
    "SC0_0", "SC0_1", "SC0_2", "rid", "gc", "term", "LocationLatitude",
    "LocationLongitude", "LocationAccuracy",
    grep("TEXT", names(pregint), fixed = TRUE, value = TRUE))

validVars <- !names(pregint) %in% ignoreVars

summary(pregint[, validVars])
