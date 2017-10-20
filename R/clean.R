## Cleaning script for pregnancy intentions dataset
library(readxl)

vars <- read_excel("data/surveyresults_2099.xlsx", n_max = 1)

namesVect <- unlist(vars[1, ])

vIdx <- grep("^V[0-9]", names(namesVect))
names(namesVect)[vIdx] <- namesVect[vIdx]

# Varnames and questions
codebook <- cbind(varname = names(namesVect), question = namesVect)

# Read actual dataset with cleaned column names
pregint <- read_excel("data/surveyresults_2099.xlsx", skip = 2,
    col_names = names(namesVect))

# Create vector of variables to remove
varsToRM <- c("ResponseSet", "Name", "ExternalDataReference", "EmailAddress",
    "IPAddress", "Status", "opp", "RISN", "V")

# Remove a few variables that have none or one response value
pregint <- pregint[, -which(names(pregint) %in% varsToRM)]

# Take only GC = 1 responses
pregint <- pregint[pregint$gc == 1L, ]

## Keep only desired variable
rm(list = ls()[-which(ls() == "pregint")])
