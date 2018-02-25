## Cleaning script for pregnancy intentions dataset
library(readxl)

vars <- read_excel("data/surveyresults_2099.xlsx", n_max = 1)

namesVect <- unlist(vars[1, ])

vIdx <- grep("^V[0-9]", names(namesVect))
names(namesVect)[vIdx] <- namesVect[vIdx]

# Varnames and questions
codebooktext <- cbind(item = names(namesVect), question = namesVect)

# Read actual dataset with cleaned column names
pregint <- suppressWarnings(
    as.data.frame(read_excel("data/surveyresults_2099.xlsx", skip = 2,
    col_names = names(namesVect)), stringsAsFactors = FALSE)
)

# Create vector of variables to remove
varsToRM <- c("ResponseSet", "Name", "ExternalDataReference", "EmailAddress",
    "IPAddress", "Status", "opp", "RISN", "V")

# Remove a few variables that have none or one response value
pregint <- pregint[, !names(pregint) %in% varsToRM]

# Take only GC = 1 responses
pregint <- pregint[pregint$gc == 1L, ]

## Remove commas from comments
commaincol <- vapply(pregint, function(col)
    any(grepl(",", col, fixed = TRUE)), logical(1L))

pregint[] <- apply(pregint, 2L, function(col) {
    gsub(",", "", col, fixed = TRUE)
})
pregint <- readr::type_convert(pregint)

## Keep only desired variable
rm(list = ls()[!ls() %in%
    c("pregint", "codebooktext", "recodeFactors",
      "adjustVarVal", "cleanBlock", "addQText")])
