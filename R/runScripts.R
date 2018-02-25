## Script to run data cleaning and recoding
filesToSource <- c(
    "install.R",
    "helpers.R",
    "clean.R",
    "codebookParse.R",
    "recode.R")
filePaths <- file.path("R", filesToSource)

## Make sure all files exist
stopifnot(all(file.exists(filePaths)))

## Source all scripts
invisible(lapply(filePaths, source))

## remove variables (if not already removed)
## rm(filesToSource, filePaths)

pregint <- pregint[, !names(pregint) %in%
    intersect(names(recodedData), names(pregint))]

pregData <- cbind.data.frame(pregint, recodedData)

## Save Dataset
# readr::write_csv(pregData, "data/pregint.csv")
