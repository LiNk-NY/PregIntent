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

dataList <- lapply(codebook, function(recodeChunk) {
    recodeFactors(pregint, recodeChunk)
})

recodedData <- dplyr::bind_cols(dataList)
doubles <- grepl("\\.\\.", names(recodedData))
dupped <- duplicated(lapply(strsplit(names(recodedData)[doubles], "\\.\\."), sort))
dupNames <- names(recodedData)[doubles][dupped]

recodedData <- recodedData[, !(names(recodedData) %in% dupNames)]
recodedData <- type.convert(recodedData)

pregint <- pregint[, !names(pregint) %in%
    intersect(names(recodedData), names(pregint))]

pregData <- cbind.data.frame(pregint, recodedData)

## Save Dataset
write.csv(pregData, "data/pregint.csv", row.names = FALSE)
