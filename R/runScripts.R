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

dataList <- lapply(codebook, function(recodeChunk) {
    recodeFactors(pregint, recodeChunk)
})

dfs <- vapply(dataList, is.data.frame, logical(1L))
datafs <- dplyr::bind_cols(dataList[dfs])

recodedData <- as.data.frame(dplyr::bind_cols(dataList[!dfs], datafs))
doubles <- grepl("\\.\\.", names(recodedData))
dupped <- duplicated(lapply(strsplit(names(recodedData)[doubles], "\\.\\."), sort))
dupNames <- names(recodedData)[doubles][dupped]

recodedData <- recodedData[,
    !(names(recodedData) %in% c(dupNames, "childnum", "ageGroup"))]

recodedData <- readr::type_convert(recodedData)

recodes <- names(recodedData) %in% names(pregint)
names(recodedData)[recodes] <- paste0(names(recodedData)[recodes], "_R")

pregData <- cbind.data.frame(pregint, recodedData)

commasincol <- apply(pregData, 2L, function(col) {
    any(grepl(",", col, fixed = TRUE))
})

pregData[] <- apply(pregData, 2L, function(col) {
    gsub(",", "", col, fixed = TRUE)
})

## Save Dataset
write.csv(pregData, "data/pregint.csv", row.names = FALSE)
