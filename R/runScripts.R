## Script to run data cleaning and recoding
filesInDir <- list.files("R/")
filesToSource <- filesInDir[-which(filesInDir == "runScripts.R")]
fileOrd <- c(4, 3, 2, 1, 5)
filesToSource <- filesToSource[fileOrd]

## Source all scripts
invisible(lapply(file.path("R", filesToSource), source))

## remove variables
rm(filesInDir, filesToSource, fileOrd)
