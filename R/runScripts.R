## Script to run data cleaning and recoding
filesInDir <- list.files("R/")
filesToSource <- filesInDir[-which(filesInDir == "runScripts.R")]
fileOrd <- c(4, 3, 2, 1, 5)
filesToSource <- filesToSource[fileOrd]

## Source all scripts
invisible(lapply(file.path("R", filesToSource), source))

## remove variables (if not already removed)
# rm(filesInDir, filesToSource, fileOrd)


variables <- ls()[!ls() %in% c("codebook", "recodeFactors", "splitFrames")]
names(variables) <- variables
dfsLog <- vapply(variables, function(x) is.data.frame(get(x)), logical(1L))


DFs <- do.call(cbind,
        lapply(variables[dfsLog], function(x) get(x))
)
## Remove vars that already were recoded
pregint <- pregint[, -which(names(pregint) %in% grep("^Q", names(DFs), value = TRUE))]

DFs <- do.call(cbind,
        lapply(variables[dfsLog], function(x) get(x))
)

DFvecs <- do.call(cbind.data.frame, lapply(variables[!dfsLog], get))

pregRec <- cbind(DFs, DFvecs)

names(pregRec) <- gsub("pregint\\.", "", names(pregRec))
anyDuplicated(gsub("pregint\\.", "", names(pregRec)))

## Save Dataset
readr::write_csv(pregRec, "data/pregint.csv")
