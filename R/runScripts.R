## Script to run data cleaning and recoding
filesToSource <- c("install.R", "helpers.R", "clean.R", "cbParse.R", "recode.R")
filePaths <- file.path("R", filesToSource)

## Make sure all files exist
all(file.exists(filePaths))

## Source all scripts
invisible(lapply(filePaths, source))

## remove variables (if not already removed)
## rm(filesToSource, filePaths)

## Remove objs that were used for cleaning
variables <- ls()[!ls() %in% c("codebook", "recodeFactors",
    "splitFrames", "adjustVarVal")]
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
