## Script to run data cleaning and recoding
filesToSource <- c(
    "install.R",
    "helpers.R",
    "clean.R",
    "codebookParse.R"
    # )
    ,
    "recode.R"
    )
filePaths <- file.path("R", filesToSource)

## Make sure all files exist
stopifnot(all(file.exists(filePaths)))

## Source all scripts
invisible(lapply(filePaths, source))

## remove variables (if not already removed)
## rm(filesToSource, filePaths)

## Remove objs that were used for cleaning
variables <- ls()[!ls() %in% c("codebooktext", "recodeFactors",
    "codebook", "adjustVarVal", "cleanChunks", "codebookSheet")]
names(variables) <- variables
dfsLog <- vapply(variables, function(x) is.data.frame(get(x)) && x != "pregint", logical(1L))
pullNames <- variables[dfsLog]

DFs <- do.call(cbind.data.frame,
        lapply(variables[dfsLog], function(x) {
            ax <- get(x)
            if (length(names(ax)) == 1L)
                names(ax) <- x
            else {
                names(ax)
            }
            ax
        })
)

DFvecs <- do.call(cbind.data.frame,
    lapply(variables[!dfsLog][-which(variables[!dfsLog] == "pregint")], get))

pregRec <- cbind(DFs, DFvecs)

names(pregRec) <- gsub("recodedData\\.|feelings\\.", "", names(pregRec))

pregint <- pregint[, !names(pregint) %in% intersect(names(pregRec), names(pregint))]

pregData <- cbind.data.frame(pregint, pregRec)

## Save Dataset
# readr::write_csv(pregData, "data/pregint.csv")
