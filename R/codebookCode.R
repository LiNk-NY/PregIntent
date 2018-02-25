## Create the codebookSheet

## Use ALL codebook chunks to recode data
# Create codebook sheet with variables ------------------------------------

codebookSheet <- dplyr::bind_rows(codebook)
codebookSheet <- cbind.data.frame(
    codebname = rep(names(codebook), lapply(codebook, nrow)),
    codebookSheet, stringsAsFactors = FALSE)
codebookSheet <- dplyr::rename(codebookSheet, dataname = variable)

recodeBook <- readxl::read_excel("docs/recodeBook.xlsx")

codebookSheet[["recodeName"]] <- unlist(recodeBook[
    match(codebookSheet[["codebname"]], recodeBook[["qname"]]), "hname"])
altCodes <- is.na(codebookSheet[["recodeName"]])
codebookSheet[altCodes, "recodeName"] <- unlist(recodeBook[
    match(codebookSheet[altCodes, "dataname"], recodeBook[["qname"]]), "hname"])

dupsToCB <- unique(vapply(
    strsplit(vapply(strsplit(dupNames, "\\.\\."), `[`, character(1L), 1L), "_"),
    `[`, character(1L), 1L))

codebookSheet <- codebookSheet[!codebookSheet[["codebname"]] %in% dupsToCB, ]

naLogic <- is.na(codebookSheet[["recodeName"]]) &
    !is.na(codebookSheet[["corresponds"]])

codebookSheet[naLogic, "recodeName"] <-
    paste0(codebookSheet[naLogic, "dataname"], "..",
        codebookSheet[naLogic, "corresponds"])

## Fill in names that stay the same
constantNames <- is.na(codebookSheet[["recodeName"]])
codebookSheet[constantNames, "recodeName"] <- codebookSheet[constantNames, "dataname"]

commentsheet <- readxl::read_excel("docs/recodeBook.xlsx", sheet = 2L)

commentvars <- intersect(codebookSheet[["recodeName"]],
    commentsheet[["variable"]])
names(commentvars) <- commentvars


nrowsDoc <-
    vapply(commentvars, function(covar)
            sum(codebookSheet$recodeName %in% covar),
    integer(1L))

commentBlocks <- lapply(commentvars, function(covar) {
    chunk <- unlist(commentsheet[commentsheet$variable == covar, "comment"])
    chunkrows <- nrowsDoc[covar]
    chunkvals <- .wrapItem("", chunk, chunkrows)
    chunkvals[chunkvals == ""] <- NA_character_
    chunkvals
})

lapply(seq_along(commentBlocks), function(i, cblock) {
    varname <- commentvars[i]
    cblock <- commentBlocks[[i]]
    idxvals <- which(codebookSheet[["recodeName"]] %in% varname)
    for (i in seq_along(idxvals))
        codebookSheet[idxvals[i], "comment"] <<- cblock[i]
}, cblock = commentBlocks)

source("R/dfmap.R")

codebooksheet <- dplyr::left_join(codebookSheet, dfmap,
    by = c("recodeName", "response"))

# Write codebook
readr::write_csv(codebookSheet, "docs/codebookSheet.csv")
