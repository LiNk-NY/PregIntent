## Create the codebookCode

## Use ALL codebook chunks to recode data
# Create codebook sheet with variables ------------------------------------

codebookCode <- dplyr::bind_rows(codebook)
codebookCode <- cbind.data.frame(
    codebname = rep(names(codebook), lapply(codebook, nrow)),
    codebookCode, stringsAsFactors = FALSE)
codebookCode <- dplyr::rename(codebookCode, dataname = variable)

recodeBook <- readxl::read_excel("docs/recodeBook.xlsx")

codebookCode[["recodeName"]] <- unlist(recodeBook[
    match(codebookCode[["codebname"]], recodeBook[["qname"]]), "hname"])
altCodes <- is.na(codebookCode[["recodeName"]])
codebookCode[altCodes, "recodeName"] <- unlist(recodeBook[
    match(codebookCode[altCodes, "dataname"], recodeBook[["qname"]]), "hname"])

dupsToCB <- unique(vapply(
    strsplit(vapply(strsplit(dupNames, "\\.\\."), `[`, character(1L), 1L), "_"),
    `[`, character(1L), 1L))

codebookCode <- codebookCode[!codebookCode[["codebname"]] %in% dupsToCB, ]

naLogic <- is.na(codebookCode[["recodeName"]]) &
    !is.na(codebookCode[["corresponds"]])

codebookCode[naLogic, "recodeName"] <-
    paste0(codebookCode[naLogic, "dataname"], "..",
        codebookCode[naLogic, "corresponds"])

## Fill in names that stay the same
constantNames <- is.na(codebookCode[["recodeName"]])
codebookCode[constantNames, "recodeName"] <-
    paste0(codebookCode[constantNames, "dataname"], "_R")

codebookCode[] <- apply(codebookCode, 2L, function(col) {
    gsub(",", "", col, fixed = TRUE)
})

commentsheet <- readxl::read_excel("docs/recodeBook.xlsx", sheet = 2L)

commentvars <- intersect(codebookCode[["recodeName"]],
    commentsheet[["variable"]])
names(commentvars) <- commentvars


nrowsDoc <-
    vapply(commentvars, function(covar)
            sum(codebookCode$recodeName %in% covar),
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
    idxvals <- which(codebookCode[["recodeName"]] %in% varname)
    for (i in seq_along(idxvals))
        codebookCode[idxvals[i], "comment"] <<- cblock[i]
}, cblock = commentBlocks)

source("R/dfmap.R")

codebookCode <- dplyr::left_join(codebookCode, dfmap,
    by = c("recodeName", "response"))

# Write codebook
readr::write_csv(codebookCode, "docs/codebookCode.csv")
