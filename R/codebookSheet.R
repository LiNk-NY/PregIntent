## Create the codebookSheet

## Use ALL codebook chunks to recode data
dataList <- lapply(codebook, function(recodeChunk) {
    recodeFactors(pregint, recodeChunk)
})

recodedData <- dplyr::bind_cols(dataList)
doubles <- grepl("\\.\\.", names(recodedData))
dupped <- duplicated(lapply(strsplit(names(recodedData)[doubles], "\\.\\."), sort))
dupNames <- names(recodedData)[doubles][dupped]

recodedData <- recodedData[, !(names(recodedData) %in% dupNames)]

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
# codebookSheet[na.omit(match(commentsheet$variable, codebookSheet$recodeName)), "comment"] <-
#     commentsheet$description[na.omit(unique(match(codebookSheet$recodeName, commentsheet$variable)))]

# Write codebook
# readr::write_csv(codebookSheet, "docs/codebookCode.csv")

