# Package dependency
library(reshape2)

# Read text version of codebook
surv <- readLines("docs/Final_Pregnancy_Intentions_Survey.txt")

# Get index of lines with question numbers
starts <- grep("^Q[0-9]|^Display", surv, value = FALSE)
ends <- starts[-1]-1
starts <- starts[-length(starts)]

# Create ranges for valid chunks
ranges <- cbind(starts, ends)

# Read lines as chunks, one chunk per range/question
readChunks <- apply(ranges, 1L, function(x) {
    surv[seq(x[[1]], x[[2]])]
})

## Remove empty characters
readChunks <- lapply(readChunks, function(x) trimws(x[x != ""]))

## Find "Display This Question" chunks
startDisp <- vapply(readChunks, function(x)
    any(grepl("^Display", x, ignore.case = TRUE)), logical(1L))

withDisplay <- lapply(which(startDisp), function(x)
    unlist(readChunks[c(x, x+1)]))
woDisplay <- readChunks[!seq_along(readChunks) %in%
    c(which(startDisp), which(startDisp)+1)]

readChunks <- c(withDisplay, woDisplay)
# Find chunks with close ended response values
withValues <- vapply(readChunks, function(x)
    any(grepl("\\([0-9]{1,2}\\)", x)), logical(1L))

# Take only chunks with response values
readChunks <- readChunks[withValues]

# Get only sex word from codebook chunk
sexCond <- lapply(readChunks, function(x) gsub("(.*)\\s(.*male)$", "\\2",
    grep("^If.*male$", x, value = TRUE, ignore.case = TRUE)))
sexCond[!lengths(sexCond)] <- "none"

# Remove lines in chunks that are not responses
cleanChunks <- lapply(readChunks, function(x) {
    goodLines <- grep("^Q[0-9]|\\([0-9]{1,2}\\)", x, value = TRUE)
    grep("^Skip", goodLines, invert = TRUE, value = TRUE)
    })

QNums <- vapply(cleanChunks, function(x)
    vapply(strsplit(x[[1L]], " "), `[`, character(1L), 1L), character(1L))
names(cleanChunks) <- QNums

multipleCoded <- vapply(cleanChunks, function(x)
    any(grepl("^[A-Z]\\.", x)), logical(1L))

YesNoResponse <- cleanChunks[multipleCoded]
cleanChunks <- cleanChunks[!multipleCoded]
sexCond <- sexCond[!multipleCoded]

## Save variable names and descriptions
vardesc <- vapply(c(cleanChunks, YesNoResponse), `[`, character(1L), 1L)
vardesc <- gsub("^Q[0-9.]{1,4}[a-d]?\\s", "", vardesc) %>%
    gsub("…", "", .) %>%
    gsub("Select all that apply", "SATA", .)

vardesctab <- cbind(item = names(vardesc), description = vardesc)
# write.table(vardesctab, "data/surveyvariables.txt", row.names = FALSE)

codebook <- lapply(cleanChunks, function(x) {
    x <- x[!(grepl("I would say I", x, fixed = TRUE) |
        grepl("I have...", x, fixed = TRUE))]
    cleanBlock(x)
})

codebook <- Map(function(x, y) { x[["subset"]] <-  rep(y, nrow(x))
    return(x) }, x = codebook, y = sexCond)

Q122 <- lapply(YesNoResponse["Q122"], function(x) {
    x <- gsub("[A-Z]\\. ", "", x)
    cleanBlock(x)
})
Q122$Q122[["subset"]] <- "none"

Q3.5 <- lapply(YesNoResponse["Q3.5"], function(x) {
    responses <- grepl("^Yes|^No", x)
    x <- x[!responses]
    x <- gsub("[A-Z]\\. ", "", x)
    cleanBlock(x)
})
Q3.5 <- adjustVarVal(Q3.5, "Q3.5")
Q3.5$Q3.5[["subset"]] <- "none"

## Adjust for inconsistent names
codebook$Q3.17a$variable <- paste0(gsub("a", "", codebook$Q3.17a$variable),
    "_", codebook$Q3.17a$value)
codebook$Q3.17a$value <- rep(1L, length(codebook$Q3.17a$value))

## Make adjustments to odd variables
codebook <- adjustVarVal(codebook, c("Q1.7", "Q1.8", "Q2.2", "Q2.7",
    "Q3.32", "Q3.33", "Q3.4", "Q3.17", "Q3.18", "Q3.27", "Q3.28", "Q3.29",
    "Q3.30"))

codebook <- c(codebook, Q122, Q3.5)

## PregFeel recode
codebook$Q3.2[4, "response"] <- "don't want me/partner pregnant"
codebook$Q3.3[4, "response"] <- "don't want me/partner pregnant"

## Education recode
codebook$Q1.10$response <- c("LT/some HS", "LT/some HS", "HS diploma/GED",
    "Some college", "College degree/Some Grad", "College degree/Some Grad",
    "Grad degree")

## Relationship recode
codebook$Q1.11$response <- c("single", rep("married/living/commit", 3),
    "div/sep/wid", "other")

## Rename variables in recoding scheme
codebook$Q3.12$variable <- "Q3.12_1"
codebook$Q3.13$variable <- "Q3.13_1"
codebook$Q2.12$variable <- "Q2.12_1"
codebook$Q2.13$variable <- "Q2.13_1"
codebook$Q3.17a$variable <- "Q3.17a_1"
codebook$Q3.17a$value <- 1:4
codebook$Q3.18a$variable <- "Q3.18a_1"

codebook$Q3.23$variable <- with(codebook$Q3.23,
    paste0(variable, "_", "x",value))
codebook$Q3.23 <- codebook$Q3.23[codebook$Q3.23$variable != "Q3.23_x11", ]
codebook$Q3.23$value <- 1L

## Current situation recode
sitRecode <- c("you/partner is pregnant",
    "would like you/partner to become pregnant soon",
    "don't want you/partner to become pregnant soon",
    "aren't trying, but would feel okay if you/partner became pregnant",
    "you/partner can't get pregnant",
    "other")

codebook$Q3.25$response <- sitRecode
codebook$Q3.26$response <- sitRecode

## Recode values 2 to No
for (var in Q3.5[[1L]][["variable"]]) {
    pregint[[var]] <- plyr::mapvalues(pregint[[var]], 2, "No")
}

## Manually recode values due to bad questionnaire
pregint$Q3.3 <- plyr::mapvalues(pregint$Q3.3, c(6,7), c(4, 5))
codebook$Q3.3$value <- plyr::mapvalues(codebook$Q3.3$value, c(6,7), c(4, 5))

## More manual recoding of 4s to 3s
pregint$Q1.9d <- plyr::mapvalues(pregint$Q1.9d, 4, 3)
codebook$Q1.9d$value <- plyr::mapvalues(codebook$Q1.9d$value, 4, 3)

## Variables with subset values either "male" or "female"
mfcodeframes <- codebook[vapply(codebook,
    function(x) all(x$subset %in% c("male", "female")), logical(1L))]
mfcodes <- names(mfcodeframes)

## Include a corresponds variable to relate sex-specific questions
codebook$Q1.9a[["corresponds"]] <- "Q1.9c"
codebook$Q1.9c[["corresponds"]] <- "Q1.9a"
codebook$Q1.9b[["corresponds"]] <- "Q1.9d"
codebook$Q1.9d[["corresponds"]] <- "Q1.9b"
codebook$Q2.2[["corresponds"]] <- codebook[["Q2.7"]]$variable
codebook$Q2.7[["corresponds"]] <- codebook[["Q2.2"]]$variable
codebook$Q2.12[["corresponds"]] <- codebook[["Q2.13"]]$variable
codebook$Q2.13[["corresponds"]] <- codebook[["Q3.12"]]$variable
codebook$Q3.2[["corresponds"]] <- "Q3.3"
codebook$Q3.3[["corresponds"]] <- "Q3.2"
codebook$Q3.12[["corresponds"]] <- codebook[["Q3.13"]]$variable
codebook$Q3.13[["corresponds"]] <- codebook[["Q3.12"]]$variable
codebook$Q3.15[["corresponds"]] <- "Q3.16"
codebook$Q3.16[["corresponds"]] <- "Q3.15"
codebook$Q3.19[["corresponds"]] <- "Q3.20"
codebook$Q3.20[["corresponds"]] <- "Q3.19"
codebook$Q3.25[["corresponds"]] <- "Q3.26"
codebook$Q3.26[["corresponds"]] <- "Q3.25"
codebook$Q3.32[["corresponds"]] <- codebook[["Q3.33"]]$variable
codebook$Q3.33[["corresponds"]] <- codebook[["Q3.32"]]$variable
codebook$Q3.34[["corresponds"]] <- "Q121"
codebook$Q121[["corresponds"]] <- "Q3.34"

codebook <- lapply(codebook, function(x) readr::type_convert(x))

cindx <- seq_along(codebook)
names(cindx) <- names(codebook)

survq <- lapply(cindx, function(i, chunks) {
    addQText(i, chunks, vardesctab)
}, chunks = codebook)

codebook <- mapply(function(x, y) {
    cbind.data.frame(x, question = y)
}, x = codebook, y = survq, SIMPLIFY = FALSE)


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

dupNames <- readLines("docs/duplicateVariables.txt")

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

# Write codebook
readr::write_csv(codebookSheet, "docs/codebookCode.csv")

## Clean variables except the needed one
rm(list = ls()[!ls() %in% c("codebook", "pregint", "codebooktext",
    "codebookSheet", "recodeFactors", "adjustVarVal")])
