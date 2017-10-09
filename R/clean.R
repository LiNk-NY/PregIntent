library(readxl)

vars <- read_excel("../data/surveyresults_2099.xlsx", n_max = 1)

namesVec <- unlist(vars[1, ])

# Varnames and questions
codebook <- cbind(varname = names(namesVec), question = namesVec)

pregint <- read_excel("../data/surveyresults_2099.xlsx", skip = 2, col_names = FALSE)

