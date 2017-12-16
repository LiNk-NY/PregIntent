## Script to easily install packages
packages <- c("dplyr", "tidyr", "readxl", "maps", "Hmisc", "plyr", "reshape2",
              "forcats", "magrittr", "broom", "readr")

for (pkg in packages) {
    if (!requireNamespace(pkg))
        install.packages(pkg)
    library(pkg, character.only = TRUE)
}
