##Let's expand.
##Baseball-reference is the next source.
##The goal here is to pull every recorded hit resulting in an RBI.
required_packages <- c("tidyverse", "devtools", "RSQLite", "DBI", "lubridate", "paws")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
invisible(lapply(required_packages, library, character.only = TRUE))
devtools::install_github("BillPetti/baseballr", ref = "development_branch")
library("baseballr")

s3 <- paws::s3()

am <- get_chadwick_lu()
am$names <- paste(am$name_last, am$name_first, sep = ", ")
am = subset(am, select = c(names, key_mlbam))
am$key_mlbam <- as.integer(am$key_mlbam)
am <- am |> filter(!is.na(key_mlbam))
