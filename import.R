

#install.packages("jsonlite")
library(jsonlite)
#install.packages("readr")
library(readr)
#install.packages("dplyr")
library(dplyr)

json_str <- readr::read_file("Data/SDTM/dm.json")

data <- jsonlite::fromJSON(json_str, simplifyVector = F)

cols <- c(sapply(data$columns, function(x) x$name))

dm <- data$rows %>% 
  lapply(function(row) {
    data.frame(matrix(unlist(row), nrow = 1), stringsAsFactors = FALSE)
  }) %>%
  dplyr::bind_rows()

names(dm) <- cols

json_str <- readr::read_file("Data/SDTM/ae.json")

data <- jsonlite::fromJSON(json_str, simplifyVector = F)

cols <- c(sapply(data$columns, function(x) x$name))

ae <- data$rows %>% 
  lapply(function(row) {
    data.frame(matrix(unlist(row), nrow = 1), stringsAsFactors = FALSE)
  }) %>%
  dplyr::bind_rows()

names(ae) <- cols