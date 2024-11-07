

#install.packages("jsonlite")
library(jsonlite)
# install.packages("readr")
library(readr)
#install.packages("dplyr")
library(dplyr)

json_str <- readr::read_file("Data/SDTM/dm.json")

data <- jsonlite::fromJSON(json_str, simplifyVector = F)

cols <- c(sapply(data$columns, function(x) x$label))

dm <- data$rows %>% 
  lapply(function(row) {
    data.frame(matrix(unlist(row), nrow = 1), stringsAsFactors = FALSE)
  }) %>%
  dplyr::bind_rows()

names(dm) <- cols





# Read and parse JSON
json_str <- readr::read_file("Data/SDTM/ae.json")
data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

# Extract column names and data types
cols <- c(sapply(data$columns, function(x) x$label))
datatypes <- c(sapply(data$columns, function(x) x$dataType))

# Convert rows to a data frame
ae <- data$rows %>%
  lapply(function(row) {
    data.frame(matrix(unlist(row), nrow = 1), stringsAsFactors = FALSE)
  }) %>%
  dplyr::bind_rows()

# Apply each data type to the respective column using mapply
ae[] <- mapply(function(column, dtype) {
  switch(
    dtype,
    "character" = as.character(column),
    "numeric" = as.numeric(column),
    "integer" = as.integer(column),
    "logical" = as.logical(column),
    column  # Default if no matching type is found
  )
}, ae, datatypes, SIMPLIFY = FALSE)

# Set column names in the data frame
names(ae) <- cols

