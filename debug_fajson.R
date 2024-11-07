json_str <- readr::read_file("Data/SDTM/ie.json")
data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

# Extract column names and data types
cols <- c(sapply(data$columns, function(x) x$name))
# Extract labels only if they are not NULL
labels <- c(sapply(data$columns, function(x) if (!is.null(x)) x$label))

datatypes <- c(sapply(data$columns, function(x) x$dataType))

# Convert rows to a data frame
ae <- data$rows %>%
  lapply(function(row) {
    data.frame(matrix(unlist(row), nrow = 1), stringsAsFactors = FALSE)
  }) %>%
  dplyr::bind_rows()

# for (i in 1:length(labels)){
#   if (is.null(data[["rows"]][[1]][[i]])){
#     #remove data[["rows"]][[1]][[i]]
#   }
#   
# }
# for (j in 1:nrow(ae)){
#   for (j in 1:length(labels)){
# # Remove all NULL elements from data[["rows"]][[1]] at once
# data[["rows"]][[j]][[i]] <- Filter(Negate(is.null), data[["rows"]][[j]][[i]])
#   }
# }

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
attr(ae, "labels") <- setNames(labels, cols)  # Store labels as attribute


