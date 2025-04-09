flatten_list <- function(lst, parent_key = "") {
  result <- list()
  
  for (key in names(lst)) {
    new_key <- ifelse(parent_key == "", key, paste(parent_key, key, sep = "_"))
    
    if (is.list(lst[[key]])) {
      # Recursively flatten nested lists
      nested <- flatten_list(lst[[key]], new_key)
      result <- c(result, nested)
    } else {
      # Store non-list values directly
      result[[new_key]] <- lst[[key]]
    }
  }
  
  return(result)
}

list_to_transposed_df <- function(lst) {
  flat_list <- flatten_list(lst)  # Flatten nested lists
  df <- tibble(
    Key = names(flat_list),
    Value = unlist(flat_list, use.names = FALSE)
  )
  return(df)
}

# Function to remove specific elements from a list
remove_specific_elements <- function(x) {
  
  if (is.list(x)) {
    # Remove specific elements if they exist
    x$elements <- NULL
    x$column <- NULL
    x$columns <- NULL  # including 'columns' in case it's plural
    x$row <- NULL
    x$rows <- NULL    # including 'rows' in case it's plural
    x$path <- NULL
  }
  
  return(x)
}