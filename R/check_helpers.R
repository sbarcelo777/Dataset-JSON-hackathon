# # Load all JSONs once
# parsed_jsons <- lapply(adam(), function(x) {
#   content <- tryCatch(fromJSON(x$path, simplifyVector = FALSE), error = function(e) NULL)
#   list(path = x$path, content = content)
# })


# # Grab all dataset names (e.g., "ADAE", "ADSL", etc.)
# all_dataset_names <- vapply(parsed_jsons, function(x) {
#   if (!is.null(x$content)) x$content$name else NA_character_
# }, character(1))
# 
# all_dataset_names <- na.omit(unique(all_dataset_names))

#Resolve applicable
resolve_applicable <- function(applicable, all_names) {
  if (is.null(applicable)) return(all_names)
  exclusions <- grep("^\\\\", applicable, value = TRUE)
  inclusions <- setdiff(applicable, exclusions)
  
  if (length(exclusions)) {
    excluded_names <- sub("^\\\\", "", exclusions)
    return(setdiff(all_names, excluded_names))
  } else {
    return(inclusions)
  }
}

