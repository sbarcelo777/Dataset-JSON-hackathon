library(dplyr)
library(tidyr)
library(jsonlite)
library(reactable)

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

# Function to convert any nested list to a data frame
list_to_dataframe <- function(lst) {
  flat_list <- flatten_list(lst)
  df <- as.data.frame(flat_list, stringsAsFactors = FALSE)
  return(df)
}

# Example usage with your data
data <- fromJSON("view-data/json/sdtm/ae.json", simplifyVector = F)
data$columns <- NULL
data$rows <- NULL
df <- list_to_dataframe(data)

# Display as an interactive table
reactable(df, searchable = TRUE, striped = TRUE, highlight = TRUE, bordered = TRUE)




list_to_transposed_df <- function(lst) {
  flat_list <- flatten_list(lst)  # Flatten nested lists
  df <- tibble(
    Key = names(flat_list),
    Value = unlist(flat_list, use.names = FALSE)
  )
  return(df)
}

# Convert and transpose your data
data <- fromJSON("view-data/json/sdtm/ae.json", simplifyVector = F)
data$columns <- NULL
data$rows <- NULL
df_transposed <- list_to_transposed_df(data)

# Display as an interactive table
reactable(df_transposed, searchable = TRUE, striped = TRUE, highlight = TRUE, bordered = TRUE)






library(httr)

# Define the API URL
url <- "https://library.cdisc.org/api/mdr/products"
url <- "https://library.cdisc.org/core-rules"


# Set up the API key in headers
headers <- add_headers("api-key" = "864708f3a0394acab32252b8a2b7f1bb")

# Make the API request
response <- GET(url, headers)

# Check if the request was successful
if (status_code(response) == 200) {
  # Read and print the response content
  api_content <- content(response, as = "text", encoding = "UTF-8")
} else {
  stop(paste("Error:", status_code(response)))
}

api_content <- tryCatch(
  fromJSON(api_content, simplifyVector = F),
  error = function(e) NULL
)



url <- "https://library.cdisc.org/api/mdr/sdtmig/3-4/datasets"
datasets/DM/variables/ARMNRS

headers <- add_headers("api-key" = "864708f3a0394acab32252b8a2b7f1bb")

# Make the API request
response <- GET(url, headers)

# Check if the request was successful
if (status_code(response) == 200) {
  # Read and print the response content
  sdtmig_api_content <- content(response, as = "text", encoding = "UTF-8")
} else {
  stop(paste("Error:", status_code(response)))
}

sdtmig_api_content <- tryCatch(
  fromJSON(sdtmig_api_content, simplifyVector = F),
  error = function(e) NULL
)

/mdr/sdtmig/md-1-0
