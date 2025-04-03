
#' Process JSON data into a data.table or SQLite database
#'
#' @param json_data List containing JSON data with 'columns' and 'rows'
#' @param use_db Logical, whether to store data in SQLite database
#' @param db_path Character, path to SQLite database file
#' @return data.table or list with metadata if use_db=TRUE
#' @import data.table
#' @import DBI
#' @import RSQLite
#' 
process_ndjson_file <- function(json_data,
                              # use_db = FALSE, 
                              db_path = "json_cache.db") {
  library(data.table)
  library(DBI)
  library(RSQLite)

  
  tryCatch({
  #   # Input validation
  #   if (!is.list(json_data) || !all(c("columns") %in% names(json_data))) {
  #     stop("Invalid JSON data format. Must contain 'columns' and 'rows'")
  #   }
  #   
    # Extract metadata with error checking
    cols <- json_data$columns$name
    
    labels <- json_data$columns$label
    
    datatypes <- json_data$columns$dataType
    
    
    
    lines <- readLines(json_data$datapath, encoding = "UTF-8")
    
    
    # Extract data (remaining lines are JSON arrays)
    dt <- lapply(lines[-1], fromJSON)
    dt <- as.data.frame(do.call(rbind, dt), stringsAsFactors = FALSE)
    
    
    # Check for columns to remove
    # if (ncol(dt) < length(labels)) {
    #   null_indices <- c()
    #   for(i in seq_along(labels)) {
    #     if (is.null(json_data[["rows"]][[1]][[i]])) {
    #       null_indices <- c(null_indices, i)
    #     }
    #   }
    #   
    #   if (length(null_indices) > 0) {
    #     message(sprintf("Removing %d columns with NULL values", length(null_indices)))
    #     labels <- labels[-null_indices]
    #     cols <- cols[-null_indices]
    #     datatypes <- datatypes[-null_indices]
    #   }
    # }
    
    # Set column names
    data.table::setnames(dt, cols)
    
    # Apply data types with error handling
    message("Applying data types...")
    for (j in seq_along(cols)) {
      col <- cols[j]
      dtype <- datatypes[j]
      
      
      tryCatch({
        data.table::set(dt, j = col, value = switch(dtype,
                                                    "character" = as.character(dt[[col]]),
                                                    "numeric" = as.numeric(dt[[col]]),
                                                    "integer" = as.integer(dt[[col]]),
                                                    "logical" = as.logical(dt[[col]]),
                                                    "string" = as.character(dt[[col]]),
                                                    # "date" = as.Date(dt[[col]]),
                                                    # "datetime" = as.POSIXct(dt[[col]]),
                                                    dt[[col]]
        ))
      }, error = function(e) {
        warning(sprintf("Error converting column %s to type %s: %s", col, dtype, e$message))
      })
    }
    
    # Set attributes for metadata
    setattr(dt, "labels", setNames(labels, cols))
    setattr(dt, "datatypes", setNames(datatypes, cols))
    
    object_size <- object.size(json_data)
    # print(object_size)
    KB50000 <- 51200000
    
    # Database handling
    if (object_size > KB50000) {
      message("Storing data in SQLite database...")
      tryCatch({
        con <- dbConnect(SQLite(), db_path)
        on.exit(dbDisconnect(con))
        
        # Create table with proper column types
        column_types <- sapply(datatypes, function(dtype) {
          switch(dtype,
                 "character" = "TEXT",
                 "numeric" = "REAL",
                 "integer" = "INTEGER",
                 "logical" = "INTEGER",
                 "date" = "TEXT",
                 "datetime" = "TEXT",
                 "TEXT"
          )
        })
        
        # Create table definition
        create_table_sql <- paste0(
          "CREATE TABLE IF NOT EXISTS json_data (",
          paste(mapply(function(col, type) sprintf("%s %s", col, type),
                       cols, column_types),
                collapse = ", "
          ),
          ")"
        )
        
        dbExecute(con, "DROP TABLE IF EXISTS json_data")
        dbExecute(con, create_table_sql)
        
        # Write data in chunks for better memory management
        chunk_size <- 1000
        total_rows <- nrow(dt)
        for(i in seq(1, total_rows, chunk_size)) {
          end_idx <- min(i + chunk_size - 1, total_rows)
          chunk <- dt[i:end_idx]
          dbWriteTable(con, "json_data", chunk, append = TRUE)
        }
        
        # Return metadata
        # return(list(
        #   cols = cols,
        #   labels = labels,
        #   datatypes = datatypes,
        #   db_path = db_path,
        #   row_count = total_rows
        # ))
        return(dt)
      }, error = function(e) {
        stop("Database operation failed: ", e$message)
      })
    }
    
    # Return data.table if not using database
    return(dt)
    
  }, error = function(e) {
    stop("JSON processing failed: ", e$message)
  })
}

# Helper function to retrieve data from database
read_json_db <- function(db_path, query = NULL) {
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    
    if (is.null(query)) {
      query <- "SELECT * FROM json_data"
    }
    
    data.table::setDT(dbGetQuery(con, query))
  }, error = function(e) {
    stop("Failed to read from database: ", e$message)
  })
}