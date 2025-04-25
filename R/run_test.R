run_test <- function(compliance = compliance, 
                     type_check = c("ADaM", "ADAM"), 
                     check_info_data = adam_check_info, 
                     input_data = adam, 
                     adsl_content = adsl_content, 
                     ts_content = ts_content,
                     progress = progress){
  
  checks <- compliance %>%
    filter(Type %in% type_check) %>%
    filter(Check_Id %in% names(check_info_data)) %>%
    select(c("Check_Id", "Check_Description", "CHeckMessage"))
  
  parsed_jsons <- lapply(input_data, function(x) {
    content <- tryCatch(fromJSON(x$path, simplifyVector = FALSE), error = function(e) NULL)
    list(path = x$path, content = content)
  })
  
  # Grab all dataset names (e.g., "ADAE", "ADSL", etc.)
  all_dataset_names <- vapply(parsed_jsons, function(x) {
    if (!is.null(x$content)) x$content$name else NA_character_
  }, character(1))
  
  all_dataset_names <- na.omit(unique(all_dataset_names))
  
  # Initialize progress at 0.1
  progress$set(value = 0.1, detail = "Starting checks...")
  
  # Calculate increment per dataset
  total_datasets <- length(all_dataset_names) * length(checks$Check_Id)
  if (total_datasets > 0) {
    # Progress from 0.1 to 0.5, so 0.4 total range
    progress_increment <- 0.4 / total_datasets
  } else {
    progress_increment <- 0
  }
  
  # Track current progress value
  current_progress <- 0.1
  
  check_results <- lapply(checks$Check_Id, function(check_id) {
    check_info <- check_info_data[[check_id]]
    if (is.null(check_info)) return(list(Result = FALSE, Failures = list()))  # Return FALSE instead of NA
    
    check_fun <- check_info$fun
    applicable_to <- resolve_applicable(check_info$applicable_to, all_dataset_names)
    
    if (!any(applicable_to %in% all_dataset_names)) {
      failures <- setNames(as.list(paste0("Dataset not found: ", applicable_to)), applicable_to)
      return(list(Result = FALSE, Failures = failures))
    }
    
    failures <- list()
    
    # Update progress to show which check we're on
    progress$set(detail = paste0("Running check: ", check_id))
    
    for (entry in parsed_jsons) {
      content <- entry$content
      path <- entry$path
      if (is.null(content)) next
      
      dataset_name <- content$name
      if (!(dataset_name %in% applicable_to)) next
      
      # Update progress detail to show which dataset we're checking
      progress$set(message = paste0("Check ", check_id), detail =  paste0(" on ", dataset_name))
      
      if(check_id %in% "ADAM001") {
        result <- check_fun(entry, adsl_content)
      }else if(check_id %in% "SDTM002") {
        result <- check_fun(entry, ts_content)
      }else{
        result <- check_fun(entry)
      }
      
      if (is.list(result)) {
        if (!result$pass) {
          failures[[dataset_name]] <- result$message
        }
      } else if (!isTRUE(result)) {
        failures[[dataset_name]] <- "Failed without detailed reason"
      }
      
      # Increment progress after each dataset check
      current_progress <- min(0.5, current_progress + progress_increment)
      progress$set(value = current_progress)
    }
    
    list(
      Result = length(failures) == 0,
      Failures = failures
    )
  })
  
  # Ensure we reach exactly 0.5 at the end of this function
  progress$set(value = 0.5, detail = "Checks complete, processing results...")
  
  # adam_checks <- checks
  checks$Result <- vapply(check_results, function(x) x$Result, logical(1))
  checks$Failed_Datasets <- I(lapply(check_results, function(x) x$Failures))
  
  return(checks)
}
