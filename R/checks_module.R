checksUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    navset_underline(
      nav_panel(
        "ADaM",
        # verbatimTextOutput(ns("debug_adam")), # Debug output
        uiOutput(ns("statusCardADaM")),
        reactableOutput(ns("adam_checks"))
      ),
      nav_panel(
        "SDTM",
        verbatimTextOutput(ns("debug_sdtm")), # Debug output
        uiOutput(ns("sdtm_list"))
      )
    )
  )
}

checksServer <- function(id, uploaded_files) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    url <- "https://cyt-stg-rsconnect.cytel.com/cdisc-compliance-api/cdisc_compliance"
    apiKey <- "eBalEcdbia0bkHgtfBZ7nMsMbAEOTtCw"
    response <- httr::GET(url,
                          httr::add_headers(Authorization = paste("Key", apiKey)))
    
    # Check if the request was successful
    if (httr::status_code(response) == 200) {
      # Read and print the response content
      compliance <- httr::content(response, as = "text", encoding = "UTF-8")
    } else {
      stop(paste("Error:", httr::status_code(response)))
    }
    
    compliance <- tryCatch(
      jsonlite::fromJSON(compliance, simplifyVector = F),
      error = function(e) NULL
    )
    compliance <- dplyr::bind_rows(compliance)
    
    adam_check_info <- list(
      ADAM001 = list(fun = ADAM001, applicable_to = c("\\ADSL")),  # applies to all except ADSL
      ADAM004 = list(fun = ADAM004, applicable_to = c("\\ADSL")),  # applies to all except ADSL
      ADAM006 = list(fun = ADAM006, applicable_to = NULL),  # applies to all 
      ADAM014 = list(fun = ADAM014, applicable_to = NULL),         # applies to all
      ADAM015 = list(fun = ADAM015, applicable_to = NULL)  # applies to all
    )
    
    adam <- reactive({
      req(uploaded_files())
      matches <- uploaded_files() %>%
        purrr::keep(~ !is.null(.x$originator) && any(grepl("ADaM", .x$originator)))
      
      if (length(matches) == 0) {
        return(NULL)
      } else {
        return(matches)
      }
    })
    
    sdtm <- reactive({
      req(uploaded_files())
      matches <- uploaded_files() %>%
        purrr::keep(~ !is.null(.x$originator) && any(grepl("SDTM", .x$originator)))
      
      if (length(matches) == 0) {
        return(NULL)
      } else {
        return(matches)
      }
    })
    
    adsl_content <- reactive({
      req(uploaded_files())
      
      # Find ADSL file from uploaded files
      adsl <- uploaded_files() %>%
        purrr::keep(~ .x$name == "ADSL") %>%
        purrr::pluck(1)
      
      if (length(adsl) == 0) {
        # Log and inform that ADSL is not available
        message("ADSL NOT available for ADAM001...")
        return(NULL)
      } else {
        # Try to parse the JSON content with error handling
        tryCatch({
          # content <- jsonlite::fromJSON(adsl[[1]]$path, simplifyVector = FALSE)
          adsl <- as.data.frame(process_json_file(adsl))
          
          # Log success
          message("ADSL available for ADAM001...")
          return(adsl)
        }, error = function(e) {
          message(paste("Error processing ADSL file:", e$message))
          return(NULL)
        })
      }
    })
    
    
    # Create a reactiveVal to store the processed ADaM checks
    adam_checks_data <- reactiveVal(NULL)
    
    # Process ADaM checks when adam() changes
    observe({
      req(adam())
      
      adsl_content <- adsl_content()
      
      adam_checks <- compliance %>%
        filter(Type %in% c("ADaM", "ADAM")) %>%
        filter(Check_Id %in% names(adam_check_info)) %>%
        select(c("Check_Id", "Check_Description", "CHeckMessage"))
      
      parsed_jsons <- lapply(adam(), function(x) {
        content <- tryCatch(fromJSON(x$path, simplifyVector = FALSE), error = function(e) NULL)
        list(path = x$path, content = content)
      })
      
      # Grab all dataset names (e.g., "ADAE", "ADSL", etc.)
      all_dataset_names <- vapply(parsed_jsons, function(x) {
        if (!is.null(x$content)) x$content$name else NA_character_
      }, character(1))
      
      all_dataset_names <- na.omit(unique(all_dataset_names))
      
      check_results <- lapply(adam_checks$Check_Id, function(check_id) {
        check_info <- adam_check_info[[check_id]]
        if (is.null(check_info)) return(list(Result = FALSE, Failures = list()))  # Return FALSE instead of NA
        
        check_fun <- check_info$fun
        applicable_to <- resolve_applicable(check_info$applicable_to, all_dataset_names)
        
        failures <- list()
        
        for (entry in parsed_jsons) {
          content <- entry$content
          path <- entry$path
          if (is.null(content)) next
          
          dataset_name <- content$name
          if (!(dataset_name %in% applicable_to)) next
          
          if(check_id %in% "ADAM001") result <- check_fun(entry, adsl_content) 
          if(!check_id %in% "ADAM001") result <- check_fun(entry)
          
          if (is.list(result)) {
            if (!result$pass) {
              failures[[dataset_name]] <- result$message
            }
          } else if (!isTRUE(result)) {
            failures[[dataset_name]] <- "Failed without detailed reason"
          }
        }
        
        list(
          Result = length(failures) == 0,
          Failures = failures
        )
      })
      
      adam_checks$Result <- vapply(check_results, function(x) x$Result, logical(1))
      adam_checks$Failed_Datasets <- I(lapply(check_results, function(x) x$Failures))  # store as list-column
      adam_checks <- adam_checks %>% select(Result, everything())
      
      # Store the processed results
      adam_checks_data(adam_checks)
    })
    
    # Create the status card using the reactive data
    output$statusCardADaM <- renderUI({
      req(adam_checks_data())
      
      results_data <- adam_checks_data()
      total_checks <- nrow(results_data)
      passed_checks <- sum(results_data$Result)
      failed_checks <- total_checks - passed_checks
      
      if (failed_checks == 0) {
        card(
          class = "mt-3 mb-3",
          card_header(strong("Check Status")),
          card_body(
            class = "bg-success-subtle",
            div(
              class = "d-flex align-items-center",
              span(fa_i("circle-check", fill = "green", height = "2em", width = "2em"), class = "me-3"),
              div(
                h4("All Checks Passed"),
                p(sprintf("All %d checks have passed successfully.", total_checks))
              )
            )
          )
        )
      } else {
        card(
          class = "mt-3 mb-3",
          card_header(strong("Check Status")),
          card_body(
            class = "bg-warning-subtle",
            div(
              class = "d-flex align-items-center",
              span(fa_i("triangle-exclamation", fill = "orange", height = "2em", width = "2em"), class = "me-3"),
              div(
                h4("Some Checks Failed"),
                p(sprintf("%d out of %d checks failed. Please review the details below.", failed_checks, total_checks))
              )
            )
          )
        )
      }
    })
    
    # Render the adam data table using the reactive data
    output$adam_checks <- renderReactable({
      req(adam_checks_data())
      
      results_data <- adam_checks_data()
      total_checks <- nrow(results_data)
      passed_checks <- sum(results_data$Result)
      
      reactable::reactable(results_data, 
                           columns = list(
                             Result = colDef(name = "",
                                             width = 50,
                                             cell = function(value) {
                                               if (value) {
                                                 tagList(
                                                   span(
                                                     fa_i("circle-check", fill = "green")
                                                   )
                                                 )
                                               } else {
                                                 tagList(
                                                   span(
                                                     fa_i("circle-xmark", fill = "red")
                                                   )
                                                 )
                                               }
                                             }),
                             
                             Check_Id = colDef(name = "", 
                                               width = 150),
                             
                             Check_Description = colDef(name = "Description",
                                                        width = 400),
                             
                             CHeckMessage = colDef(name = "Details"),
                             
                             Failed_Datasets = colDef(show = FALSE)
                           ),
                           
                           highlight = TRUE,
                           outlined = FALSE,
                           bordered = FALSE,
                           borderless = FALSE,
                           striped = TRUE,
                           compact = TRUE,
                           resizable = TRUE,
                           
                           details = function(index) {
                             fails <- results_data$Failed_Datasets[[index]]
                             if (length(fails) == 0) return(NULL)
                             
                             # Create a data frame from the list of failures
                             detail_df <- data.frame(
                               Dataset = names(fails),
                               Issue = unlist(fails),
                               stringsAsFactors = FALSE
                             )
                             
                             # Return a nested reactable with the details
                             reactable(
                               detail_df,
                               columns = list(
                                 Dataset = colDef(name = "Dataset", minWidth = 150),
                                 Issue = colDef(name = "Issue Description", minWidth = 300)
                               ),
                               fullWidth = TRUE,
                               borderless = TRUE,
                               outlined = TRUE,
                               pagination = FALSE,
                               minRows = 1
                             )
                           },
                           
                           rowStyle = JS("
                       function(rowInfo, state) {
                         if (rowInfo) {
                           if (rowInfo.row.Result === true) {
                             return { backgroundColor: '#e6ffe6' };  // light green
                           } else {
                             return { backgroundColor: '#ffe6e6' };  // light red
                           }
                         }
                         return {};
                       }
                     ")
      )
    })
    
    output$sdtm_list <- renderUI({
      req(sdtm())
      
      names <- t(sapply(sdtm(), function(x) c(name = x$name)))
      labels <- t(sapply(sdtm(), function(x) c(name = x$label)))
      # The correct way to display the single adam() object
      tagList(
        h4(HTML(paste("File Names:", paste(names, collapse = "<br>")))),
        # Display data if it exists
        tableOutput(ns("sdtm_checks"))
      )
    })
    
    # Render the sdtm data table if it exists
    output$sdtm_checks <- renderTable({
      req(sdtm())
    })
    
    # Return the reactiveVal
    
  })
}