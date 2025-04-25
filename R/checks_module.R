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
        uiOutput(ns("statusCardSDTM")),
        reactableOutput(ns("sdtm_checks"))
      )
    )
  )
}

checksServer <- function(id, uploaded_files, progress) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    is_complete <- reactiveVal(FALSE)
    
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
      ADAM004 = list(fun = ADAM004, applicable_to = NULL),         # applies to all 
      ADAM006 = list(fun = ADAM006, applicable_to = NULL),         # applies to all 
      ADAM014 = list(fun = ADAM014, applicable_to = NULL),         # applies to all
      ADAM015 = list(fun = ADAM015, applicable_to = NULL)          # applies to all
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
    
    sdtm_check_info <- list(
      SDTM002 = list(fun = SDTM002, applicable_to = "DM")  # applies to DM
      # ADAM004 = list(fun = ADAM004, applicable_to = NULL),  # applies to all except ADSL
      # ADAM006 = list(fun = ADAM006, applicable_to = NULL),  # applies to all 
      # ADAM014 = list(fun = ADAM014, applicable_to = NULL),  # applies to all
      # ADAM015 = list(fun = ADAM015, applicable_to = NULL)   # applies to all
    )
    
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
    
    
    ts_content <- reactive({
      req(uploaded_files())
      
      # Find ADSL file from uploaded files
      ts <- uploaded_files() %>%
        purrr::keep(~ .x$name == "TS") %>%
        purrr::pluck(1)
      
      if (length(ts) == 0) {
        # Log and inform that ADSL is not available
        message("TS NOT available...")
        return(NULL)
      } else {
        # Try to parse the JSON content with error handling
        tryCatch({
          # content <- jsonlite::fromJSON(adsl[[1]]$path, simplifyVector = FALSE)
          ts <- as.data.frame(process_json_file(ts))
          
          # Log success
          message("TS available...")
          return(ts)
        }, error = function(e) {
          message(paste("Error processing TS file:", e$message))
          return(NULL)
        })
      }
    })
    
    # Create a reactiveVal to store the processed ADaM checks
    adam_checks_data <- reactiveVal(NULL)
    sdtm_checks_data <- reactiveVal(NULL)
    
    # Process ADaM checks when adam() changes
    observe({
      # Check if adam() is NULL
      if (is.null(adam())) {
        # Set adam_checks_data to a special value indicating "No Adam"
        adam_checks_data(data.frame(
          Result = logical(0),
          Check_Id = character(0),
          Check_Description = character(0), 
          CHeckMessage = character(0),
          Failed_Datasets = I(list())
        ))
        return()  # Exit the observer
      }

      adsl_content <- adsl_content()
      ts_content <- ts_content()
      
      adam_checks <- run_test(compliance = compliance, 
                              type_check = c("ADaM", "ADAM"), 
                              check_info_data = adam_check_info, 
                              input_data = adam(), 
                              adsl_content = adsl_content, 
                              ts_content = ts_content,
                              progress = progress)
      
      adam_checks <- adam_checks %>% select(Result, everything())
      
      # Store the processed results
      adam_checks_data(adam_checks)
    })
    
    # Process SDTM checks when adam() changes
    observe({
      # Check if sdtm() is NULL
      if (is.null(sdtm())) {
        # Set adam_checks_data to a special value indicating "No Adam"
        sdtm_checks_data(data.frame(
          Result = logical(0),
          Check_Id = character(0),
          Check_Description = character(0), 
          CHeckMessage = character(0),
          Failed_Datasets = I(list())
        ))
        return()  # Exit the observer
      }
      
      adsl_content <- adsl_content()
      ts_content <- ts_content()
      
      sdtm_checks <- run_test(compliance = compliance, 
                              type_check = c("SDTM"), 
                              check_info_data = sdtm_check_info, 
                              input_data = sdtm(), 
                              adsl_content = adsl_content, 
                              ts_content = ts_content,
                              progress = progress)
      
      sdtm_checks <- sdtm_checks %>% select(Result, everything())
      
      # Store the processed results
      sdtm_checks_data(sdtm_checks)
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
      
      if (is.null(adam_checks_data()) || nrow(adam_checks_data()) == 0) {
        return(reactable(
          data.frame(Message = "No ADaM datasets available"),
          pagination = FALSE
        ))
      }
      
      req(adam_checks_data())
      
      results_data <- adam_checks_data()
      total_checks <- nrow(results_data)
      passed_checks <- sum(results_data$Result)
      check_table(results_data)
      

    })
    
  
    output$statusCardSDTM <- renderUI({
      req(sdtm_checks_data())
      
      results_data <- sdtm_checks_data()
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
    
    # Render the sdtm data table if it exists
    output$sdtm_checks <- renderReactable({
      
      if (is.null(sdtm_checks_data()) || nrow(sdtm_checks_data()) == 0) {
        return(reactable(
          data.frame(Message = "No SDTM datasets available"),
          pagination = FALSE
        ))
      }
      
      req(sdtm_checks_data())
      
      results_data <- sdtm_checks_data()
      total_checks <- nrow(results_data)
      passed_checks <- sum(results_data$Result)
      check_table(results_data)
      
      
    })
    
    observe({
      # Check if adam() is not NULL
        req(adam_checks_data())
        req(sdtm_checks_data())
        # Optional: add a small delay to allow rendering to complete
        shinyjs::delay(300, {
          # Set reactive value to indicate completion
          is_complete(TRUE)
        })
    })
    
    # Return the reactiveVal
    return(reactive({ is_complete() }))
  })
}