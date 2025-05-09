# 1. filter_module.R
filterRUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    # theme = bs_theme(
    #   version = 5,
      # primary = "#2C3E50"  # Une couleur bleu foncé élégante
    # ),
    # height = 350,
    # card_header("Filter Options"),
    br(),
    searchInput(
      inputId = ns("r_filter"),
      label = "Filter data using R expressions",
      placeholder = "e.g., AGE > 30 & STATUS == 'Active'",
      btnSearch = icon("search"),
      btnReset = icon("remove"),
      width = "100%"
    ),
    uiOutput(ns("nbobs")),
    uiOutput(ns("active_filters"))
  )
}

filterRServer <- function(id, get_current_dataset) {
  moduleServer(id, function(input, output, session) {
    r_filters <- reactiveVal(list())
    filtered_data <- reactiveVal(NULL)
    
    
    observeEvent(input$r_filter_search, {
      req(input$r_filter)
      if (nchar(glue::trim(input$r_filter)) > 0) {
        current_filters <- r_filters()
        filter_id <- paste0("filter_", format(Sys.time(), "%Y%m%d%H%M%OS6"))
        current_filters[[filter_id]] <- input$r_filter
        r_filters(current_filters)
        updateSearchInput(session, "r_filter", value = "")
      }
    })
    
    
    output$nbobs <- renderUI({
      
      tagList(
        tags$em(
          paste0("You currently have ", nrow(filtered_data()) ," out of ", nrow(get_current_dataset), " observations")
        )
      )
    })
    
    # Render active filters
    output$active_filters <- renderUI({
      current_filters <- r_filters()
      if (length(current_filters) == 0) {
        return(NULL)
      }
      
      
      page_fluid(
        class = "mt-3",
        lapply(names(current_filters), function(filter_id) {
          div(
            class = "d-flex justify-content-between align-items-center mb-2 p-2 border rounded",
            # style = "background-color: #f8f9fa;",
            span(current_filters[[filter_id]]),
            actionButton(
              inputId = session$ns(paste0("remove_", filter_id)),
              label = NULL,
              icon = icon("times"),
              class = "btn-sm btn-primary"
            )
          )
        })
      )
    })
    
    # Handle filter removal
    observe({
      current_filters <- r_filters()
      for(filter_id in names(current_filters)) {
        local({
          local_filter_id <- filter_id
          observeEvent(input[[paste0("remove_", local_filter_id)]], {
            current_filters <- r_filters()
            current_filters[[local_filter_id]] <- NULL
            r_filters(current_filters)
          }, ignoreInit = TRUE)
        })
      }
    })
    
    # Apply filters and update filtered data
    filtered_result <- reactive({
      req(get_current_dataset)
      result <- get_current_dataset

      current_filters <- r_filters()
      invalid_filters <- c()

      for(filter_id in names(current_filters)) {
        filter_expr <- current_filters[[filter_id]]

        tryCatch({
          result <- result %>%
            filter(eval(parse(text = filter_expr)))
        }, error = function(e) {
          invalid_filters <- c(invalid_filters, filter_id)

          # showNotification(
          #   paste("Error in filter expression:", e$message),
          #   type = "error"
          # )


        })
      }
      
      if(length(invalid_filters) > 0) {
        current_filters <- current_filters[!names(current_filters) %in% invalid_filters]
        r_filters(current_filters)
      }
      
      result
    })
    
    # Reset filters when dataset changes
    observeEvent(get_current_dataset, {
      r_filters(list())
      filtered_data(get_current_dataset)
    })
    
    # Update filtered data
    observe({
      filtered_data(filtered_result())
    })
    
    
    return(list(
      filtered_data = filtered_data,
      r_filters = r_filters,
      nrows = reactive(nrow(get_current_dataset))
    ))
  })
}