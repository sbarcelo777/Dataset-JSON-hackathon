# 1. filter_module.R
filterUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    theme = bs_theme(
      version = 5,
      primary = "#2C3E50",  # Une couleur bleu foncé élégante
    ),
    height = 350,
    # card_header("Filter Options"),
    pickerInput(
      ns("file_select"),
      "Select a dataset",
      choices = NULL,
      width = "100%",
      options = pickerOptions(container = "body",
                              liveSearch = TRUE,
                              size = 15)
      ),
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

filterServer <- function(id, uploaded_files, file_format) {
  moduleServer(id, function(input, output, session) {
    r_filters <- reactiveVal(list())
    filtered_data <- reactiveVal(NULL)


    # Get current dataset function
    get_current_dataset <- reactive({
      tryCatch({
        req(input$file_select)
        
        selected_name_label <- strsplit(input$file_select, "-->")[[1]]
        selected_name <- selected_name_label[1]
        selected_label <- selected_name_label[2]
        
        json_data <- uploaded_files() %>%
          purrr::keep(~ .x$name == selected_name 
                      # && .x$label == selected_label
                      ) %>%
          purrr::pluck(1)
        
        if (file_format() == "json") {
          render_df <- as.data.frame(process_json_file(json_data = json_data))
        } else if(file_format() == "ndjson") {
          render_df <- as.data.frame(process_ndjson_file(json_data = json_data))
        } else if(file_format() == "xpt") {
          render_df <- json_data$content
          labels <- labelled::var_label(render_df)
          setattr(render_df, "labels", setNames(labels, names(render_df)))
        }else if(file_format() == "sas7bdat") {
          render_df <- as.data.frame(json_data$content)
          labels <- labelled::var_label(render_df)
          setattr(render_df, "labels", setNames(labels, names(render_df)))
          
        }
        
        return(render_df)
      }, error = function(e) {
        # Handle error by returning NULL
        return(NULL)
      })
    })

    # File selection observer
    observe({
      if (is.null(uploaded_files()) || length(uploaded_files()) == 0) {
        choices <- NULL
      } else {
        names <- t(sapply(uploaded_files(), function(x) c(name = x$name)))
        labels <- t(sapply(uploaded_files(), function(x) 
          c(label = ifelse(is.null(x$label), " ", x$label))
        ))        
        choices <- setNames(paste0(names, "-->", labels), paste0(names, " - ", labels))
        updatePickerInput(session, "file_select",
                          choices = choices,
                          selected = choices[1])
      }

    })

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
          paste0("You currently have ", nrow(filtered_data()) ," out of ", nrow(get_current_dataset()), " observations")
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
      req(get_current_dataset())
      result <- get_current_dataset()

      current_filters <- r_filters()
      invalid_filters <- c()

      for(filter_id in names(current_filters)) {
        filter_expr <- current_filters[[filter_id]]

        tryCatch({
          temp_result <- result %>%
            filter(eval(parse(text = filter_expr)))
          result <- temp_result
        }, error = function(e) {
          invalid_filters <- filter_id

          showNotification(
            paste("Error in filter expression:", e$message),
            type = "error"
          )


        })
      }


      if(length(invalid_filters) > 0) {
        # print(invalid_filters)
        # print("TRUE")
        current_filters <- current_filters[!names(current_filters) %in% invalid_filters]
        current_filters <- setdiff(current_filters, invalid_filters)
        r_filters(current_filters)
      }

      result
    })

    # Reset filters when dataset changes
    observeEvent(input$file_select, {
      r_filters(list())
      filtered_data(get_current_dataset())
    })

    # Update filtered data
    observe({
      filtered_data(filtered_result())
    })

  

    return(list(
      filtered_data = filtered_data,
      r_filters = r_filters,
      selected_file = reactive(input$file_select),
      nrows = reactive(nrow(get_current_dataset()))
    ))
  })
}