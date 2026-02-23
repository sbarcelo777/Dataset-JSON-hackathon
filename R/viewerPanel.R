viewerPanelUI <- function(id) {
  ns <- NS(id)
  
  page_fluid(
    muiDependency(),
    tags$script(jsCode),
    
    # Use layout_sidebar but with responsive options
    layout_sidebar(
      sidebar = sidebar(
        width = "22%", # Use percentage instead of fixed pixels
        position = "left",
        open = TRUE,
        # Add a class for responsive targeting
        class = "sidebar-content",
        navset_underline(
          nav_panel(
            "Data Manipulation",
            filterRUI(ns("filter")),
            varDescriptionUI(ns("desc_stats"))
          )
        )
      ),
      
      # Main content with a class for responsive targeting
      div(
        class = "main-content",
        pickerInput(
          ns("data_select"),
          "Select a dataset",
          choices = NULL,
          # width = "100%",  # Make width 100% to be more responsive
          options = pickerOptions(
            container = "body",
            liveSearch = TRUE,
            size = 15
          )
        ),
        tableUI(ns("settings"))
      )
    ),
  )
}

# 4. Viewer Server 2
viewerPanel <- function(id, upload_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    # File selection observer
    observe({
      if (is.null(upload_data$uploaded_files()) || length(upload_data$uploaded_files()) == 0) {
        choices <- NULL
      } else {
        names <- t(sapply(upload_data$uploaded_files(), function(x) c(name = x$name)))
        labels <- t(sapply(upload_data$uploaded_files(), function(x) 
          c(label = ifelse(is.null(x$label), " ", x$label))
        ))        
        choices <- setNames(paste0(names, "-->", labels), paste0(names, " - ", labels))
        updatePickerInput(session, "data_select",
                          choices = choices,
                          selected = choices[1])
      }
      
    })
    
    # Get current dataset function
    get_current_dataset <- reactive({
      tryCatch({
        req(input$data_select)
        
        selected_name_label <- strsplit(input$data_select, "-->")[[1]]
        selected_name <- selected_name_label[1]
        selected_label <- selected_name_label[2]
        
        json_data <- upload_data$uploaded_files() %>%
          purrr::keep(~ .x$name == selected_name 
                      # && .x$label == selected_label
          ) %>%
          purrr::pluck(1)
        
        if (upload_data$file_format() == "json") {
          render_df <- as.data.frame(process_json_file(json_data = json_data))
        } else if(upload_data$file_format() == "ndjson") {
          render_df <- as.data.frame(process_ndjson_file(json_data = json_data))
        } else if(upload_data$file_format() == "xpt") {
          render_df <- json_data$content
          labels <- labelled::var_label(render_df)
          setattr(render_df, "labels", setNames(labels, names(render_df)))
        }else if(upload_data$file_format() == "sas7bdat") {
          print(json_data$content)
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
    
    observe({
      # print(head(get_current_dataset()))
      
      selected_file <- reactive(input$data_select)
      dataset <- get_current_dataset()
      filter_results <- filterRServer("filter", dataset)
      
      varDescriptionServer("desc_stats",
                           filter_results$filtered_data,
                           selected_file())
      
      tableServer("settings",
                  filter_results$filtered_data,
                  selected_file())
      
    })
    

    # Return values that might be needed by parent module
    # return(
    #   list(
    #     filtered_data = filter_results$filtered_data,
    #     selected_file = filter_results$selected_file
    #     # navigate_request = settings_result$navigate_triggered
    #   )
    # )
  })
}
