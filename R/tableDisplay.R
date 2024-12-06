# 1. Table Display UI
tableDisplayUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      open = FALSE,
      width = 300,
      title = "Table Options",
      accordion(
        accordion_panel(
          "Display Options",
          checkboxInput(ns("show_label"), "Show labels", value = FALSE),
          downloadButton(ns("download_data"), "Download Data", 
                         class = "btn-primary btn-sm mb-3"),
          selectInput(ns("page_size"), "Rows per page", 
                       choices = c(15,30,50,100), selected = 30)
        ),
        accordion_panel(
          "Table Info",
          htmlOutput(ns("info"))
        )
      )
    ),
    reactableOutput(ns("json_table"))
  )
}

# 2. Table Display Server
tableDisplayServer <- function(id, filtered_data,
                               column_management,
                               # hide, sticky, 
                               selected_file,
                               labels) {
  moduleServer(id, function(input, output, session) {
    
    print(labels)
    # Create a reactive for the visible columns
    visible_columns <- reactive({
      req(filtered_data())
      req(column_management$selected_vars())
      
      # Get the selected columns for the current file
      selected <- column_management$selected_vars()[[selected_file()]]
      if (is.null(selected) || length(selected) == 0) {
        return(names(filtered_data()))
      }
      selected
    })
    
    
    # Create column definitions
    createColDef <- function(data) {
      # Ensure we have valid data
      req(data)
      if (nrow(data) < 1 || ncol(data) < 1) {
        return(NULL)
      }
      
      column_labels <- attr(data, "labels")
      label_list <- if (is.null(column_labels)) {
        list()
      } else if (is.vector(column_labels)) {
        as.list(column_labels)
      } else {
        column_labels
      }
      
      print(attr(data, "labels"))
      print(column_labels)
      print(label_list)
      
      # Safely get sticky columns
      col_sticky <- tryCatch({
        column_management$selected_vars_s()[[selected_file()]]
      }, error = function(e) NULL)
      
      colDefs <- lapply(names(data), function(name) {
        # Safely handle column data
        col_data <- tryCatch({
          data[[name]]
        }, error = function(e) NULL)
        
        if (is.null(col_data)) return(NULL)
        
        if (is.numeric(col_data)) {
          # Safe check for valid numeric data
          if (length(col_data) > 0 && sum(!is.na(col_data)) > 0) {
            colDef(
              header = if (input$show_label && !is.null(label_list[[name]])) 
                label_list[[name]] else name,
              filterable = TRUE,
              filterMethod = JS("filterRange"),
              filterInput = JS("muiRangeFilter"),
              sticky = if(!is.null(col_sticky) && name %in% col_sticky) "left",
              # format = colFormat(digits = 2),
              style = list(
                whiteSpace = "nowrap",
                overflow = "hidden",
                textOverflow = "ellipsis"
              )
            )
          } else {
            # Fallback for invalid numeric columns
            colDef(
              header = name,
              filterable = FALSE,
              style = list(color = "#999")
            )
          }
        } else {
          colDef(
            header = if (input$show_label && !is.null(label_list[[name]])) 
              label_list[[name]] else name,
            sticky = if(!is.null(col_sticky) && name %in% col_sticky) "left",
            filterable = TRUE,
            style = list(
              whiteSpace = "nowrap",
              overflow = "hidden",
              textOverflow = "ellipsis"
            )
          )
        }
      })
      
      # Remove NULL entries
      colDefs <- Filter(Negate(is.null), colDefs)
      names(colDefs) <- names(data)[seq_along(colDefs)]
      colDefs
    }
    
    # Render table with safety checks
    output$json_table <- renderReactable({
      req(filtered_data())
      req(visible_columns())
      
      # Get only the visible columns
      data_subset <- filtered_data()[, visible_columns(), drop = FALSE]
      
      # Create table with error handling
      tryCatch({
        reactable(
          data_subset,
          filterable = TRUE,
          sortable = TRUE,
          pagination = TRUE,
          borderless = TRUE,
          striped = TRUE,
          highlight = TRUE,
          compact = TRUE,
          resizable = TRUE,
          columns = createColDef(data_subset),
          defaultColDef = colDef(
            align = 'center', 
            vAlign = "center", 
            minWidth = 150
          ),
          theme = reactableTheme(
            borderColor = "#dfe2e5",
            stripedColor = "#f6f8fa",
            highlightColor = "#f0f5f9",
            cellPadding = "8px 12px"
          ),
          defaultPageSize = ifelse(is.null(input$page_size), 30, as.numeric(input$page_size)),
        )
      }, error = function(e) {
        showNotification(
          paste("Error creating table:", e$message),
          type = "error"
        )
        reactable(data.frame(Error = "Unable to display data"))
      })
    })
    
    # Table info output with safety checks
    output$info <- renderUI({
      req(filtered_data())
      
      tryCatch({
        data <- filtered_data()
        total_rows <- nrow(data)
        total_cols <- ncol(data)
        numeric_cols <- sum(sapply(data, is.numeric))
        factor_cols <- sum(sapply(data, is.factor))
        char_cols <- sum(sapply(data, is.character))
        
        HTML(paste0(
          "<div style='padding: 10px;'>",
          "<strong>Dataset Summary:</strong><br>",
          "Total Rows: ", total_rows, "<br>",
          "Total Columns: ", total_cols, "<br>",
          "Numeric Columns: ", numeric_cols, "<br>",
          "Factor Columns: ", factor_cols, "<br>",
          "Character Columns: ", char_cols,
          "</div>"
        ))
      }, error = function(e) {
        HTML("<div class='text-danger'>Error loading dataset information</div>")
      })
    })
    
    # Download handler with safety checks
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("data-", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        tryCatch({
          write.csv(filtered_data(), file, row.names = FALSE)
        }, error = function(e) {
          showNotification(
            "Error downloading data. Please try again.",
            type = "error"
          )
        })
      }
    )
  })
}