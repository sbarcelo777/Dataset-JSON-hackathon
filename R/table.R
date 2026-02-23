# Updated Table Module UI
tableUI <- function(id) {
  ns <- NS(id)
  
  css <- "
  /* Modern scrollbar styling for webkit browsers (Chrome, Safari, Edge) */
  .ReactTable .rt-table::-webkit-scrollbar {
    width: 6px;  /* Width of vertical scrollbar */
    height: 6px; /* Height of horizontal scrollbar */
  }

  .ReactTable .rt-table::-webkit-scrollbar-track {
    background: rgba(0, 0, 0, 0.05); /* Subtle light gray background */
    border-radius: 10px;
  }

  .ReactTable .rt-table::-webkit-scrollbar-thumb {
    background: rgba(44, 62, 80, 0.5); /* Semi-transparent primary color */
    border-radius: 10px;
    transition: background 0.2s ease;
  }

  .ReactTable .rt-table::-webkit-scrollbar-thumb:hover {
    background: rgba(44, 62, 80, 0.8); /* Darker on hover */
  }

  .ReactTable .rt-table::-webkit-scrollbar-corner {
    background: transparent; /* Clean corner where scrollbars meet */
  }

  /* Firefox styling */
  .ReactTable .rt-table {
    scrollbar-width: thin;
    scrollbar-color: rgba(44, 62, 80, 0.5) rgba(0, 0, 0, 0.05);
  }
"
  
  
  tags$head(
    tags$style(css)
  )
  
  page_fluid(
    theme = bs_theme(
      version = 5,
      primary = "#2C3E50",  # Une couleur bleu foncé élégante
    ),
    tags$head(
      tags$style(HTML(css))
    ),
    div(
      style = "display: flex; align-items: center; gap: 10px;",
      dropdown(
        materialSwitch(
          inputId = ns("show_label"),
          label = "Show Labels",
          value = FALSE,
          status = "primary"
        ),
        pickerInput(
          inputId = ns("column_picker"),
          label = "Columns to show",
          choices = NULL,
          multiple = TRUE,
          options = pickerOptions(container = "body", 
                                  liveSearch = TRUE,
                                  size = 15)
        ),
        pickerInput(
          inputId = ns("sticky_picker"),
          label = "Columns to stick",
          choices = NULL,
          multiple = TRUE,
          options = pickerOptions(container = "body", 
                                  liveSearch = TRUE,
                                  size = 15)
        ),
        downloadButton(
          outputId = ns("download_data"),
          label = "Download",
          icon = icon("download"),
          class = "btn-primary",
          style = "unite",
          block = TRUE
        ),
        style = "unite", 
        icon = icon("gear"),
        status = "primary",
        width = "300px",
        animate = animateOptions(
          enter = animations$zooming_entrances$zoomIn,
          exit = animations$zooming_exits$zoomOut
        )
      ),
      dropdown(
        pickerInput(
          inputId = ns("variable_picker"),
          label = "Chose a variable",
          choices = NULL,
          multiple = FALSE,
          options = pickerOptions(container = "body", 
                                  liveSearch = TRUE,
                                  size = 15),
        ),
        uiOutput(ns("dynamic_input_container")),
        colorPickr(
          inputId = ns("color_picker"),
          label = "Colors",
          selected = "#2C3E50",  # Flatly's default dark blue
          swatches = c(
            "#2C3E50",  # Dark blue (primary)
            "#18BC9C",  # Teal (success)
            "#3498DB",  # Blue (info)
            "#F39C12",  # Orange (warning)
            "#E74C3C",  # Red (danger)
            "#95A5A6",  # Gray
            "#34495E",  # Darker blue
            "#16A085",  # Darker teal
            "#2980B9"   # Darker blue (alternate)
          ),
          update = "change",
          hue = FALSE,
          
          interaction = list(
            hex= FALSE,
            rgba = FALSE,
            input = FALSE,
            save = FALSE,
            clear = FALSE
          )
        ),
        style = "unite", 
        icon = icon("highlighter"),
        status = "primary",
        width = "300px",
        animate = animateOptions(
          enter = animations$zooming_entrances$zoomIn,
          exit = animations$zooming_exits$zoomOut
        )
      )
    ),
    addSpinner(reactableOutput(ns("reac")
                               , height = "1000px"
    ), spin = "rotating-plane")
  )
}

# Updated Table Module Server
tableServer <- function(id, filtered_data, selected_file) {
  moduleServer(id, function(input, output, session) {
    
    data_ready <- reactiveVal(FALSE)
    
    debounced_data <- reactive({
      req(filtered_data())
      filtered_data()
    }) %>% debounce(300) 
    
    
    observe({
      req(debounced_data())
      
      updatePickerInput(session,
                        "column_picker", 
                        choices = names(filtered_data()),
                        selected = names(filtered_data()),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 0",
                          `select-all-text` = "All columns",
                          `count-selected-text` = "{0} / {1} columns selected"
                        ))
      
      updatePickerInput(session,
                        "sticky_picker", 
                        choices = names(filtered_data()),
                        selected = NULL,
                        options = list(
                          `actions-box` = FALSE,
                          `multiple-separator` = " | ",
                          `max-options` = 3,
                          `max-options-text` = "Please chose only 3"
                        ))
      updatePickerInput(session,
                        "variable_picker",
                        choices = names(filtered_data()),
                        selected = character(0)
      )
      
      
      data_ready(TRUE)
      # print(input$num_value_picker[1])
      
    })
    
    output$dynamic_input_container <- renderUI({
      ns <- session$ns
      # Get the selected column data
      selected_col <- filtered_data()[,input$variable_picker, drop = TRUE]
      
      if (is.numeric(selected_col) || is.integer(selected_col)) {
        
        # For numeric columns, create a sliderInput
        min_val <- min(selected_col, na.rm = TRUE)
        max_val <- max(selected_col, na.rm = TRUE)
        
        sliderInput(
          inputId = ns("num_value_picker"),
          label = "Select Range",
          min = min_val,
          max = max_val,
          value = c(min_val, max_val),
          step = 1
        )
      } else {
        # For non-numeric columns, use picker
        choices <- unique(selected_col)
        pickerInput(
          inputId = ns("value_picker"),
          label = "Select Values",
          choices = choices,
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(container = "body", 
                                  liveSearch = TRUE,
                                  size = 15)
        )
      }
    })
    
    error_message <- reactiveVal(NULL)
    
    # Validate data function
    validate_data <- function(data, col_sticky) {
      if (is.null(data) || ncol(data) < 1) {
        error_message("Error: Dataset must have at least one column")
        return(FALSE)
      }
      error_message(NULL)
      return(TRUE)
    }
    
    processed_data <- reactive({
      req(debounced_data())
      tryCatch({
        # Get selected columns or use all if none selected
        columns_to_show <- if (is.null(input$column_picker)) names(debounced_data()) else input$column_picker
        
        # Validate columns exist
        if (length(columns_to_show) < 1) {
          return(NULL)
        }
        
        # Create subset of data
        data_subset <- debounced_data()[, columns_to_show, drop = FALSE]
        
        # Validate resulting dataset
        if (!validate_data(data_subset)) {
          return(NULL)
        }
        
        return(data_subset)
      }, error = function(e) {
        error_message(paste("Error processing data:", e$message))
        return(NULL)
      })
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
      
      col_sticky <- input$sticky_picker
      
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
              minWidth = 150,  # Set minimum width
              resizable = TRUE,  # Allow manual column resizing
              style = list(
                whiteSpace = "nowrap"  # Keep text in one line
              )
            )
          } else {
            # Fallback for invalid numeric columns
            colDef(
              header = name,
              filterable = FALSE,
              minWidth = 150,  # Set minimum width
              resizable = TRUE,  # Allow manual column resizing
              style = list(
                color = "#999",
                whiteSpace = "nowrap"  # Keep text in one line
              )
            )
          }
        } else {
          colDef(
            header = if (input$show_label && !is.null(label_list[[name]]))
              label_list[[name]] else name,
            sticky = if(!is.null(col_sticky) && name %in% col_sticky) "left",
            filterable = TRUE,
            minWidth = 150,  # Set minimum width
            resizable = TRUE,  # Allow manual column resizing
            style = list(
              whiteSpace = "nowrap"  # Keep text in one line
            )
          )
        }
      })
      
      # Remove NULL entries
      colDefs <- Filter(Negate(is.null), colDefs)
      names(colDefs) <- names(data)[seq_along(colDefs)]
      colDefs
    }
    
    output$reac <- renderReactable({
      req(debounced_data(), data_ready(), input$column_picker)
      
      data <- processed_data()
      
      
      # Create table with error handling
      tryCatch({
        reactable(
          data,
          showPageSizeOptions = TRUE,
          defaultPageSize = 25,
          filterable = TRUE,
          sortable = TRUE,
          pagination = TRUE,
          borderless = TRUE,
          striped = TRUE,
          highlight = TRUE,
          compact = TRUE,
          resizable = TRUE,
          rowStyle = function(index) {
            if(!is.null(input$variable_picker)){
              if (data[index, input$variable_picker] %in% input$value_picker || 
                  (!is.null(input$num_value_picker) && length(input$num_value_picker) == 2 && 
                   input$num_value_picker[1] <= data[index, input$variable_picker] && 
                   data[index, input$variable_picker] <= input$num_value_picker[2] && 
                   (input$num_value_picker[1] != min(data[, input$variable_picker], na.rm = TRUE) || 
                    input$num_value_picker[2] != max(data[, input$variable_picker], na.rm = TRUE)))) {
                
                
                bg_color <- as.character(input$color_picker)
                list(background = bg_color,
                     color = auto_text_color(bg_color)
                )
              } 
            }
          },
          columns = createColDef(filtered_data()),
          defaultColDef = colDef(
            align = 'center', 
            vAlign = "center", 
            minWidth = 150
          )
          ,
          theme = reactableTheme(
            # backgroundColor = "#2C3E50",
            # borderColor = "#34495E",
            # stripedColor = "#3A5875",
            # highlightColor = "#446A90",
            # color = "hsl(0, 0%, 87%)",
            # cellPadding = "8px 12px",
            # inputStyle = list(backgroundColor = "#3A5875"),
            # selectStyle = list(backgroundColor = "#3A5875"),
            # pageButtonHoverStyle = list(backgroundColor = "#3A5875"),
            # pageButtonActiveStyle = list(backgroundColor = "#446A90")
          )
        )
      }, error = function(e) {
        # showNotification(
        # paste("Error creating table:", e$message),
        # type = "error"
        # )
        # reactable(data.frame(Error = "Unable to display data"))
      })
      
    })
    
    name <- reactive({
      req(selected_file)
      selected_name_label <- strsplit(selected_file, "-->")[[1]]
      selected_name <- selected_name_label[1]
      return(selected_name)
    })
    
    # Download handler with safety checks
    output$download_data <- downloadHandler(
      
      filename = function() {
        paste0(name(),"_", format(Sys.Date(), "%Y%m%d"), ".csv")
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