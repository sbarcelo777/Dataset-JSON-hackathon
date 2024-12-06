# Source all .R files in the R/ directory
file_paths <- list.files("module_approach", pattern = "\\.R$", full.names = TRUE)

lapply(file_paths, source)

viewerUI22 <- function(id) {
  ns <- NS(id)
  
  # Add custom CSS for fixed positioning
  tags$head(
    tags$style(HTML("
      .fixed-card {
        position: sticky;
        top: 1rem;
        height: calc(100vh - 2rem);
        overflow-y: auto;
      }
      
      .main-content {
        min-height: calc(100vh - 2rem);
      }
    "))
  )
  
  page_fluid(
    muiDependency(),
    tags$script(jsCode),
    layout_columns(
      col_widths = c(3, 9),
      fill = FALSE,
      
      # Left sidebar with controls - now with fixed positioning
      div(
        class = "fixed-card",
        card(
          height = "100%",
          card_header(
            "Control Panel",
            popover(
              "Click for help",
              "This panel contains all controls for filtering and analyzing your data"
            )
          ),
          layout_column_wrap(
            width = 1,
            # Filter section
            filterUI(ns("filter")),
            
            # Variable description section
            varDescriptionUI(ns("var_desc"))
          )
        )
      ),
      
      # Main content area
      div(
        class = "main-content",
        card(
          full_screen = TRUE,
          card_header(
            "Data Explorer",
            class = "d-flex justify-content-between align-items-center py-2",  # Added py-2 to reduce vertical padding
            columnManagementUI(ns("col_mgmt"))
          ),
          tableDisplayUI(ns("table"))
        )
      )
    )
  )
}

# 4. Viewer Server 2
viewerServer22 <- function(id, uploaded_files) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize filter module
    filter_results <- filterServer("filter", uploaded_files)
    
    # Initialize column management module
    col_management <- columnManagementServer(
      "col_mgmt",
      filter_results$filtered_data,
      filter_results$selected_file
    )
 
    # Initialize variable description module
    varDescriptionServer(
      "var_desc",
      filter_results$filtered_data,
      filter_results$selected_file
    )
    
    
    print(filter_results$labels)
    
    # Observe column management changes and trigger table updates
    observe({
      
      # # This will re-render the table when column settings change
      col_management$selected_vars()
      col_management$selected_vars_s()
      
      # Initialize table display module
      tableDisplayServer(
        "table",
        filter_results$filtered_data,
        col_management,
        filter_results$selected_file,
        filter_results$labels
      )
      
    })
    
    # Return values that might be needed by parent module
    return(list(
      filtered_data = filter_results$filtered_data,
      selected_file = filter_results$selected_file,
      column_settings = col_management,
      visible_columns = col_management$selected_vars,
      sticky_columns = col_management$selected_vars_s
    ))
  })
}