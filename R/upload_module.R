# Module 1: File Upload UI
uploadUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 4,
      div(    
        card(
          full_screen = TRUE,
          card_header("Upload JSON Files"),
          fileInput(ns("json_files"), "Choose JSON File(s)", 
                    multiple = TRUE,
                    accept = ".json")
        ),
        card(
          full_screen = TRUE,
          card_header("Loaded JSON files:"),
          uiOutput(ns("file_selection"))
        )
      )
    ),
    column(
      width = 8,
      tagList(    
        card(
          full_screen = TRUE,
          card_header("JSON Metadata"),
          verbatimTextOutput(ns("json_content"))
        )
      )
    )
    
  )
}

# Module 1: File Upload Server
uploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    uploaded_files <- reactiveVal(list())
    
    observeEvent(input$json_files, {
      req(input$json_files)
      files <- input$json_files
      files_list <- uploaded_files()
      
      # Process each uploaded file
      for(i in 1:nrow(input$json_files)) {
        file_path <- input$json_files$datapath[i]
        file_name <- input$json_files$name[i]
        
        # Read JSON content
        content <- tryCatch(
          fromJSON(file_path, simplifyVector = F),
          error = function(e) NULL
        )
        
        if(!is.null(content)) {
          files_list[[file_name]] <- content
        }
      }
    
      uploaded_files(files_list)
    })
    
    # Generate radio buttons for file selection
    output$file_selection <- renderUI({
      files <- uploaded_files()
      if (length(files) == 0) {
        return(helpText("No files uploaded yet"))
      }
      
      radioButtons(session$ns("selected_file"), 
                   label = NULL,
                   choices = names(files))
    })
    
    # Function to remove specific elements from a list
    remove_specific_elements <- function(x) {
      if (is.list(x)) {
        # Remove specific elements if they exist
        x$elements <- NULL
        x$column <- NULL
        x$columns <- NULL  # including 'columns' in case it's plural
        x$row <- NULL
        x$rows <- NULL    # including 'rows' in case it's plural
        return(x)
      }
      return(x)
    }
    # Display selected JSON content
    output$json_content <- renderPrint({
      req(input$selected_file)
      files <- uploaded_files()
      # Get the selected file's content
      selected_content <- files[[input$selected_file]]

      # Remove specific elements from the content
      modified_content <- remove_specific_elements(selected_content)
      
      # Pretty print the modified structure
      str(modified_content)
    })
    
    return(uploaded_files)
  })
}
