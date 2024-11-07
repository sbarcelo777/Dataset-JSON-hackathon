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
          textOutput(ns("JSON_list"))
        )
      )
    ),
    column(
      width = 8,
      tagList(    
        card(
          full_screen = TRUE,
          card_header("Files metrics")
        )
      )
    )
    
  )
}

# Module 1: File Upload Server
uploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    uploaded_files <- reactiveVal(list())
    JSON_list_text <- reactiveVal("No JSON files selected")
    
    observeEvent(input$json_files, {
      req(input$json_files)
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
      
      JSON_list_text(names(files_list))
      
      uploaded_files(files_list)
    })
    
    output$JSON_list <- renderText({
      # This can be any dynamic content
      paste("You selected option:", JSON_list_text())
      #paste("You selected option:", "no var")
    })
    return(uploaded_files)
  })
}
