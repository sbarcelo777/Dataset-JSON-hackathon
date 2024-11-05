# Module 2: JSON Viewer UI
viewerUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      full_screen = TRUE,
      card_header("View JSON Content"),
      selectInput(ns("file_select"), "Select JSON File", choices = NULL),
      DTOutput(ns("json_table"))
    )
  )
}

# Module 2: JSON Viewer Server
viewerServer <- function(id, uploaded_files) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      files <- names(uploaded_files())
      updateSelectInput(session, "file_select", 
                        choices = c("Select a file" = "", files))
    })
    
    output$json_table <- renderDT({
      req(input$file_select)
      json_data <- uploaded_files()[[input$file_select]]
      
      # json_str <- readr::read_file(json_data)
      # data <- jsonlite::fromJSON(json_str, simplifyVector = F)
      
      cols <- c(sapply(json_data$columns, function(x) x$name))
      
      render_df <- json_data$rows %>% 
        lapply(function(row) {
          data.frame(matrix(unlist(row), nrow = 1), stringsAsFactors = FALSE)
        }) %>%
        dplyr::bind_rows()
      
      names(render_df) <- cols
      datatable(render_df)
      
      
      
      
      # if(is.data.frame(json_data)) {
      #   datatable(json_data)
      # } else if(is.list(json_data)) {
      #   # Convert list to data frame if possible
      #   df <- as.data.frame(json_data)
      #   datatable(df)
      # }
    })
  })
}
