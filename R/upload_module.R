uploadUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    layout_columns(
      col_widths = c(4, 8),
      
      card(
        max_height = "600px",
        card_header("File Management"),
        
        div(
          style = "display: flex; flex-direction: column; gap: 10px;",
          
          # Dropdown to select file format
          selectInput(ns("file_format"), "Select File Format", 
                      choices = c("JSON" = "json", "NDJSON" = "ndjson"),
                      selected = "json"),
          
          div(
            style = "display: flex; gap: 10px; align-items: flex-start;",
            
            div(
              style = "flex-grow: 1;",
              uiOutput(ns("file_input_ui"))  # Dynamic fileInput
            ),
            
            div(
              style = "margin-top: 25px;",
              actionButton(ns("remove_files"), 
                           "Remove All Files", 
                           class = "btn-primary")
            )
          )
        ),
        
        uiOutput(ns("file_selection"))
      )
      ,
      card(
        card_header("JSON Content Details"),
        verbatimTextOutput(ns("json_content"))
      )
    )
    ,

    layout_columns(
      col_widths = c(2, 10),
      page_fluid(
        uiOutput(ns("boxes"))
      ),
    card(
      card_header("Visualization"),
      plotlyOutput(ns("plot_metadata"))
    )
    )
  )
}

# Module 1: File Upload Server
uploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    options(shiny.maxRequestSize=5*1024^3)
    
    observe({
      file_ext <- input$file_format
      accept_types <- if (file_ext == "json") ".json" else ".ndjson"
      
      output$file_input_ui <- renderUI({
        fileInput(ns("json_files"), 
                  "Choose File(s)", 
                  multiple = TRUE, 
                  accept = accept_types)
      })
    })
    
    file_format <- reactive({
      input$file_format
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
    
    
    observeEvent(input$remove_files, {
      # session$reload()
      uploaded_files(NULL)
      logger::log_info("ALL FILES REMOVED")
    })
    
    observeEvent(input$file_format, {
      # session$reload()
      uploaded_files(NULL)
      logger::log_info(paste0("Change file format to "), input$file_format)
      
    })
    
    uploaded_files <- reactiveVal(list())
    records <- reactiveVal(list())
    labels <- reactiveVal(list())
    

    observeEvent(input$json_files, {
      req(input$json_files)
      files <- input$json_files
      files_list <- uploaded_files()

      # Process each uploaded file
      for(i in 1:nrow(input$json_files)) {
        file_path <- input$json_files$datapath[i]
        file_name <- input$json_files$name[i]
        if (input$file_format == "json"){
          logger::log_info(paste0("Uploaded file : "), file_name)
          # Read JSON content
          content <- tryCatch(
            fromJSON(file_path, simplifyVector = F),
            error = function(e) NULL
          )
    
        }else if (input$file_format == "ndjson"){
          
          logger::log_info(paste0("Uploaded file : "), file_name)
          # Read JSON content
          lines <- readLines(file_path, encoding = "UTF-8")
          # Extract metadata (first line is a JSON object)
          metadata <- fromJSON(lines[1])
          content <- metadata
          
        }
        

        if(!is.null(content)) {
          content$datapath <- file_path
          files_list[[file_name]] <- content
        }
      }
      # files_list[[file_path]] <- file_path
      
      uploaded_files(files_list)
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
          }
        
      return(x)
    }
    
    # Display selected JSON content
    output$json_content <- renderPrint({
      req(input$selected_file)
      files <- uploaded_files()
      if (length(files) == 0) {
        return(cat("Metadata will be shown there."))
      }
      
      # Get the selected file's content
      selected_content <- files[[input$selected_file]]
      modified_content <- remove_specific_elements(selected_content)
      
      # Pretty print the modified structure
      return(str(modified_content))
    })
    
    
    output$boxes <- renderUI({
      req(input$json_files)

      files <- uploaded_files()
      records <- t(sapply(uploaded_files(), function(x) c(records = x$records)))
      total_records <- format(sum(as.numeric(records)), big.mark = ".", decimal.mark = ",")


      vbs <- list(
        value_box(
          title = "Navigate accross",
          value = paste0(length(uploaded_files()), " CDISC Datasets"),
          showcase = bsicons::bs_icon("bar-chart"),
          theme = value_box_theme(bg = "#2C3E50", fg = "#fff"),
          p("more easily")
        ),
        value_box(
          title = "Covering the total amount of ",
          value = paste0(total_records, " records"),
          showcase = bsicons::bs_icon("graph-up"),
          theme = value_box_theme(bg = "#18BC9C", fg = "#fff"),
          p("in an efficient way"),
        )
      )

      layout_column_wrap(
        # width = "",
        !!!vbs
      )


    })
    
    output$plot_metadata <- renderPlotly({
      req(input$json_files)
      req(input$selected_file)


      records <- t(sapply(uploaded_files(), function(x) c(records = x$records)))
      labels <- t(sapply(uploaded_files(), function(x) c(labels = x$label)))

      files <- uploaded_files()
      selected_content <- files[[input$selected_file]]


      df <- as.data.frame(records)
      names(df) <- as.factor(labels)


      df <- tidyr::pivot_longer(df,
                          cols = everything(),
                          names_to = "Category",
                          values_to = "Count")

      p <- plot_ly(df,
                   y = ~Category,
                   x = ~Count,
                   type = "bar",
                   orientation = 'v',
                   marker = list(
                     color = ~ifelse(Category == selected_content$label, "#18BC9C", "#2C3E50")
                   )) %>%
        layout(
          xaxis = list(title = "Count"),
          yaxis = list(title = "")
        )


    p %>%
      layout(
        showlegend = FALSE,
        title = list(
          text = "Distribution of Records by Category",
          x = 0.5
        )
      ) %>%
      config(displayModeBar = FALSE)

    })
    

    return(list(
      uploaded_files = uploaded_files,
      file_format = file_format
    ))
  })
}
