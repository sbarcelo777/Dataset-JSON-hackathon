uploadUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    layout_columns(
      col_widths = c(4, 8),
      
      card(
        max_height = "600px",
        card_header("File Management"),
        div(
          style = "display: flex; gap: 10px; align-items: flex-start;",
          div(
            style = "flex-grow: 1;",
            fileInput(ns("json_files"), 
                      "Choose JSON File(s)", 
                      multiple = TRUE,
                      accept = c(".json", ".ndjson"))
          ),
          div(
            style = "margin-top: 25px;",
            actionButton(ns("remove_files"), 
                         "Remove All Files", 
                         class = "btn-primary")
          )
        ),
        uiOutput(ns("file_selection"))
      ),
      card(
        card_header("JSON Content Details"),
        verbatimTextOutput(ns("json_content"))
      )
    ),
    
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
    
    options(shiny.maxRequestSize=5*1024^3)
    
    observeEvent(input$remove_files, {
      session$reload()
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
      files <- uploaded_files()
      if (length(files) == 0) {
        return(cat("Metadata will be shown there."))
      }
      
      req(input$selected_file)
      
      # Get the selected file's content
      selected_content <- files[[input$selected_file]]

      # Remove specific elements from the content
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
        # ,
        # value_box(
        #   # title = "Welcome to our ",
        #   value = shiny::img(src = "json.svg", width = "180px", height = "180px"),
        #   # showcase = shiny::img(src = "json.svg", width = "180px", height = "180px"),
        #   theme = value_box_theme(bg = "#2C3E50", fg = "#fff"),
        #   p("Feel free to share your feedback with us."),
        #   p("Sebastià Barceló"),
        #   p("Hugo Signol"),
        #   p("v0.1")
        # )
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
    

    return(uploaded_files)
  })
}
