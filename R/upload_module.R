uploadUI <- function(id) {
  ns <- NS(id)
  
  # Add CSS for responsiveness
  css <- tags$style(HTML("
    /* Adjust spacing on smaller screens */
    @media (max-width: 992px) {
      .card-body { padding: 0.75rem; }
      .form-group { margin-bottom: 0.5rem; }
      .responsive-card { margin-bottom: 1rem; }
    }
    
    /* Stack buttons vertically on very small screens */
    @media (max-width: 576px) {
      .btn-group-responsive { display: flex; flex-direction: column; }
      .btn-group-responsive .btn { margin-bottom: 0.5rem; width: 100%; }
    }
    
 
  "))
  
  page_fluid(
    css,
    layout_column_wrap(
      width = "400px", # Columns will wrap when narrower than this
      heights_equal = "row",
      layout_columns(
        col_widths = c(3, 9),
        heights_equal = "row",
        
        # File Management Card
        card(
          class = "responsive-card",
          full_screen = FALSE,
          card_header(
            div(
              style = "display: flex; align-items: center;",
              icon("file-alt"),
              span(style = "margin-left: 10px; font-size: 18px;", "File Management")
            )
          ),
          selectInput(ns("file_format"), "File Format", 
                      choices = c(
                        "JSON" = "json", 
                        "NDJSON" = "ndjson", 
                        "SAS XPT" = "xpt", 
                        "SAS7BDAT" = "sas7bdat"
                      ),
                      selected = "json"),
          uiOutput(ns("file_input_ui")),  # Dynamic fileInput
          div(
            class = "d-grid gap-2", # Bootstrap grid for button
            actionButton(ns("remove_files"), 
                         "Clear All Files", 
                         class = "btn-danger btn-sm",
                         icon = icon("trash-alt"))
          )
        ),
        
        # Available Files Card
        card(
          max_height = "600px",
          full_screen = TRUE,
          layout_sidebar(
            fillable = FALSE,
            sidebar = sidebar(
              open = "always",
              width = 250,
              position = "left",
              uiOutput(ns("file_selection"))
            ),
            uiOutput(ns("json_content")
            )              
          )
        )
      )
      
    ),
    
    # Second section - Boxes and Visualization
    layout_column_wrap(
      width = "250px",
      heights_equal = "row",
      
      # Boxes Section - Directly using value boxes without a card
      uiOutput(ns("boxes")),
      uiOutput(ns("visualisation"), height = "300px")
      
    )
  )
}

# Module 1: File Upload Server
uploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    uploaded_files <- reactiveVal(list())
    records <- reactiveVal(list())
    labels <- reactiveVal(list())
    actual_format <- reactiveVal()
    
    options(shiny.maxRequestSize=5*1024^3)
    
    observe({
      file_ext <- input$file_format
      accept_types <- case_when(file_ext == "json" ~ ".json",
                                file_ext == "ndjson" ~ ".ndjson",
                                file_ext == "xpt" ~ ".xpt",
                                file_ext == "sas7bdat" ~ ".sas7bdat")
      
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
      logger::log_info(paste0("Change file format to "), input$file_format)
      
    })

    observeEvent(input$json_files, {
      req(input$json_files)
      
      if(!is.null(actual_format()) && actual_format() != input$file_format) uploaded_files(NULL)
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
          label <- content$label
          name <- content$name
          records <- content$records
          originator <- content$originator
          
          files_list[[file_name]] <- list(
            label = label,
            name = name,
            records = records,
            path = file_path,
            originator = originator
          )
          

        }else if (input$file_format == "ndjson"){
          logger::log_info(paste0("Uploaded file : "), file_name)
          
          # Read JSON content
          lines <- readLines(file_path, encoding = "UTF-8")
          content <- fromJSON(lines[1])
          
          files_list[[file_name]] <- list(
            label = content$label,
            name = content$name,
            records = content$records,
            path = file_path
          )

        }else if (input$file_format == "xpt"){
          logger::log_info(paste0("Uploaded file : "), file_name)
          
          content <- haven::read_xpt(file_path)
          label <- attr(content, "label")
          name <- toupper(sub("\\.XPT$", "", file_name, ignore.case = TRUE))

          files_list[[file_name]] <- list(
            label = label,
            name = name,
            path = file_path,
            records = nrow(content),
            content = content
          )

        }else if (input$file_format == "sas7bdat"){
          logger::log_info(paste0("Uploaded file : "), file_name)
          
          content <- haven::read_sas(file_path, encoding = "UTF-8")
          label <- attr(content, "label")
          name <- toupper(sub("\\.SAS7BDAT$", "", file_name, ignore.case = TRUE))
          
          files_list[[file_name]] <- list(
            label = label,
            name = name,
            path = file_path,
            records = nrow(content),
            content = content
          )
        }

      }

      uploaded_files(files_list)
      actual_format(tolower(tools::file_ext(input$json_files$name[i])))
      logger::log_info(paste0("Actual file format: ", actual_format()))
      
    })
  
    


    # Display selected JSON content
    output$json_content <- renderUI({
      req(input$selected_file)
      req(input$selected_file)
      req(uploaded_files())
      files <- uploaded_files()
      selected_file <- files[[input$selected_file]]
      file_path <- selected_file$path

      if (length(files) == 0) {
        return(cat("Metadata will be shown there."))
      }
      if(!actual_format() %in% c("json", "ndjson")){
        plotlyOutput(ns("plot_metadata"))
      }else{
        if (actual_format() == "json"){
          # Read JSON content
          content <- tryCatch(
            fromJSON(file_path, simplifyVector = F),
            error = function(e) NULL
          )
        }else if (actual_format() == "ndjson"){
          # Read JSON content
          lines <- readLines(file_path, encoding = "UTF-8")
          content <- fromJSON(lines[1])
        }

        modified_content <- remove_specific_elements(content)
        # Convert and transpose your data
        df_transposed <- list_to_transposed_df(modified_content)

        # Display as an interactive table
       reactable(df_transposed, striped = TRUE, highlight = TRUE, bordered = TRUE, defaultPageSize = 20, style = list(fontSize = "11px"),columns = list(
         Key = colDef(width = 150)  # Set desired width in pixels
       ))
      }
    })
    
   


    output$boxes <- renderUI({

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
      req(uploaded_files())


      records <- t(sapply(uploaded_files(), function(x) c(records = x$records)))
      labels <- t(sapply(uploaded_files(), function(x) {
        c(label = if (is.null(x$label)) x$name else x$label)
      }))

      files <- uploaded_files()
      selected_content <- files[[input$selected_file]]
      if(is.null(selected_content$label)) selected_content$label <- selected_content$name


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
    

    output$visualisation <- renderUI({
      req(actual_format())
      req(uploaded_files())
      if(actual_format() %in% c("json", "ndjson")){
        plotlyOutput(ns("plot_metadata"))
      }else{
        NULL
      }
    })


    return(list(
      uploaded_files = uploaded_files,
      file_format = file_format
    ))
  })
}
