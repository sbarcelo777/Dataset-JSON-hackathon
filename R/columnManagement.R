# 2. column_management_module.R
columnManagementUI <- function(id) {
  ns <- NS(id)
  div(
      class = "d-flex gap-2",
    actionButton(ns("hide_window"),
                 icon = icon("eye-slash"), 
                 NULL,
                 class = "btn-primary mb-3",
                 disabled = TRUE),
    uiOutput(ns("hidden")),
    actionButton(ns("sticky"),
                 icon = icon("bookmark"), 
                 NULL,
                 class = "btn-primary mb-3",
                 disabled = TRUE),
    uiOutput(ns("sticken")),
  )
}

columnManagementServer <- function(id, filtered_data, selected_file) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    selected_vars <- reactiveVal(NULL)
    selected_vars_s <- reactiveVal(NULL)
    
    # Initialize selected columns when dataset changes
    observe({
      req(selected_file())
      if(nchar(selected_file()) > 1) {
        if (is.null(selected_vars()[[selected_file()]])) {
          current_data <- filtered_data()
          column_init <- names(current_data)
          column_init_s <- if ("USUBJID" %in% names(current_data)) "USUBJID" else NULL
          
          current_selections <- selected_vars()
          current_selections_s <- selected_vars_s()
          current_selections[[selected_file()]] <- column_init
          current_selections_s[[selected_file()]] <- column_init_s
          
          selected_vars(current_selections)
          selected_vars_s(current_selections_s)
        }
        updateActionButton(session, "hide_window", disabled = FALSE)
        updateActionButton(session, "sticky", disabled = FALSE)
      }
    })
    
    # Hide/Unhide columns modal
    observeEvent(input$hide_window, {
      showModal(modalDialog(
        title = "Select Columns to display",
        div(
          style = "margin-bottom: 15px",
          actionButton(session$ns("selectAll"), "Select All"),
          actionButton(session$ns("unselectAll"), "Unselect All")
        ),
        div(
          style = "max-height: 400px; overflow-y: auto;",
          checkboxGroupInput(
            session$ns("columns"),
            label = NULL,
            choices = names(filtered_data()),
            selected = selected_vars()[[selected_file()]]
          )
        ),
        footer = tagList(
          modalButton("Close")
        ),
        size = "m"
      ))
    })
    
    # Select/Unselect All handlers
    observeEvent(input$selectAll, {
      updateCheckboxGroupInput(session,
                               inputId = "columns",
                               choices = names(filtered_data()),
                               selected = names(filtered_data()))
    })
    
    observeEvent(input$unselectAll, {
      updateCheckboxGroupInput(session,
                               inputId = "columns",
                               choices = names(filtered_data()),
                               selected = character(0))
    })
    
    # Update selected columns
    observeEvent(input$columns, {
      req(selected_file())
      # req(input$columns)
      
      current_selections <- selected_vars()
      current_selections[[selected_file()]] <- input$columns
      selected_vars(current_selections)
      
      # Force reactivity
      selected_vars(current_selections)
      
      # removeModal()
      showNotification("Column visibility updated", type = "message")
    })
    
    # Sticky columns modal
    observeEvent(input$sticky, {
      showModal(modalDialog(
        title = "Sticky Columns",
        div(
          style = "margin-bottom: 15px",
          checkboxInput(session$ns("select_all_s"),
                        "Select/Deselect All",
                        value = FALSE)
        ),
        div(
          style = "max-height: 400px; overflow-y: auto;",
          checkboxGroupInput(
            session$ns("columns_vars_s"),
            label = NULL,
            choices = names(filtered_data()),
            selected = selected_vars_s()[[selected_file()]]
          )
        ),
        footer = tagList(
          modalButton("Close")
        ),
        size = "m"
      ))
    })
    
    # Select/Deselect All sticky columns
    observeEvent(input$select_all_s, {
      if (input$select_all_s) {
        updateCheckboxGroupInput(session, "columns_vars_s",
                                 choices = names(filtered_data()),
                                 selected = names(filtered_data()))
      } else {
        updateCheckboxGroupInput(session, "columns_vars_s",
                                 choices = names(filtered_data()),
                                 selected = NULL)
      }
    })
    
    # Update sticky columns
    observeEvent(input$columns_vars_s, {
      req(selected_file())
      current_selections_s <- selected_vars_s()
      current_selections_s[[selected_file()]] <- input$columns_vars_s
      selected_vars_s(current_selections_s)
    })
    
    output$hidden <- renderUI({
      req(selected_file())
      data <- filtered_data()
      cols <- selected_vars()[[selected_file()]]
      if(is.null(col)) return(helpText(""))
      total_cols <- ncol(data)
      HTML(paste0(
        "<div style='padding: 10px;'>",
        "Total Columns: ", length(cols),"/",total_cols,
        "</div>"
      ))
    })
    
    output$sticken <- renderUI({
      req(selected_file())
      stick <- selected_vars_s()[[selected_file()]]
      
      if(is.null(stick)) {
        return(HTML("<div style='padding: 10px;'> None </div>"))
      }
      
      # If more than 3 variables
      if(length(stick) > 3) {
        return(HTML("<div style='padding: 10px;'> > 3 variables...</div>"))
      }
      
      # Default case: show the variables
      HTML(paste0(
        "<div style='padding: 10px;'>",
        "Stick : ", paste(stick, collapse = ", "),
        "</div>"
      ))
    })
    
    return(list(
      selected_vars = selected_vars,
      selected_vars_s = selected_vars_s
    ))
  })
}