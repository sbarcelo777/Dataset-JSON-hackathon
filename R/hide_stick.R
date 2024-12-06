# Column Hide Module
columnHideUI <- function(id) {
  ns <- NS(id)
  actionButton(ns("hide_window"), "Hide/Unhide columns", 
               class = "btn-primary mb-3",
               disabled = TRUE)
}

columnHideServer <- function(id, filtered_data, selected_file) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    selected_vars <- reactiveVal(NULL)
    
    # Initialize selected columns when dataset changes
    observe({
      req(selected_file())
      if(nchar(selected_file()) > 1) {
        if (is.null(selected_vars()[[selected_file()]])) {
          current_data <- filtered_data()
          column_init <- names(current_data)
          
          current_selections <- selected_vars()
          current_selections[[selected_file()]] <- column_init
          
          selected_vars(current_selections)
        }
        updateActionButton(session, "hide_window", disabled = FALSE)
      }
    })
    
    # Hide/Unhide columns modal
    observeEvent(input$hide_window, {
      showModal(modalDialog(
        title = "Select Columns",
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
      
      current_selections <- selected_vars()
      current_selections[[selected_file()]] <- input$columns
      selected_vars(current_selections)
      
      showNotification("Column visibility updated", type = "message")
    })
    
    return(selected_vars)
  })
}

# Column Sticky Module
columnStickyUI <- function(id) {
  ns <- NS(id)
  actionButton(ns("sticky"), "Stick/Unstick columns", 
               class = "btn-primary mb-3",
               disabled = TRUE)
}

columnStickyServer <- function(id, filtered_data, selected_file) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    selected_vars_s <- reactiveVal(NULL)
    
    # Initialize selected columns when dataset changes
    observe({
      req(selected_file())
      if(nchar(selected_file()) > 1) {
        if (is.null(selected_vars_s()[[selected_file()]])) {
          current_data <- filtered_data()
          column_init_s <- if ("USUBJID" %in% names(current_data)) "USUBJID" else NULL
          
          current_selections_s <- selected_vars_s()
          current_selections_s[[selected_file()]] <- column_init_s
          
          selected_vars_s(current_selections_s)
        }
        updateActionButton(session, "sticky", disabled = FALSE)
      }
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
    
    return(selected_vars_s)
  })
}