mod_column_selector_server <- function(id, filtered_data, file_select) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store selected variables
    selected_vars <- reactiveVal(list())
    
    observeEvent(input$hide_window, {
      column_choice <- names(filtered_data()[5:ncol(filtered_data())])
      current_selected <- selected_vars()[[file_select()]]
      
      showModal(
        modalDialog(
          title = "Column Visibility",
          div(
            div(
              style = "margin-bottom: 15px;",
              checkboxInput(ns("select_all"),
                            "Select/Deselect All",
                            value = length(current_selected) == length(column_choice)
              )
            ),
            div(
              style = "max-height: 400px; overflow-y: auto;",
              checkboxGroupInput(
                ns("columns_vars"),
                label = NULL,
                choices = column_choice,
                selected = current_selected
              )
            )
          ),
          footer = tagList(
            modalButton("X")
          ),
          size = "m"
        )
      )
    })
    
    observeEvent(input$select_all, {
      column_choice <- names(filtered_data()[5:ncol(filtered_data())])
      if (input$select_all) {
        updateCheckboxGroupInput(session, "columns_vars",
                                 choices = column_choice,
                                 selected = column_choice)
      } else {
        updateCheckboxGroupInput(session, "columns_vars",
                                 choices = column_choice,
                                 selected = NULL)
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$columns_vars, {
      req(file_select())
      current_selections <- selected_vars()
      current_selections[[file_select()]] <- input$columns_vars
      selected_vars(current_selections)
    })
    
    # Return the selected columns as a reactive expression
    return(
      reactive({
        req(file_select())
        selected_vars()[[file_select()]]
      })
    )
  })
}