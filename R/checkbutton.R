# button_module.R
library(shinyWidgets)

buttonUI <- function(id) {
  ns <- NS(id)
  actionButton(ns("navigate_button"),
               label =  "Run checks",
               icon = icon("check"),
               # class = "btn-primary",
               style = "unite", 
               status = "primary")
}

buttonServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Return a reactive value that becomes TRUE when the button is clicked
    return(reactive({
      input$navigate_button
    }))
  })
}