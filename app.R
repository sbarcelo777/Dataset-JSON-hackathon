library(shinyWidgets)
library(shiny)
library(bslib)
library(jsonlite)
library(DT)
library(dplyr)
library(reactable)
library(plotly)
library(purrr)
library(reactR)
library(htmltools)
library(bsicons)
library(logger)
library(haven)
library(fontawesome)


# Source all .R files in the R/ directory
file_paths <- list.files("R", pattern = "\\.R$", full.names = TRUE)

lapply(file_paths, source)

# Main UI
ui <- page_navbar(
  theme = bs_theme(preset = "flatly",
                   font_scale = 0.95,
                   bg = "#fff",
                   fg = "#2C3E50"
                   ),
  title = "JSON File Viewer",
  id = "mainTabs",
  nav_panel(
    title = "Home",
    uploadUI("upload")
  ),
  nav_panel(
    title = "Viewer",
    value = "viewer2_tab",
    viewerUI("viewer")
  ),
  nav_panel(
    title = "Checks",
    value = "checks_tab",
    checksUI("checks")
  ),
  nav_spacer()
)



# Main Server
server <- function(input, output, session) {
  # bs_themer()
  
  checks_ready <- reactiveVal(FALSE)
  
  upload_data <- uploadServer("upload")
  
  observe({
    # Check if files are uploaded
    has_files <- !is.null(upload_data$uploaded_files()) && length(upload_data$uploaded_files()) > 0
    
    # Hide/show viewer tabs based on file upload status
    if (!has_files) {
      hideTab(inputId = "mainTabs", target = "viewer2_tab")
      hideTab(inputId = "mainTabs", target = "checks_tab")
    } else {
      showTab(inputId = "mainTabs", target = "viewer2_tab")
    }
  })
  
  viewer_result <- viewerServer("viewer", upload_data)
  
  # checks_data <- checksServer("checks", upload_data$uploaded_files)
  
  # Observe the navigation request from the nested modules
  observeEvent(viewer_result$navigate_request(), {

    # later::later(function() {
      # Call checksServer without passing session explicitly
      # as it's either handled internally or not needed
      checksServer("checks", upload_data$uploaded_files)
      showTab(inputId = "mainTabs", target = "checks_tab")
      updateNavbarPage(session, "mainTabs", selected = "checks_tab")
    # }, 0.1)
  })
  
  # # Observe the navigation request from the nested modules
  # observeEvent(checks_ready(), {
  #   if(checks_ready()) {
  #     # Hide loading indicator if you showed one
  #     # removeModal()
  #     
  #     # Navigate to the "Checks" tab
  #     showTab(inputId = "mainTabs", target = "checks_tab")
  #     updateNavbarPage(session, "mainTabs", selected = "checks_tab")
  #   }
  # }, ignoreInit = TRUE)
  
  # checksServer("checks", upload_data$uploaded_files)
 
  observe({
    if (is.null(upload_data$uploaded_files()) || length(upload_data$uploaded_files()) == 0) {
      updateNavbarPage(session, "mainTabs", selected = "Home")
    }
  })
  
}

shinyApp(ui, server)
