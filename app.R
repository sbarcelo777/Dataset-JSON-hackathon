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
      uploadUI("upload"),
  ),
  nav_panel(
    title = "Viewer",
    value = "viewer2_tab",
    viewerUI("viewer"),
  ),
  nav_spacer()
)



# Main Server
server <- function(input, output, session) {
  # bs_themer()

  upload_data <- uploadServer("upload")

  
  observe({
    # Check if files are uploaded
    has_files <- !is.null(upload_data$uploaded_files()) && length(upload_data$uploaded_files()) > 0
    
    # Hide/show viewer tabs based on file upload status
    if (!has_files) {
      hideTab(inputId = "mainTabs", target = "viewer2_tab")
    } else {
      showTab(inputId = "mainTabs", target = "viewer2_tab")
    }
  })
  
  viewerServer("viewer", upload_data)
  
  
  # observeEvent(uploaded_files(), {
  observe({
    if (is.null(upload_data$uploaded_files()) || length(upload_data$uploaded_files()) == 0) {
      updateNavbarPage(session, "mainTabs", selected = "Home")
    }
  })
  
}

shinyApp(ui, server)
