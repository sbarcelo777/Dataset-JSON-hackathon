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
  shinyjs::useShinyjs(),
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
 
  # Observe the navigation request from the nested modules
  observeEvent(viewer_result$navigate_request(), {
    # Create a progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Running checks", value = 0.1)
    
    # Show the modal
    showModal(modalDialog(
      title = "Running Checks",
      div(id = "modal-content", "Waiting for execution of checks..."),
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Call the module with the progress object
    check_status <- checksServer("checks", upload_data$uploaded_files, progress)
    
    # Create an observer that waits for the module to complete
    observe({
      # Only proceed when checks are complete
      req(check_status())
      
      # Final progress step
      progress$set(value = 1, detail = "Checks complete!")
      
      # Update the UI
      showTab(inputId = "mainTabs", target = "checks_tab")
      updateNavbarPage(session, "mainTabs", selected = "checks_tab")
      
      # Clean up
      progress$close()
      removeModal()
      
      # Log completion
      logger::log_info("Checks completed successfully")
    })
    
    # Handle potential errors or timeouts
    observeEvent(check_status(), {
      if (!check_status()) {
        progress$close()
        removeModal()
        showNotification("The check process did not complete successfully", type = "error")
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
  })
  

  observe({
    if (is.null(upload_data$uploaded_files()) || length(upload_data$uploaded_files()) == 0) {
      updateNavbarPage(session, "mainTabs", selected = "Home")
    }
  })
  
}

shinyApp(ui, server)
