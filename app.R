library(shiny)
library(bslib)
library(jsonlite)
library(DT)
library(dplyr)
library(reactable)

# Source the module files
source("R/upload_module.R")
source("R/viewer_module.R")
# Main UI
ui <- page_navbar(
  theme = bs_theme(preset = "flatly"),

#  layout_column_wrap(
#    width = 1/2,
#   uploadUI("upload"),
#    viewerUI("viewer")
#  )  
    title = "JSON File Viewer",
    id = "mainTabs",
    # Assign an ID to the tabsetPanel
    nav_panel(
      title = "Home",
      uploadUI("upload")
    ),    
    nav_panel(
      title = "Viewer1",
      viewerUI("viewer")
    ),
    nav_panel(
      title = "Viewer2",
      # tidyviewerUI("tidyviewer")
    ),
  )

# Main Server
server <- function(input, output, session) {
  #bs_themer()

  uploaded_files <- uploadServer("upload")
  viewerServer("viewer", uploaded_files)
  # tidyviewerServer("tidyviewer", uploaded_files)
}

shinyApp(ui, server)
