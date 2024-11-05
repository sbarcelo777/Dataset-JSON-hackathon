
###############################################################################;
# PROJECT/PATH : <PROGRAM_PATH>
# PROGRAM NAME : <PROG_NAME>
# CREATION DATE: 11/15/2021 (mm/dd/yyyy)
# PROGRAMMER   : Sebastià Barceló Bauzà (user name)
# PURPOSE      : Instructions for data manipulation.
#                NOTE: R is key sensitive regarding to the name of the variables
###############################################################################;
# Modifications History:
#------------------------------------------------------------------------------;
# Modif. #01   :
# Programmer   :
# Date         :
# Purpose      :
#------------------------------------------------------------------------------;
# Modif. #0n   :
# Programmer   :
# Date         :
# Purpose      :
###############################################################################;



# Define UI

fluidPage(theme = shinytheme("united"),
          navbarPage(
            # theme = "cerulean",  # <--- To use a theme, uncomment this
            "Data Visualization",
            tabPanel("Datasets",
                     sidebarLayout(
                       sidebarPanel(
                         conditionalPanel(
                           'input.dataset === "dm"',
                           checkboxGroupInput("show_vars", "Columns in DM to show:",
                                              names(dm), selected = names(dm))
                         ),
                         conditionalPanel(
                           'input.dataset === "ae"',
                           selectInput("AESEV",
                                       "Severity:",
                                       c("All",
                                         unique(as.character(ae$AESEV)))),
                           selectInput("AEREL",
                                       "Relationship:",
                                       c("All",
                                         unique(as.character(ae$AEREL))))
                         ),
                       ),
                       mainPanel(
                         tabsetPanel(
                           id = 'dataset',
                           tabPanel("dm", DT::dataTableOutput("mytable1")),
                           tabPanel("ae", DT::dataTableOutput("mytable2"))
                         )
                       )
                     )
                     
            )
          ) # navbarPage  
          
)


#*--- end of program ------------------------------------------------------------------------------;