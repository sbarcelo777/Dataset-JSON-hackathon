viewerUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    muiDependency(),
    tags$script(jsCode),
    theme = bs_theme(
      version = 5,
      primary = "#2C3E50",  # Une couleur bleu foncé élégante
      "card-bg" = "#000000",  # Black background for cards
      "card-color" = "#ffffff", 
    ),
    muiDependency(),
    tags$script(jsCode),
    layout_columns(
      col_widths = c(3, 9),
      fill = FALSE,
      navset_card_tab(
        height = 450,
        # title = "Viewer Tool",
        nav_panel(
          "Data Manipulation",
          card_title("Chose/Filter Data"),
          filterUI(ns("filter")),
        ),
        nav_panel(
          "Variable browser",
          card_title("Variables description"),
          varDescriptionUI(ns("var_desc"))
          
        ),
        nav_spacer(),
        nav_panel(
          shiny::icon("circle-info"),
          markdown(
            "
#### Data Filtering and Analysis Panel

This panel provides all the controls you need to filter and analyze your data. Start by selecting your dataset and applying filters using R syntax.  
**Note:** Only the filters you define here will be applied when downloading the data.

---

### Tips for Using the Filtering Tool

Follow these simple tips to ensure smooth and error-free filtering:

- **Comparison:** Use `==` for comparing values (e.g., `VAR == \"Value\"`).
- **Variable Names:** Match variable names exactly, including capitalization.
- **Logical Operators:**
  - Use `&&` for \"AND\" conditions.
  - Use `||` for \"OR\" conditions.
  - Use `!` for \"NOT\" conditions.
- **Text Values:** Always wrap text values in double quotes (e.g., `\"Value\"`).
- **Numeric Conditions:** Write numeric comparisons properly (e.g., `VARNUM < 78`).
- **Review Filters:** Double-check that your filters make sense for your data to avoid unexpected results.

Taking a moment to review your filters before running them can save you time and ensure accurate results!
"
          )
        )
      ),
        settingsUI(ns("settings"))
             )
    )
}





# 4. Viewer Server 2
viewerServer <- function(id, uploaded_files) {
  moduleServer(id, function(input, output, session) {
    # Initialize filter module
    filter_results <- filterServer("filter", uploaded_files)
  
    # Initialize variable description module
    varDescriptionServer("var_desc",
                         filter_results$filtered_data,
                         filter_results$selected_file)
    
    settingsServer("settings",
                   filter_results$filtered_data,
                   filter_results$selected_file)
    
    # Return values that might be needed by parent module
    return(
      list(
        filtered_data = filter_results$filtered_data,
        selected_file = filter_results$selected_file
       
      )
    )
  })
}
