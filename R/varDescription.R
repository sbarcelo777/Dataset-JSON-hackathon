# 3. var_description_module.R
varDescriptionUI <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    height = 650,
    pickerInput(
      ns("var_select"), 
      label = NULL,
      choices = NULL,
      width = "100%"
    ),
    uiOutput(ns("var"))
  )
}

varDescriptionServer <- function(id, filtered_data, selected_file) {
  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(show_outputs = TRUE)
    
    observeEvent(selected_file(), {
      rv$show_outputs <- FALSE
    })
    
    # Update variable choices
    observe({
      req(filtered_data())
      updatePickerInput(session, "var_select",
                        choices = c("Select a variable" = "", names(filtered_data())))
    })
    
    # Reset show_outputs when a variable is selected
    observeEvent(input$var_select, {
      if (input$var_select != "") {
        rv$show_outputs <- TRUE
      } else {
        rv$show_outputs <- FALSE
      }
    })
    
    # Render variable description
    output$var <- renderUI({
      req(filtered_data())
      req(input$var_select)
      
      # Get the selected variable
      var <- filtered_data()[[input$var_select]]

      if(is.null(var)) return(NULL)
      if(!rv$show_outputs) return(NULL)  # Return NULL if outputs should be hidden
      
      # Get categorical variables for grouping
      cat_vars <- names(filtered_data())[sapply(filtered_data(), function(x) !is.numeric(x))]
      cat_vars <- setdiff(cat_vars, input$var_select)
      
      if (is.numeric(var)) {
        # Numeric variable handling
        plot_id <- paste0("plot_", input$var_select)
        
        # Create plot
        output[[plot_id]] <- renderPlotly({
          req(filtered_data())
          plot_data <- filtered_data()
          
          # Create box plot with or without grouping
          if (input$check_group_l && !is.null(input$var_group_l) && 
              input$var_group_l %in% names(plot_data)) {
            
            # Handle NA and empty values in grouping variable
            group_var <- plot_data[[input$var_group_l]]
            group_var[group_var == "" | is.na(group_var)] <- NA
            
            # Grouped box plot
            fig <- plot_ly(
              data = plot_data,
              y = ~var,
              color = ~group_var,
              type = "box",
              boxpoints = FALSE
            ) %>%
              layout(
                title = list(
                  text = paste("Distribution of", input$var_select, 
                               "by", input$var_group_l),
                  font = list(size = 16)
                ),
                xaxis = list(
                  title = input$var_group_l,
                  tickfont = list(size = 12),
                  titlefont = list(size = 14)
                ),
                yaxis = list(
                  title = input$var_select,
                  tickfont = list(size = 12),
                  titlefont = list(size = 14)
                ),
                showlegend = TRUE,
                margin = list(t = 50)
              )
            
          } else {
            # Single box plot
            fig <- plot_ly(
              data = plot_data,
              y = ~var,
              type = "box",
              boxpoints = FALSE,
              marker = list(color = '#3366CC')
            ) %>%
              layout(
                title = list(
                  text = paste("Distribution of", input$var_select),
                  font = list(size = 16)
                ),
                xaxis = list(
                  title = "",
                  showticklabels = FALSE
                ),
                yaxis = list(
                  title = input$var_select,
                  tickfont = list(size = 12),
                  titlefont = list(size = 14)
                ),
                margin = list(t = 50)
              )
          }
          
          fig %>% config(displayModeBar = FALSE)
        })
        
        # Return UI for numeric variable
        tagList(
          layout_columns(
            col_widths = c(6, 6),
            checkboxInput(session$ns("check_group_l"), 
                          "Add grouping variable", 
                          value = FALSE),
            conditionalPanel(
              condition = "input.check_group_l == true",
              ns = session$ns,
              pickerInput(session$ns("var_group_l"),
                          "Select grouping variable",
                          choices = cat_vars,
                          selected = NULL)
            )
          ),
          plotlyOutput(session$ns(plot_id))
        )
        
      } else {
        # Categorical variable handling
        table_id <- paste0("freq_table_", input$var_select)
        
        # Create frequency table
        output[[table_id]] <- renderReactable({
          req(filtered_data())
          if (input$check_group && !is.null(input$var_group) && 
              input$var_group %in% names(filtered_data())) {
            # Cross tabulation
            cross_tab <- table(filtered_data()[[input$var_select]], 
                               filtered_data()[[input$var_group]])
            tab_df <- as.data.frame.matrix(cross_tab)
            
            # Add row totals
            tab_df$Total <- rowSums(tab_df)
            
            # Add column totals
            total_row <- colSums(tab_df)
            tab_df <- rbind(tab_df, Total = total_row)
            
          } else {
            # Simple frequency table
            tab_df <- as.data.frame(table(var))
            names(tab_df) <- c("Category", "Count")
            
            # Add percentage column
            tab_df$Percentage <- sprintf("%.1f%%", 
                                         100 * tab_df$Count / sum(tab_df$Count))
          }
          
          reactable(
            tab_df,
            defaultPageSize = 10,
            striped = TRUE,
            highlight = TRUE,
            bordered = TRUE,
            compact = TRUE,
            defaultColDef = colDef(
              align = "center",
              headerStyle = list(background = "#f7f7f8")
            )
          )
        })
        
        # Return UI for categorical variable
        tagList(
          layout_columns(
            col_widths = c(6, 6),
            checkboxInput(session$ns("check_group"), 
                          "Add grouping variable", 
                          value = FALSE),
            conditionalPanel(
              condition = "input.check_group == true",
              ns = session$ns,
              pickerInput(session$ns("var_group"),
                          "Select grouping variable",
                          choices = cat_vars,
                          selected = NULL)
            )
          ),
          reactableOutput(session$ns(table_id))
        )
      }
    })

    })
}