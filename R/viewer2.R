source("R/filterjs.R")
source("R/process_json_file.R")
library(plotly)


# Module 2: JSON Viewer UI
viewerUI2 <- function(id) {
  ns <- NS(id)
  tagList(
    muiDependency(),
    tags$script(jsCode),
    layout_columns(
      col_widths = c(3, 9), 
      layout_column_wrap(
        width = 1,  
        heights_equal = "row",
        card(
          card_header("Filter Options"),
          selectInput(
            ns("file_select"), 
            "Select a dataset",
            choices = NULL,
            width = "100%"),
          textAreaInput(
            inputId = ns("r_filter"),
            label = "Filter data using R expressions",
            placeholder = "e.g., AGE > 30 & STATUS == 'Active'",
            width = "100%",
            rows = 1
          ),
          actionButton(ns("apply_filter"), "Apply Filter", class = "btn-primary mb-3"),
          uiOutput(ns("active_filters")),
        ),
        card(
          full_screen = TRUE,
          height = 350,
          card_header("Variables description"),
          selectInput(
            ns("var_select"), 
            "Select a variable",
            choices = NULL,
            width = "100%"),
          uiOutput(ns("var"))
        )
      ),
      card(
        full_screen = TRUE,
        card_header("View JSON Content"),
        layout_sidebar(
          fillable = TRUE,
          sidebar = sidebar(
            open = FALSE,
            htmlOutput(ns("info")),
            checkboxInput(ns("show_label"), "Show labels", value = FALSE),
            actionButton(ns("hide_window"), "Hide/Unhide columns", 
                         class = "btn-primary mb-3",
                         disabled = TRUE),
            actionButton(ns("sticky"), "Stick/Unstick columns", 
                         class = "btn-primary mb-3",
                         disabled = TRUE)
          ),
          reactableOutput(ns("json_table"))
        )
      )
    )
  )
}




# Module 2: JSON Viewer Server
viewerServer2 <- function(id, uploaded_files) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive value for selected vars to show
    selected_vars <- reactiveVal(NULL)
    
    # Reactive value for selected vars to stick
    selected_vars_s <- reactiveVal(NULL)
    
    # Current filtered dataset
    filtered_data <- reactiveVal(NULL)
    
    # Reactive value for active filters
    r_filters <- reactiveVal(list())
    
    observe({
      if (is.null(uploaded_files()) || length(uploaded_files()) == 0) {
        choices <- NULL
      } else {
        names <- t(sapply(uploaded_files(), function(x) c(name = x$name)))
        labels <- t(sapply(uploaded_files(), function(x) c(label = x$label)))
        choices <- setNames(paste0(names, "-->", labels), paste0(names, " - ", labels))
      }
      updateSelectInput(session, "file_select", 
                        choices = c("Select a file" = "", choices))
    })
    
    #######################################################################################
    
    # Function to get the current dataset
    get_current_dataset <- reactive({
      req(input$file_select)
      
      selected_name_label <- strsplit(input$file_select, "-->")[[1]]
      selected_name <- selected_name_label[1]
      selected_label <- selected_name_label[2]
      
      json_data <- uploaded_files() %>%
        purrr::keep(~ .x$name == selected_name && .x$label == selected_label) %>%
        purrr::pluck(1)

      
      render_df <- as.data.frame(process_json_file(json_data = json_data))
      return(render_df)
    })
      
   
      
    
    #######################################################################################
    
    # INPUT FILE SELECT
    
    # Initialize or update selected columns when dataset changes
    observeEvent(input$file_select, {
      req(input$file_select)
      if(nchar(input$file_select) > 1) {
        if (is.null(selected_vars()[[input$file_select]])) {
          current_data <- get_current_dataset()
          column_init <- names(current_data)
          column_init_s <- if ("USUBJID" %in% names(current_data)) "USUBJID" else NULL
          current_selections <- selected_vars()
          current_selections_s <- selected_vars_s()
          current_selections[[input$file_select]] <- column_init
          current_selections_s[[input$file_select]] <- column_init_s
          selected_vars(current_selections)
          selected_vars_s(current_selections_s)
        }
        filtered_data(get_current_dataset())
        updateActionButton(session, "hide_window", disabled = FALSE)
        updateActionButton(session, "sticky", disabled = FALSE)
        updateSelectInput(session, "var_select", choices = c("Select a variable" = "", names(filtered_data())))
      }
    })
    
    
    #INPUT APLLY FILTER 
    
    # Observer for the R filter button
    observeEvent(input$apply_filter, {
      req(input$r_filter)
      if (nchar(glue::trim(input$r_filter)) > 0) {
        current_filters <- r_filters()
        filter_id <- paste0("filter_", format(Sys.time(), "%Y%m%d%H%M%OS6"))
        current_filters[[filter_id]] <- input$r_filter
        r_filters(current_filters)
        updateTextAreaInput(session, "r_filter", value = "")
      }
    })
    
    # Render active filters
    output$active_filters <- renderUI({
      current_filters <- r_filters()
      if (length(current_filters) == 0) {
        return(NULL)
      }
      
      card(
        card_header("Active Filters"),
        class = "mt-3",
        lapply(names(current_filters), function(filter_id) {
          div(
            class = "d-flex justify-content-between align-items-center mb-2 p-2 border rounded",
            style = "background-color: #f8f9fa;",
            span(current_filters[[filter_id]]),
            actionButton(
              inputId = session$ns(paste0("remove_", filter_id)),
              label = NULL,
              icon = icon("times"),
              class = "btn-sm btn-danger"
            )
          )
        })
      )
    })
    
    # Handle filter removal
    observe({
      current_filters <- r_filters()
      for(filter_id in names(current_filters)) {
        local({
          local_filter_id <- filter_id
          observeEvent(input[[paste0("remove_", local_filter_id)]], {
            current_filters <- r_filters()
            current_filters[[local_filter_id]] <- NULL
            r_filters(current_filters)
          }, ignoreInit = TRUE)
        })
      }
    })
    
    # Apply filters and update data
    filtered_result <- reactive({
      req(get_current_dataset())
      result <- get_current_dataset()
      
      # Apply R expression filters and track invalid ones
      current_filters <- r_filters()
      invalid_filters <- c()
      
      for(filter_id in names(current_filters)) {
        filter_expr <- current_filters[[filter_id]]
        
        # Try to apply the filter
        tryCatch({
          temp_result <- result %>%
            filter(eval(parse(text = filter_expr)))
          # If successful, update the result
          result <- temp_result
        }, error = function(e) {
          #Track invalid
          invalid_filters <- c(invalid_filters, filter_id)
          showNotification(
            paste("Error in filter expression:", e$message),
            type = "error"
          )
        })
        
      }
      
      # Remove invalid filters from r_filters
      if(length(invalid_filters) > 0) {
        current_filters <- current_filters[!names(current_filters) %in% invalid_filters]
        r_filters(current_filters)
      }
      
      result
    })
    
    # Reset filters when dataset changes
    observeEvent(input$file_select, {
      r_filters(list())
      filtered_data(get_current_dataset())
    })
    
    # Update filtered data
    observe({
      filtered_data(filtered_result())
    })
  
    
    # Hide/UnHide
    observeEvent(input$hide_window, {
      
      column_choice <- names(filtered_data()
                             # [5:ncol(filtered_data())]
                             )
      current_selected <- selected_vars()[[input$file_select]]
      
      showModal(modalDialog(
        title = "Sélectionner les colonnes",
        div(
          style = "margin-bottom: 15px",
          actionButton(ns("selectAll"), "Select All"),
          actionButton(ns("unselectAll"), "Unselect All")
        ),
        div(
          style = "max-height: 400px; overflow-y: auto;",
          checkboxGroupInput(
            ns("columns"),
            label = NULL,
            choices = names(filtered_data()),
            selected = current_selected
          )
        ),
        footer = tagList(
          # actionButton(ns("X"), "X")
          modalButton("X")
        ),
        size = "m"
      ))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)  # Important!
    
    # Gérer Select All
    observeEvent(input$selectAll, {
      updateCheckboxGroupInput(session,
                               inputId = "columns",
                               choices = names(filtered_data()),
                               selected = names(filtered_data())
      )
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Gérer Unselect All
    observeEvent(input$unselectAll, {
      updateCheckboxGroupInput(session,
                               inputId = "columns",
                               choices = names(filtered_data()),
                               selected = character(0)
      )
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      
      # Update selected columns
      observeEvent(input$columns, {
        req(input$file_select)
        current_selections <- selected_vars()
        current_selections[[input$file_select]] <- input$columns
        selected_vars(current_selections)
      })
      
    # })
    
    
    
    # REACTABLE
    output$json_table <- renderReactable({
      req(filtered_data(), input$file_select)
      
      ############################################# COL DEF FUNCTION
      createColDef <- function(data) {
        
        column_labels <- attr(data, "labels")
        label_list <- if (is.null(column_labels)) {
          list()
        } else if (is.vector(column_labels)) {
          as.list(column_labels)
        } else {
          column_labels
        }
        
        col_sticky <- selected_vars_s()[[input$file_select]]
        
        colDefs <- lapply(names(data), function(name) {
          
          if (is.numeric(data[[name]])) {
            if (sum(!is.na(data[[name]])) > 1) {
              colDef(
                header = if (input$show_label) label_list[[name]] else name,
                filterable = TRUE,
                filterMethod = JS("filterRange"),
                filterInput = JS("muiRangeFilter"),
                sticky = if(name %in% col_sticky) "left",
                style = list(
                  whiteSpace = "nowrap",
                  overflow = "hidden",
                  textOverflow = "ellipsis"
                )
              )
            }
          }else {
            
            colDef(
              header =  if (input$show_label) label_list[[name]] else name,
              sticky = if(name %in% col_sticky) "left",
              filterable = TRUE,
              style = list(
                whiteSpace = "nowrap",
                overflow = "hidden",
                textOverflow = "ellipsis"
              )
            )
          }
        })
        names(colDefs) <- names(data)
        colDefs
      }
      #################################################################
      
      selected_cols <- selected_vars()[[input$file_select]]
      
      reactable(filtered_data()[,selected_cols],
                 filterable = TRUE,
                 sortable = TRUE,
                 pagination = if (nrow(filtered_data()) > 20) TRUE else FALSE,
                 borderless = TRUE,
                 striped = TRUE,
                 highlight = TRUE,
                 compact = TRUE,
                 resizable = TRUE,
                 columns = createColDef(filtered_data()),
                 defaultColDef = colDef(align = 'center', vAlign = "center", minWidth = 150),
                 theme = reactableTheme(
                   borderColor = "#dfe2e5",
                   stripedColor = "#f6f8fa",
                   highlightColor = "#f0f5f9",
                   cellPadding = "8px 12px"
                 ),
                 defaultPageSize = 30
      )
    })
    
    
    # Hide/UnHide
    observeEvent(input$sticky, {
      
      column_choice_s <- names(filtered_data())
      current_selected_s <- selected_vars_s()[[input$file_select]]
      
      showModal(
        modalDialog(
          title = "Column Visibility",
          div(
            div(
              style = "margin-bottom: 15px;",
              checkboxInput(ns("select_all_s"),
                            "Select/Deselect All",
                            value = length(current_selected_s) == length(column_choice_s)
              )
            ),
            div(
              style = "max-height: 400px; overflow-y: auto;",
              checkboxGroupInput(
                ns("columns_vars_s"),
                label = NULL,
                choices = column_choice_s,
                selected = current_selected_s
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
    
    observeEvent(input$select_all_s,{
      column_choice_s <- names(filtered_data())
      if (input$select_all_s) {
        updateCheckboxGroupInput(session, "columns_vars_s",
                                 choices = column_choice_s,
                                 selected = column_choice_s)
      } else {
        updateCheckboxGroupInput(session, "columns_vars_s",
                                 choices = column_choice_s,
                                 selected = NULL)
      }
    }, ignoreInit = TRUE)
    
    
    
    # Update selected columns
    observeEvent(input$columns_vars_s, {
      req(input$file_select)
      current_selections_s <- selected_vars_s()
      current_selections_s[[input$file_select]] <- input$columns_vars_s
      selected_vars_s(current_selections_s)
    })
    
    # UI Function
    output$var <- renderUI({
      req(filtered_data())
      req(input$file_select)
      
      # Verify var_select is valid
      if (is.null(input$var_select) || !input$var_select %in% names(filtered_data())) {
        return(NULL)
      }
      
      var <- filtered_data()[[input$var_select]]
      
      #Cat variables available
      cat_vars <- names(filtered_data())[sapply(filtered_data(), function(x) !is.numeric(x))]
      cat_vars <- setdiff(cat_vars, input$var_select)
      info <- getCurrentOutputInfo()
      ex_info <<- info
      
      if (is.numeric(var)) {
        
        # Create unique ID
        plot_id <- paste0("plot_", input$file_select)
        
        output[[plot_id]] <- renderPlotly({
          info <- getCurrentOutputInfo()
          
          plot_data <- filtered_data()
          # plot_data$var[plot_data$var == "" | is.na(plot_data$var)] <- NA
          
          if (input$check_group_l && !is.null(input$var_group_l) && 
              input$var_group_l %in% names(plot_data)) {
            
            group_var <- plot_data[[input$var_group_l]]
            group_var[group_var == "" | is.na(group_var)] <- NA
            
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
                showlegend = info$fullscreen,
                margin = list(t = 50)
              ) %>%
              config(displayModeBar = FALSE)
            
          } else {
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
            ) %>%
              config(displayModeBar = FALSE)
          }
          
          return(fig)
        })
        

        # UI 
        tagList(
          layout_columns(
            col_widths = c(6, 6),
            checkboxInput(ns("check_group_l"), "Add grouping variable", value = FALSE),
            conditionalPanel(
              condition = "input.check_group_l == true",
              ns = NS(id),
              selectInput(ns("var_group_l"),
                          "Select grouping variable",
                          choices = cat_vars,
                          selected = NULL)
            )
          ),
          plotlyOutput(ns(plot_id))
        )
        
      } else {
        # Create unique ID
        table_id <- paste0("freq_table_", input$file_select)
        
        # Create frequency table
        output[[table_id]] <- renderReactable({
          
          if (input$check_group && !is.null(input$var_group) && 
              input$var_group %in% names(filtered_data())) {
            # Cross tab
            cross_tab <- table(filtered_data()[[input$var_select]], 
                               filtered_data()[[input$var_group]])
            tab_df <- as.data.frame.matrix(cross_tab)
          } else {
            # Simple tab
            tab_df <- as.data.frame(table(var))
            names(tab_df) <- c("Category", "Count")
          }
          
          reactable(tab_df, 
                    defaultPageSize = 10,
                    striped = TRUE,
                    highlight = TRUE)
        })
        
        # UI 
        tagList(
          layout_columns(
            col_widths = c(6, 6),
            checkboxInput(ns("check_group"), "Add grouping variable", value = FALSE),
            conditionalPanel(
              condition = "input.check_group == true",
              ns = NS(id),
              selectInput(ns("var_group"),
                          "Select grouping variable",
                          choices = cat_vars,
                          selected = NULL)
            )
          ),
          reactableOutput(ns(table_id))
        )
      }
    })
    
    
  })
}