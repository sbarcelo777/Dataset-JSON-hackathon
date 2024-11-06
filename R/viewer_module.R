library(htmltools)
library(reactR)
library(shinyWidgets)

# Module 2: JSON Viewer UI
viewerUI <- function(id) {
  ns <- NS(id)
  tagList(
    muiDependency(),
    tags$script(jsCode),
    card(
      full_screen = TRUE,
      card_header("View JSON Content"),
      selectInput(ns("file_select"), "Select a dataset", choices = NULL),
      # muiDependency(),
      # tags$script(jsCode),
      reactableOutput(ns("json_table"))
    )
  )
}

muiDependency <- function() {
  list(
    reactR::html_dependency_react(),
    htmlDependency(
      name = "mui",
      version = "5.6.3",
      src = c(href = "https://unpkg.com/@mui/material@5.6.3/umd/"),
      script = "material-ui.production.min.js"
    )
  )
}

# Code JavaScript pour le filtre de plage Material UI
jsCode <- HTML('
const muiRangeFilter = (column, state) => {
  const range = React.useMemo(() => {
    let min = Infinity
    let max = -Infinity
    state.data.forEach(row => {
      const value = row[column.id]
      if (value < min) {
        min = Math.floor(value)
      } else if (value > max) {
        max = Math.ceil(value)
      }
    })
    return [min, max]
  }, [state.data])

  const value = column.filterValue ? column.filterValue : range
  const valueLabel = `${value[0]}...${value[1]}`

  return React.createElement(
    "div",
    { style: { margin: "0 8px" } },
    [
      valueLabel,
      React.createElement(
        MaterialUI.Slider,
        {
          value: value,
          min: range[0],
          max: range[1],
          valueLabelDisplay: "auto",
          onChange: (e, val) => column.setFilter(val),
          "aria-label": `Filter ${column.name}`
        }
      )
    ]
  )
}

const filterRange = (rows, columnId, filterValue) => {
  const [min, max] = filterValue
  return rows.filter(row => {
    const value = row.values[columnId]
    return value >= min && value <= max
  })
}

const muiExcelStyleFilterInHeader = (column, state) => {
  const uniqueValues = Array.from(new Set(state.data.map(row => row[column.id])));
  const [anchorEl, setAnchorEl] = React.useState(null);
  const [selectedValues, setSelectedValues] = React.useState(column.filterValue || []);

  const handleMenuOpen = (event) => {
    setAnchorEl(event.currentTarget);
  };

  const handleMenuClose = () => {
    setAnchorEl(null);
  };

  const handleSelectAll = () => {
    setSelectedValues(uniqueValues);
    column.setFilter(uniqueValues);
  };

  const handleUnselectAll = () => {
    setSelectedValues([]);
    column.setFilter([]);
  };

  const handleSelectValue = (value) => {
    const newSelected = selectedValues.includes(value)
      ? selectedValues.filter(item => item !== value)
      : [...selectedValues, value];

    setSelectedValues(newSelected);
    column.setFilter(newSelected);
  };

  const handleClickOutside = (event) => {
    if (anchorEl && !anchorEl.contains(event.target) && !event.target.closest("[data-filter-button]")) {
      handleMenuClose();
    }
  };

  React.useEffect(() => {
    document.addEventListener("click", handleClickOutside);

    return () => {
      document.removeEventListener("click", handleClickOutside);
    };
  }, [anchorEl]);

  return React.createElement(
    "div",
    { style: { display: "flex", flexDirection: "column", alignItems: "center", cursor: "pointer" }, onClick: handleMenuOpen },
    [
      React.createElement(
        "div",
        { style: { display: "flex", alignItems: "center", cursor: "pointer" }, onClick: handleMenuOpen, "data-filter-button": true },
        [
          React.createElement("span", { style: { fontSize: "12px", marginLeft: "8px" } }, "▼")
        ]
      ),
      React.createElement(
        MaterialUI.Menu,
        {
          anchorEl: anchorEl,
          open: Boolean(anchorEl),
          onClose: handleMenuClose
        },
        [
          React.createElement(
            MaterialUI.MenuItem,
            {
              onClick: handleSelectAll,
              style: { display: "flex", justifyContent: "space-between" }
            },
            [
              "Select All",
              React.createElement(MaterialUI.Checkbox, { checked: selectedValues.length === uniqueValues.length })
            ]
          ),
          React.createElement(
            MaterialUI.MenuItem,
            {
              onClick: handleUnselectAll,
              style: { display: "flex", justifyContent: "space-between" }
            },
            [
              "Unselect All",
              React.createElement(MaterialUI.Checkbox, { checked: selectedValues.length === 0 })
            ]
          ),
          ...uniqueValues.map(value =>
            React.createElement(
              MaterialUI.MenuItem,
              {
                key: value,
                onClick: () => handleSelectValue(value),
                style: { display: "flex", justifyContent: "space-between" }
              },
              [
                value,
                React.createElement(MaterialUI.Checkbox, {
                  checked: selectedValues.includes(value)
                })
              ]
            )
          )
        ]
      )
    ]
  );
};

const filterExcelStyleInHeader = (rows, columnId, filterValues) => {
  return rows.filter(row => {
    const value = row.values[columnId];
    return filterValues.length === 0 || filterValues.includes(value);
  });
};

')

# Définir une fonction pour créer la définition des colonnes
createColDef <- function(data) {
  colDefs <- lapply(names(data), function(name) {
  #   if (name == "Unique Subject Identifier") {
  #     colDef(
  #       sticky = "left",
  #       style = list(
  #         whiteSpace = "nowrap",
  #         overflow = "hidden",
  #         textOverflow = "ellipsis"
  #       )
  #     )
  #   } else
      if (is.numeric(data[[name]])) {
      if (sum(!is.na(data[[name]])) > 1) {
        colDef(
          filterable = TRUE,
          filterMethod = JS("filterRange"),
          filterInput = JS("muiRangeFilter"),
          style = list(
            whiteSpace = "nowrap",
            overflow = "hidden",
            textOverflow = "ellipsis"
          )
        )
      } else {
        colDef(
          filterable = FALSE,
          style = list(
            whiteSpace = "nowrap",
            overflow = "hidden",
            textOverflow = "ellipsis"
          )
        )
      }
    } else if (is.character(data[[name]])) {  # For string columns
      colDef(
        filterable = TRUE,
        filterMethod = JS("filterExcelStyleInHeader"),
        filterInput = JS("muiExcelStyleFilterInHeader"),
        style = list(
          whiteSpace = "nowrap",
          overflow = "hidden",
          textOverflow = "ellipsis"
        )
      )
    } else {
      colDef(
        cell = JS("function(cellInfo) {
          return cellInfo.value === 'Y' ? '\u2714\ufe0f' : (cellInfo.value === 'N' ? '\u274c' : cellInfo.value);
        }"),
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


# Module 2: JSON Viewer Server
viewerServer <- function(id, uploaded_files) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      # files <- names(uploaded_files())
      # labels <- t(sapply(uploaded_files(), function(x) c(label = x$label)))
      # updateSelectInput(session, "file_select", 
      #                   choices = c("Select a file" = "", labels))
      
      names <- t(sapply(uploaded_files(), function(x) c(name = x$name)))
      labels <- t(sapply(uploaded_files(), function(x) c(label = x$label)))
      
      # Concatenate name and label for the selectInput choices
      choices <- setNames(paste0(names, "-", labels), paste0(names, " - ", labels))
  
      updateSelectInput(session, "file_select", 
                        choices = c("Select a file" = "", choices))
      
    })
    
    output$json_table <- renderReactable({
      req(input$file_select)
      
      # json_data <- uploaded_files() %>%
      #   purrr::keep(~ .x$label == input$file_select) %>%
      #   purrr::pluck(1)
      
      # Split the selected input back to `name` and `label`
      selected_name_label <- strsplit(input$file_select, "-")[[1]]
      selected_name <- selected_name_label[1]
      selected_label <- selected_name_label[2]
      
      # Filter the correct dataset based on name and label
      json_data <- uploaded_files() %>%
        purrr::keep(~ .x$name == selected_name && .x$label == selected_label) %>%
        purrr::pluck(1)

      
      cols <- c(sapply(json_data$columns, function(x) x$label))
      datatypes <- c(sapply(json_data$columns, function(x) x$dataType))
      
      render_df <- json_data$rows %>% 
        lapply(function(row) {
          data.frame(matrix(unlist(row), nrow = 1), stringsAsFactors = FALSE)
        }) %>%
        dplyr::bind_rows()
      
      render_df[] <- mapply(function(column, dtype) {
        switch(
          dtype,
          "character" = as.character(column),
          "numeric" = as.numeric(column),
          "integer" = as.integer(column),
          "logical" = as.logical(column),
          column  # Default if no matching type is found
        )
      }, render_df, datatypes, SIMPLIFY = FALSE)

      
      names(render_df) <- cols
      
      
        reactable(render_df,
                filterable = TRUE,
                sortable = TRUE,
                pagination = if (nrow(render_df) > 20) TRUE else FALSE,
                borderless = TRUE,
                striped = TRUE,
                highlight = TRUE,
                compact = TRUE,
                resizable = TRUE,
                columns = createColDef(render_df),
                defaultColDef = colDef(align = 'center', vAlign = "center", minWidth = 150),
                theme = reactableTheme(
                  borderColor = "#dfe2e5",
                  stripedColor = "#f6f8fa",
                  highlightColor = "#f0f5f9",
                  cellPadding = "8px 12px"
                ),
                defaultPageSize = 20
        )
      
      

      # if(is.data.frame(json_data)) {
      #   datatable(json_data)
      # } else if(is.list(json_data)) {
      #   # Convert list to data frame if possible
      #   df <- as.data.frame(json_data)
      #   datatable(df)
      # }
    })
    
    
  })
}
