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
')

createColDef <- function(data) {
  colDefs <- lapply(names(data), function(name) {
    # if (name == "Unique Subject Identifier") {
    #   colDef(
    #     sticky = "left",
    #     style = list(
    #       whiteSpace = "nowrap",
    #       overflow = "hidden",
    #       textOverflow = "ellipsis"
    #     )
    #   )
    # } else 
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
    } 
    # else {
    #   colDef(
    #     cell = JS("function(cellInfo) {
    #       return cellInfo.value === 'Y' ? '\u2714\ufe0f' : (cellInfo.value === 'N' ? '\u274c' : cellInfo.value);
    #     }"),
    #     style = list(
    #       whiteSpace = "nowrap",
    #       overflow = "hidden",
    #       textOverflow = "ellipsis"
    #     )
    #   )
    # }
  })
  names(colDefs) <- names(data)
  colDefs
}