

#install.packages("jsonlite")
library(jsonlite)
# install.packages("readr")
library(readr)
#install.packages("dplyr")
library(dplyr)

json_str <- readr::read_file("Data/SDTM/dm.json")

data <- jsonlite::fromJSON(json_str, simplifyVector = F)

cols <- c(sapply(data$columns, function(x) x$label))

dm <- data$rows %>% 
  lapply(function(row) {
    data.frame(matrix(unlist(row), nrow = 1), stringsAsFactors = FALSE)
  }) %>%
  dplyr::bind_rows()

names(dm) <- cols





# Read and parse JSON
json_str <- readr::read_file("Data/SDTM/dm.json")
data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

# Extract column names and data types
cols <- c(sapply(data$columns, function(x) x$label))
datatypes <- c(sapply(data$columns, function(x) x$dataType))

# Convert rows to a data frame
ae <- data$rows %>%
  lapply(function(row) {
    data.frame(matrix(unlist(row), nrow = 1), stringsAsFactors = FALSE)
  }) %>%
  dplyr::bind_rows()

# Apply each data type to the respective column using mapply
ae[] <- mapply(function(column, dtype) {
  switch(
    dtype,
    "character" = as.character(column),
    "numeric" = as.numeric(column),
    "integer" = as.integer(column),
    "logical" = as.logical(column),
    column  # Default if no matching type is found
  )
}, ae, datatypes, SIMPLIFY = FALSE)

# Set column names in the data frame
names(ae) <- cols

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

tags$script(HTML(jsCode))

# Function to create column definitions for reactable
createColDef <- function(ae) {
  colDefs <- lapply(names(ae), function(name) {
    # Customize column definitions based on data type
    if (is.numeric(ae[[name]])) {
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
        filterable = TRUE,
        style = list(
          whiteSpace = "nowrap",
          overflow = "hidden",
          textOverflow = "ellipsis"
        )
      )
    }
  })
  # Assign column labels as names
  names(colDefs) <- cols
  colDefs
}





# Generate column definitions
column_definitions <- createColDef(ae)

# Render reactable with column definitions
browsable(
  htmlwidgets::onRender(
    reactable(
      ae,
      filterable = TRUE,
      sortable = TRUE,
      pagination = if (nrow(ae) > 20) TRUE else FALSE,
      borderless = TRUE,
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      resizable = TRUE,
      columns = createColDef(ae)
    ),
    jsCode
  )
)

