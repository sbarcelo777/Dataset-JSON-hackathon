muiDependency <- function() {
  list(
    reactR::html_dependency_react(),
    htmltools::htmlDependency(
      name = "mui",
      version = "5.6.3",
      src = c(href = "https://unpkg.com/@mui/material@5.6.3/umd/"),
      script = "material-ui.production.min.js"
    )
  )
}

# Code JavaScript pour le filtre de plage Material UI
jsCode <- HTML("
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
  const valueLabel = `${value[0]} - ${value[1]}`

  return React.createElement(
    'div',
    { 
      style: { 
        margin: '0 12px',
        padding: '8px 0',
      } 
    },
    [
      React.createElement(
        'div',
        {
          style: {
            fontSize: '0.875rem',
            color: '#666',
            marginBottom: '8px',
            textAlign: 'center'
          }
        },
        valueLabel
      ),
      React.createElement(
        MaterialUI.Slider,
        {
          value: value,
          min: range[0],
          max: range[1],
          valueLabelDisplay: 'auto',
          onChange: (e, val) => column.setFilter(val),
          'aria-label': `Filter ${column.name}`,
          sx: {
            color: '#2C3E50',
            '& .MuiSlider-thumb': {
              height: 20,
              width: 20,
              backgroundColor: '#fff',
              border: '2px solid #2C3E50',
              '&:focus, &:hover, &.Mui-active': {
                boxShadow: '0 0 0 8px rgba(25, 118, 210, 0.25)',
              },
            },
            '& .MuiSlider-valueLabel': {
              backgroundColor: '#2C3E50',
              borderRadius: '6px',
              padding: '4px 8px',
              fontSize: '0.75rem',
            },
            '& .MuiSlider-track': {
              height: 4,
            },
            '& .MuiSlider-rail': {
              height: 4,
              opacity: 0.2,
            },
          }
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
")


auto_text_color <- function(background_color) {
  rgb_color <- col2rgb(background_color)
  luminance <- (0.299 * rgb_color[1] + 0.587 * rgb_color[2] + 0.114 * rgb_color[3]) / 255
  if (luminance > 0.5) {
    return("#000000")  # Black text for light backgrounds
  } else {
    return("#ffffff")  # White text for dark backgrounds
  }
}
