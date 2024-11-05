# Server calculations

function(input, output, session) {
  
  # choose columns to display
  dm2 = dm[sample(nrow(dm), 10), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(dm2[, input$show_vars, drop = FALSE])
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    data <- ae
    if (input$AESEV != "All") {
      data <- data[data$AESEV == input$AESEV,]
    }
    if (input$AEREL != "All") {
      data <- data[data$AEREL == input$AEREL,]
    }
    DT::datatable(data, options = list(orderClasses = TRUE))
  })
}

#*--- end of program ------------------------------------------------------------------------------;