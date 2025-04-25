
#Resolve applicable
resolve_applicable <- function(applicable, all_names) {
  if (is.null(applicable)) return(all_names)
  exclusions <- grep("^\\\\", applicable, value = TRUE)
  inclusions <- setdiff(applicable, exclusions)
  
  if (length(exclusions)) {
    excluded_names <- sub("^\\\\", "", exclusions)
    return(setdiff(all_names, excluded_names))
  } else {
    return(inclusions)
  }
}




check_table <- function(results_data) {
  
  reactable::reactable(results_data, 
                       columns = list(
                         Result = colDef(name = "",
                                         width = 50,
                                         cell = function(value) {
                                           if (value) {
                                             tagList(
                                               span(
                                                 fa_i("circle-check", fill = "green")
                                               )
                                             )
                                           } else {
                                             tagList(
                                               span(
                                                 fa_i("circle-xmark", fill = "red")
                                               )
                                             )
                                           }
                                         }),
                         
                         Check_Id = colDef(name = "", 
                                           width = 150),
                         
                         Check_Description = colDef(name = "Description",
                                                    width = 400),
                         
                         CHeckMessage = colDef(name = "Details"),
                         
                         Failed_Datasets = colDef(show = FALSE)
                       ),
                       
                       highlight = TRUE,
                       outlined = FALSE,
                       bordered = FALSE,
                       borderless = FALSE,
                       striped = TRUE,
                       compact = TRUE,
                       resizable = TRUE,
                       
                       details = function(index) {
                         fails <- results_data$Failed_Datasets[[index]]
                         if (length(fails) == 0) return(NULL)
                         
                         # Create a data frame from the list of failures
                         detail_df <- data.frame(
                           Dataset = names(fails),
                           Issue = unlist(fails),
                           stringsAsFactors = FALSE
                         )
                         
                         # Return a nested reactable with the details
                         reactable(
                           detail_df,
                           columns = list(
                             Dataset = colDef(name = "Dataset", minWidth = 150),
                             Issue = colDef(name = "Issue Description", minWidth = 300)
                           ),
                           fullWidth = TRUE,
                           borderless = TRUE,
                           outlined = TRUE,
                           pagination = FALSE,
                           minRows = 1
                         )
                       },
                       
                       rowStyle = JS("
                       function(rowInfo, state) {
                         if (rowInfo) {
                           if (rowInfo.row.Result === true) {
                             return { backgroundColor: '#e6ffe6' };  // light green
                           } else {
                             return { backgroundColor: '#ffe6e6' };  // light red
                           }
                         }
                         return {};
                       }
                     ")
  )
}