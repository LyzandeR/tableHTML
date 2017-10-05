extract_column_data <- function(tableHTML, indices, levels) {
  
  # checks
  if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
  
  if (suppressWarnings(any(is.na(as.numeric(indices))))) stop('indices have to be numeric')
  
  col_classes <- attr(tableHTML, 'col_classes')
  #col_classes <- unname(sapply(mtcars, function(x) class(x)))
  data_cols <- list()
  
  for (i in indices) {
    
    if (identical(i, 0)) {
      
      split_cols <- strsplit(tableHTML, "</td>")
      
      data_col <- 
        unlist(lapply(split_cols[[1]][1:length(split_cols[[1]])], function(x) {
          
          if (grepl('id="tableHTML_rownames"', x)) {
            return(substr(x,
                          max(unlist(gregexpr(">", x))) + 1,
                          nchar(x)))
          }
          
        }))
      
      data_cols <- append(data_cols, list(data_col))
      names(data_cols)[length(data_cols)] <- "rownames"
      
    } else if (identical(i, -1)) {
      
      split_cols <- strsplit(tableHTML, "</td>")
      
      data_col <- 
        unlist(lapply(split_cols[[1]][1:length(split_cols[[1]])], function(x) {
          
          if (grepl('id="tableHTML_row_groups"', x)) {
            return(substr(x,
                          max(unlist(gregexpr(">", x))) + 1,
                          nchar(x)))
          }
          
        }))
      
      data_cols <- append(data_cols, list(data_col))
      names(data_cols)[length(data_cols)] <- "row_groups"
      
    } else {
      
      
      split_cols <- strsplit(tableHTML, "</td>")
      
      data_col <- 
        unlist(lapply(split_cols[[1]][1:length(split_cols[[1]])], function(x) {
          
          if (grepl(paste0('id="tableHTML_column_', i, '"'), x)) {
            
            return(substr(x, 
                          max(unlist(gregexpr(">", x))) + 1,
                          nchar(x)))
            
          }
        }))
      
      data_col <- convert_type(data_col, type = col_classes[i], levels = levels)
      
      data_cols <- append(data_cols, list(data_col))
      names(data_cols)[length(data_cols)] <- attr(tableHTML, "headers")[i]
      
      
}
    
  }
  
  # data_cols <- lapply(data_cols, function(data_col) {
  #   if (suppressWarnings(!any(is.na(as.numeric(data_col))))) {
  #     as.numeric(data_col)
  #   } else {
  #     data_col
  #   }
  # })
  return(data_cols)
}

