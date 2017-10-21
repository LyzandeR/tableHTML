is.fachar <- function(x) is.character(x) | is.factor(x)


replace_style <- function(tableHTML, split, style, condition = NULL){
  
  if (is.null(condition)) {
    condition <- rep(TRUE, attr(tableHTML, "nrows"))
  }
  
  tableHTML <- gsub(paste(split, "style="), 
                    paste0(split), tableHTML)
  
  splits <- strsplit(tableHTML, split)

  splits[[1]][2:length(splits[[1]])] <- 
    vapply(splits[[1]][2:length(splits[[1]])], function(x) {
      #check if style should be applied
      
      if (condition[which(splits[[1]] %in% x) - 1]) {
        paste0(split,' ', style[which(splits[[1]] %in% x) - 1], x)
      } else {
        #if not, check if style was applied from previous functions
        if (substr(x, 1, 1) == ">") {
          paste0(split, x)
        } else {
          paste0(split, ' style=', x)
        }
      }
      
    }, FUN.VALUE = character(1))
 
  gsub(';""', ';', paste(splits[[1]], collapse = ''))
 
}

convert_type <- function(v, type, levels) {
  
  switch(type,
         numeric = as.numeric(v),
         integer = as.integer(v),
         factor = if (is.null(levels)) {
           as.factor(v)
         } else {
           factor(v, levels = levels)
         },
         character = as.character(v))
}






