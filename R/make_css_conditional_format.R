#' Get css properties for conditional formatting
#'
#' \code{make_css_conditional_format} will create a list of css properties needed for custom conditional formatting.
#' 
#' \code{make_css_conditional_format} will add conditional css to a tableHTML's columns. \code{add_conditional_css_column} will only
#'   add css to the columns without the headers or second headers (i.e. it only affects the td tag
#'   internally and not the th tag). If you want to add css to the headers or second headers please
#'   use \code{add_css_header} or \code{add_css_second_header}. 
#'
#' @param column_data A named list of vectors of values that are in a tableHTML column which
#'  should be mapped to a colour palette. 
#'
#'   
#' @inheritParams grDevices::colorRampPalette
#' @inheritParams base::order
#' @inheritParams add_css_condtional_column
#'
#' @return A list of css properties 
#'         
#' @examples
#' 
#' tableHTML <- tableHTML(mtcars)
#' 
#' css <- make_css_conditional_format(mtcars$mpg, c("orange", "yellow","springgreen","royalblue"))
#' 
#' tableHTML %>% add_css_conditional_column(conditional_theme = "Custom", css = css, column = 1)
#' 
#' @export

make_css_conditional_format<- function(column_data,
                                       colors,
                                       decreasing = FALSE, 
                                       same_scale = TRUE) {
  
  col_names <- names(column_data)

  cols_context <- switch(ifelse(same_scale, "TRUE", "FALSE"),
                         "TRUE" = function(x) { return(unname(unlist(column_data))) },
                         "FALSE" = function(y) { return(y) }
  )
  
  css <- lapply(column_data, function(cd) {
    
    
    css_colours <- character(length(cd))
    colour_palette <- colorRampPalette(colors)
    
    col_df <- data.frame(value = unique(cols_context(cd))[order(unique(cols_context(cd)), decreasing = decreasing)],
                         colour = colour_palette(length(unique(cols_context(cd)))),
                         stringsAsFactors = FALSE)
    
    css_colours[1:length(cd)] <- 
      vapply(1:length(cd), function(i) {
        
        col_df$colour[which(col_df$value %in%  cd[i])]
        
      }, FUN.VALUE = character(1))
    
    list(c("background-color"), list(c(css_colours)))
    
    
  })
  
  names(css) <- col_names
  
  return(css)
}
  

  

  

  