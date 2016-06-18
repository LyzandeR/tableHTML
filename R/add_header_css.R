#' Add css to tableHTML's headers 
#'
#' \code{add_css_header} will add css to a tableHTML's headers
#' 
#' \code{add_css_header} will add css to a tableHTML's headers.
#'
#' @param tableHTML A tableHTML object created by the tableHTML function.
#'
#' @param css A list of two elements with the corresponding css. The first element of the list
#'   should be an atomic vector with the style definitions (e.g. background-color). The second
#'   element will be an atomic vector with the same length as the first element, which will 
#'   contain the style definitions' values (e.g. red). Check the examples for more information.
#'   
#' @param headers A numeric atomic vector with the indices of the headers where the style
#'   definitions will be applied on. At least one header index must be provided.
#'
#' @return A tableHTML object. 
#'         
#' @examples
#' tableHTML(mtcars) %>% 
#'   add_css_header(css = list(c('background-color', 'border'), c('lightgray', '3px solid green')),
#'                  headers = 2) 
#' 
#' tableHTML(mtcars) %>% 
#'   add_css_header(css = list(c('background-color', 'border'), c('lightgray', '3px solid green')), 
#'                  headers = c(1, 4))
#' @export
add_css_header <- function(tableHTML, 
                           css, 
                           headers) {
 
 #checks
 if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
 if (length(css[[1]]) != length(css[[2]])) stop('css needs to be a list of two elements of the
                                                same length') 
 
 tabHTML <- tableHTML
 
 #create style
 css_comp <- paste0(css[[1]], ':', css[[2]], ';')
 css_comp <- paste(css_comp, collapse = '')
 
 style <- paste0('style="', css_comp, '"')
 
 if (grepl('id="header_0"', tabHTML)) {
  
  for (i in (headers - 1)) {
   tabHTML <- gsub(paste0('id="header_', i, '" style='), paste0('id="header_', i, '"'), tabHTML)
   tabHTML <- gsub(paste0('id="header_', i, '"'), paste0('id="header_', i, '" ', style), tabHTML)
   tabHTML <- gsub(';""', ';', tabHTML)
  } 
  
 } else {
 
  for (i in headers) {
   tabHTML <- gsub(paste0('id="header_', i, '" style='), paste0('id="header_', i, '"'), tabHTML)
   tabHTML <- gsub(paste0('id="header_', i, '"'), paste0('id="header_', i, '" ', style), tabHTML)
   tabHTML <- gsub(';""', ';', tabHTML)
  }
  
 }
 
 tabHTML
 
}

