#' Add css to tableHTML's second headers 
#'
#' \code{add_css_second_header} will add css to a tableHTML's second headers
#' 
#' \code{add_css_second_header} will add css to a tableHTML's second headers.
#'
#' @param tableHTML A tableHTML object created by the tableHTML function.
#'
#' @param css A list of two elements with the corresponding css. The first element of the list
#'   should be an atomic vector with the style definitions (e.g. background-color). The second
#'   element will be an atomic vector with the same length as the first element, which will 
#'   contain the style definitions' values (e.g. red). Check the examples for more information.
#'   
#' @param second_headers A numeric atomic vector with the indices of the second headers where the 
#'   style definitions will be applied on. At least one second header index must be provided.
#'
#' @return A tableHTML object. 
#'         
#' @examples
#' tableHTML(mtcars, second_header = list(c(3, 4, 5), c('col1', 'col2', 'col3'))) %>% 
#'   add_css_second_header(css = list(c('background-color', 'border'), 
#'                                    c('lightgray', '3px solid green')),
#'                         second_headers = c(1, 3))  
#' 
#' @export
add_css_second_header <- function(tableHTML, 
                                  css, 
                                  second_headers) {
 
 #checks
 if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
 if (length(css[[1]]) != length(css[[2]])) stop('css needs to be a list of two elements of the
                                                same length') 
 
 attributes <- attributes(tableHTML)
 
 #create style
 css_comp <- paste0(css[[1]], ':', css[[2]], ';')
 css_comp <- paste(css_comp, collapse = '')
 
 style <- paste0('style="', css_comp, '"')
 
 for (i in second_headers) {
  tableHTML <- gsub(paste0('id="tableHTML_second_header_', i, '" style='), 
                    paste0('id="tableHTML_second_header_', i, '"'),
                  tableHTML)
  tableHTML <- gsub(paste0('id="tableHTML_second_header_', i, '"'), 
                    paste0('id="tableHTML_second_header_', i, '" ', style),
                  tableHTML)
  tableHTML <- gsub(';""', ';', tableHTML)
 }
 
 attributes(tableHTML) <- attributes
 
 tableHTML
 
}

