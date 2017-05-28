#' Add css to the thead tag 
#'
#' \code{add_css_thead} will add css to the thead tag i.e. to headers and second_headers.
#' 
#' \code{add_css_thead} will add css to the thead tag i.e. to headers and second_headers.
#'
#' @param tableHTML A tableHTML object created by the tableHTML function.
#'
#' @param css A list of two elements with the corresponding css. The first element of the list
#'   should be an atomic vector with the style definitions (e.g. background-color). The second
#'   element will be an atomic vector with the same length as the first element, which will 
#'   contain the style definitions' values (e.g. red). Check the examples for more information.
#'
#' @return A tableHTML object. 
#'         
#' @examples
#' tableHTML(mtcars) %>% 
#'   add_css_thead(css = list('background-color', 'lightgray')) 
#'   
#' tableHTML(mtcars) %>% 
#'   add_css_thead(css = list('background-color', 'lightgray')) %>%
#'   add_css_thead(css = list('background-color', 'lightblue')) 
#'   
#' tableHTML(mtcars) %>% 
#'   add_css_thead(css = list('background-color', 'lightgray')) %>%
#'   add_css_thead(css = list('text-align', 'center'))
#'   
#' 
#' @export
add_css_thead <- function(tableHTML, 
                          css) {
 
 #checks
 if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
 if (length(css[[1]]) != length(css[[2]])) stop('css needs to be a list of two elements of the
                                                same length') 
 
 attributes <- attributes(tableHTML)
 
 #create style
 css_comp <- paste0(css[[1]], ':', css[[2]], ';')
 css_comp <- paste(css_comp, collapse = '')
 
 style <- paste0('style="', css_comp, '"')
 
 tableHTML <- sub('thead style=', 'thead', tableHTML)
 tableHTML <- sub('thead', paste0('thead ', style), tableHTML)
 tableHTML <- sub(';""', ';', tableHTML)
 
 attributes(tableHTML) <- attributes
 
 tableHTML
 
}

