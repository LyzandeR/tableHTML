#' Add css to the tbody tag 
#'
#' \code{add_css_tbody} will add css to the tbody tag i.e. to all table apart from the headers and
#'   second headers.
#' 
#' \code{add_css_thead} will add css to the tbody tag i.e. to all table apart from the headers and
#'   second headers.
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
#'   add_css_tbody(css = list('background-color', 'lightgray')) 
#'   
#' tableHTML(mtcars) %>% 
#'   add_css_tbody(css = list('background-color', 'lightgray')) %>%
#'   add_css_tbody(css = list('background-color', 'lightblue')) 
#'   
#' tableHTML(mtcars) %>% 
#'   add_css_tbody(css = list('background-color', 'lightgray')) %>%
#'   add_css_tbody(css = list('text-align', 'center'))
#'   
#' 
#' @export
add_css_tbody <- function(tableHTML, 
                          css) {
 
 #checks
 if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
 if (length(css[[1]]) != length(css[[2]])) stop('css needs to be a list of two elements of the
                                                same length') 
 
 tabHTML <- tableHTML
 
 #create style
 css_comp <- paste0(css[[1]], ':', css[[2]], ';')
 css_comp <- paste(css_comp, collapse = '')
 
 style <- paste0('style="', css_comp, '"')
 
 tabHTML <- sub('tbody style=', 'tbody', tabHTML)
 tabHTML <- sub('tbody', paste0('tbody ', style), tabHTML)
 tabHTML <- sub(';""', ';', tabHTML)
 
 tabHTML
 
}

