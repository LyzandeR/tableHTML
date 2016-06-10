#' Add css to tableHTML's footer 
#'
#' \code{add_css_footer} will add css to a tableHTML's footer
#' 
#' \code{add_css_footer} will add css to a tableHTML's footer.
#'
#' @param tableHTML A tableHTML object created by the tableHTML function.
#'
#' @param css A list of two elements with the corresponding css. The first element of the list
#'   should be an atomic vector with the style definitions (e.g. background-color). The second
#'   element will be an atomic vector with the same length as the first element, which will 
#'   contain the style definitions' values (e.g. red). Check the examples for more information.
#'
#' @return An tableHTML object. 
#'         
#' @examples
#' tableHTML(mtcars, footer = 'This is a footer') %>% 
#'   add_css_footer(css = list(c('color', 'font-size'), c('blue', '50px'))) 
#' 
#' tableHTML(mtcars, footer = 'This is a footer') %>% 
#'   add_css_footer(css = list(c('color', 'font-size'), c('blue', '50px'))) %>%
#'   add_css_footer(css = list('background-color', 'green'))
#' 
#' @export
add_css_footer <- function(tableHTML, css) {
 
 #checks
 if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
 if (length(css[[1]]) != length(css[[2]])) stop('css needs to be a list of two elements of the
                                                same length') 
 
 tabHTML <- tableHTML
 
 #create style
 css_comp <- Map(function(x, y) paste0(x, ':', y, ';'), css[[1]], css[[2]])
 css_comp <- paste(css_comp, collapse = '')
 
 style <- paste0('style="', css_comp, '"')
 
 tabHTML <- sub('<caption id="footer" align="bottom" style=', 
                '<caption id="footer" align="bottom"', 
                tabHTML)
 tabHTML <- sub('<caption id="footer" align="bottom"', 
                paste0('<caption id="footer" align="bottom" ', style), 
                tabHTML)
 tabHTML <- sub(';""', ';', tabHTML)
 
 tabHTML
 
}

