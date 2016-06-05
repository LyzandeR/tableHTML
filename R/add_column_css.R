#' Add css to tableHTML's columns 
#'
#' \code{add_css_column} will add css to a tableHTML's columns
#' 
#' \code{add_css_column} will add css to a tableHTML's columns.
#'
#' @param tableHTML A tableHTML object created by the tableHTML function.
#'
#' @param css A list of two elements with the corresponding css. The first element of the list
#'   should be an atomic vector with the style definitions (e.g. background-color). The second
#'   element will be an atomic vector with the same length as the first element, which will 
#'   contain the style definitions' values (e.g. red). Check the examples for more information.
#'   
#' @param column_names A character atomic vector with the names of the columns where the style
#'   definitions will be applied on. At least one column name must be provided. If the rownames
#'   are included the column name is "rownames".
#'
#' @return An tableHTML object. 
#'         
#' @examples
#' tableHTML(mtcars) %>% 
#'   add_css_column(css = list(c('background-color', 'border'), c('lightgray', '3px solid green')),
#'                  column_names = 'mpg') 
#' 
#' tableHTML(mtcars) %>% 
#'   add_css_column(css = list(c('background-color', 'border'), c('lightgray', '3px solid green')), 
#'                  column_names = c('mpg', 'disp', 'rownames'))
#' @export
add_css_column <- function(tableHTML, 
                           css, 
                           column_names) {
 
 #checks
 if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
 if (length(css[[1]]) != length(css[[2]])) stop('css needs to be a list of two elements of the
                                                same length') 
 
 tabHTML <- tableHTML
 
 #create style
 css_comp <- Map(function(x, y) paste0(x, ':', y, ';'), css[[1]], css[[2]])
 css_comp <- paste(css_comp, collapse = '')
 
 style <- paste0('style="', css_comp, '"')
 
 for (i in column_names) {
   tabHTML <- gsub(paste0('id="', i, '" style='), paste0('id="', i, '"'), tabHTML)
   tabHTML <- gsub(paste0('id="', i, '"'), paste0('id="', i, '" ', style), tabHTML)
   tabHTML <- gsub(';""', ';', tabHTML)
 }
 
 tabHTML
 
}

