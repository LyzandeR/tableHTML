#' Add css to tableHTML's rows 
#'
#' \code{add_css_row} will add css to a tableHTML's rows
#' 
#' \code{add_css_row} will add css to a tableHTML's rows. The only thing you need to be cautious 
#' about is the rows argument. \code{headers} and \code{second_headers} are still considered rows.
#'
#' @param tableHTML A tableHTML object created by the tableHTML function.
#'
#' @param css A list of two elements with the corresponding css. The first element of the list
#'   should be an atomic vector with the style definitions (e.g. background-color). The second
#'   element will be an atomic vector with the same length as the first element, which will 
#'   contain the style definitions' values (e.g. red). Check the examples for more information.
#'   
#' @param rows A numeric atomic vector with the indices of the rows on which the style definitions
#'   will be applied. headers and second_headers are included in the rows.Default is NULL
#'   which means that it will be applied to all rows.
#'
#' @return An tableHTML object. 
#'         
#' @examples
#' tableHTML(mtcars) %>% 
#'   add_css_row(css = list(c('background-color', 'border'), c('lightgray', '3px solid green'))) 
#' 
#' tableHTML(mtcars) %>% 
#'   add_css_row(css = list(c('background-color', 'border'), c('lightgray', '3px solid green')), 
#'               rows = 1:33)
#'         
#' @export
add_css_row <- function(tableHTML, 
                        css, 
                        rows = NULL) {
 
 #checks
 if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
 if (length(css[[1]]) != length(css[[2]])) stop('css needs to be a list of two elements of the
                                                same length') 
 
 tabHTML <- tableHTML
 
 #create style
 css_comp <- Map(function(x, y) paste0(x, ':', y, ';'), css[[1]], css[[2]])
 css_comp <- paste(css_comp, collapse = '')
 
 style <- paste0('style="', css_comp, '"')
 
 splits <- strsplit(tabHTML, '<tr')
 
 splits[[1]][2:length(splits[[1]])] <- 
  vapply(splits[[1]][2:length(splits[[1]])], function(x) paste0('<tr', x), FUN.VALUE = character(1))
 
 if (is.null(rows)) {
  rows <- 2:length(splits[[1]]) 
 } else {
  rows <- rows + 1
 }
 
 splits[[1]][rows ] <- 
  vapply(splits[[1]][rows], function(x) {
   x <- sub('<tr style=', '<tr', x)
   x <- gsub('<tr', paste0('<tr ', style), x)
   x <- sub('" "', '', x)
   x
  }, FUN.VALUE = character(1))
 
 new_tab <- paste(splits[[1]], collapse = '')
 
 new_tab <- htmltools::HTML(new_tab)
 
 class(new_tab) <- c('tableHTML', class(new_tab))
 
 new_tab
 
}

