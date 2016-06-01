#' Pipe css
#'
#' Like dplyr and ggvis, tableHTML also uses the pipe function, \code{\%>\%} to chain
#' css functions. The pipe function originally came from the magrittr package.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A tableHTML and a function to apply to it
#' @examples
#' # Instead of
#'   add_css_row(tableHTML(mtcars),
#'               css = list(c('background-color', 'border'), c('lightgray', '3px solid green')))
#' # you can write
#'   mtcars %>%
#'     tableHTML() %>% 
#'     add_css_row(css = list(c('background-color', 'border'), c('lightgray', '3px solid green'))) 
NULL