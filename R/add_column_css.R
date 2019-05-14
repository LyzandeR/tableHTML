#' Add css to tableHTML's columns
#'
#' \code{add_css_column} will add css to a tableHTML's columns
#'
#' \code{add_css_column} will add css to a tableHTML's columns. \code{add_css_column} will only
#'   add css to the columns without the headers or second headers (i.e. it only affects the td tag
#'   internally and not the th tag). If you want to add css to the headers or second headers please
#'   use \code{add_css_header} or \code{add_css_second_header}.
#'
#' @param tableHTML A tableHTML object created by the tableHTML function.
#'
#' @param css A list of two elements with the corresponding css. The first element of the list
#'   should be an atomic vector with the style definitions (e.g. background-color). The second
#'   element will be an atomic vector with the same length as the first element, which will
#'   contain the style definitions' values (e.g. red). Check the examples for more information.
#'
#' @param columns A character atomic vector with the names of the columns or a numeric atomic vector
#'   with the positions of the columns where the style definitions will be applied on. At least one
#'   column must be provided. If the rownames are included the column name is "tableHTML_rownames"
#'   and the position is 0. If row_groups are are included the column name is "tableHTML_row_groups"
#'   and the position is -1.
#'
#' @param column_names Deprecated. Please use the argument columns.
#'
#' @return A tableHTML object.
#'
#' @examples
#' tableHTML(mtcars) %>%
#'   add_css_column(css = list(c('background-color', 'border'), c('lightgray', '3px solid green')),
#'                  columns = 'mpg')
#'
#' tableHTML(mtcars) %>%
#'   add_css_column(css = list(c('background-color', 'border'), c('lightgray', '3px solid green')),
#'                  columns = c('mpg', 'disp', 'rownames'))
#'
#' tableHTML(mtcars,
#'           rownames = FALSE,
#'           widths = c(120, rep(50, 11)),
#'           row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3'))) %>%
#'   add_css_column(css = list('background-color', 'lightgray'), columns = 'row_groups') %>%
#'   add_css_header(css = list('background-color', 'lightgray'), headers = 1)
#'
#' tableHTML(mtcars,
#'           rownames = TRUE,
#'           widths = c(140, rep(50, 12)),
#'           row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3')),
#'           second_headers = list(c(3, 4), c('col1', 'col2'))) %>%
#'   add_css_column(list('background-color', 'green'), -1) %>%
#'   add_css_column(list('background-color', 'red'), c(0, 1))
#'
#' @export
add_css_column <- function(tableHTML,
                           css,
                           columns) {

 #checks
 if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
 if (length(css[[1]]) != length(css[[2]])) stop('css needs to be a list of two elements of the
                                                same length')

 if (is.numeric(columns) | suppressWarnings(!any(is.na(as.numeric(columns))))) {
  indices <- columns
 } else {
  indices <- which(attr(tableHTML, 'header') %in% columns)
  if ('row_groups' %in% columns) {
   indices <- c(-1, indices)
  }
  if ('rownames' %in% columns) {
   indices <- c(0, indices)
  }
  indices <- sort(indices)
 }

 attributes <- attributes(tableHTML)

 #create style
 css_comp <- paste0(css[[1]], ':', css[[2]], ';')
 css_comp <- paste(css_comp, collapse = '')

 style <- paste0('style="', css_comp, '"')

 for (i in indices) {

  if (identical(i, 0)) {

   tableHTML <- gsub(paste0('id="tableHTML_rownames" style='),
                     paste0('id="tableHTML_rownames"'), tableHTML)
   tableHTML <- gsub(paste0('id="tableHTML_rownames"'),
                     paste0('id="tableHTML_rownames" ', style), tableHTML)
   tableHTML <- gsub(';""', ';', tableHTML)

  } else if (identical(i, -1)) {

   tableHTML <- gsub(paste0('id="tableHTML_row_groups" style='),
                     paste0('id="tableHTML_row_groups"'), tableHTML)
   tableHTML <- gsub(paste0('id="tableHTML_row_groups"'),
                     paste0('id="tableHTML_row_groups" ', style), tableHTML)
   tableHTML <- gsub(';""', ';', tableHTML)

  } else {

  tableHTML <- gsub(paste0('id="tableHTML_column_', i, '" style='),
                    paste0('id="tableHTML_column_', i, '"'), tableHTML)
  tableHTML <- gsub(paste0('id="tableHTML_column_', i, '"'),
                    paste0('id="tableHTML_column_', i, '" ', style), tableHTML)
  tableHTML <- gsub(';""', ';', tableHTML)

  }
 }

 attributes(tableHTML) <- attributes

 tableHTML

}

