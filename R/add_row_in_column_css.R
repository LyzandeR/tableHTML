#' Add css to tableHTML's columns' rows.
#'
#' \code{add_css_rows_in_column} will add css to a tableHTML's individual rows within a column
#'
#' \code{add_css_rows_in_column} will add css to a tableHTML's individual rows within a column.
#' Only one css style definition can be used, and multiple values (same length as the column)
#' will be applied to the rows within the column. As an example a list of different colours
#' can be provided for all the rows within a column.
#'
#' @param tableHTML A tableHTML object created by the tableHTML function.
#'
#' @param css A list of two elements. The first element of the list
#'   should be an atomic vector of length 1 with the style definition (e.g. background-color). 
#'   The second element will be an atomic vector with the same length as the column, which will
#'   contain the style definitions' values (e.g. red). Check the examples for more information.
#'
#' @param column A character atomic vector of length 1, with the name of the column 
#'   or a numeric atomic vector with the positions of the columns where the 
#'   style definitions will be applied on. Only one column must be provided. 
#'   If the rownames are included the column name is "tableHTML_rownames"
#'   and the position is 0. If row_groups are included the column name is "tableHTML_row_groups"
#'   and the position is -1.
#'
#' @return A tableHTML object.
#'
#' @examples
#' tableHTML(mtcars) %>%
#'   add_css_rows_in_column(css = list('background-color', 
#'                                    rep(c('red', 'green'), each = 16)),
#'                         column = 'mpg')
#'
#' tableHTML(mtcars) %>%
#'   add_css_column(css = list('border', '3px solid blue'),
#'                  columns = c('mpg', 'disp', 'rownames')) %>%
#'   add_css_rows_in_column(css = list(c('background-color'), 
#'                                    rep(c('red', 'green'), each = 16)),
#'                         column = 'mpg')
#'                         
#'                         
#' tableHTML(mtcars) %>%
#'   add_css_rows_in_column(css = list(c('background-color'), 
#'                                    rep(c('red', 'green'), each = 16)),
#'                         column = 'mpg') %>%
#'   add_css_column(css = list('border', '3px solid blue'),
#'                  columns = c('mpg', 'disp', 'rownames')) 
#'
#'
#' @export
add_css_rows_in_column <- function(tableHTML, css, column) {

  #the number of rows
  num_rows <- attr(tableHTML, 'nrows')
  
  #checks
  if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
  if (length(css[[1]]) != 1) stop('only one style definition can be provided')
  if (length(css[[2]]) != num_rows) stop('the values of the 
                                          style definitions must be
                                          as many as the data.frame rows')
  if (length(column) != 1) stop('only one column is allowed. If you need more you
                                 can chain multiple add_css_rows_in_column together!')
  
  #save attributes to use later
  attributes <- attributes(tableHTML)
  
  #convert to indices
  if (is.numeric(column) | suppressWarnings(!any(is.na(as.numeric(column))))) {
    indice <- column
  } else {
    indice <- which(attr(tableHTML, 'headers') %in% column)
    if ('row_groups' %in% column) {
      indice <- c(-1, indice)
    }
    if ('rownames' %in% column) {
      indice <- c(0, indice)
    }
  }
  
  if (length(indice) == 0) {
    stop('column not found in data')
  }

  #get the pattern index
  i <- get_pattern_column_css(indice)
  #make the ids per row
  additions <- paste0(i, ' style="', css[[1]], ':', css[[2]], ';"')
  
  #check for previous styles
  if (grepl(paste0(i, ' style='), tableHTML)) {
    i <- paste0(i, ' style=')
  }
  
  #split
  splits <- strsplit(tableHTML, i)[[1]]

  #paste together. Start from scratch binding everything together.
  tableHTML <- ''
  for (i in 1:num_rows) {
    tableHTML <- paste0(tableHTML, splits[i], additions[i])
  }
  tableHTML <- paste0(tableHTML, splits[length(splits)])
  tableHTML <- gsub(';""', ';', tableHTML)

  #add back attributes
  attributes(tableHTML) <- attributes
  
  #return
  tableHTML
  
}