#' Make columns Editable
#'
#' \code{add_editable_column} will make the specified columns editable
#'
#' @param tableHTML A tableHTML object created by the tableHTML function.
#'
#' @param columns A character atomic vector with the names of the columns or a numeric atomic vector
#'   with the positions of the columns where the style definitions will be applied on. At least one
#'   column must be provided. If the rownames are included the column name is "tableHTML_rownames"
#'   and the position is 0. If row_groups are are included the column name is "tableHTML_row_groups"
#'   and the position is -1.
#'
#' @return A tableHTML object.
#'
#' @examples
#' tableHTML(mtcars) %>%
#'   add_editable_column(columns = 'mpg')
#'
#' tableHTML(mtcars,
#'           rownames = TRUE,
#'           widths = c(150, 100, rep(50, 11)),
#'           row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3'))) %>%
#'    add_css_column(css = list('background-color', 'lightgray'), columns = 'row_groups') %>%
#'    add_css_column(css = list('text-align', 'right'), columns = 'row_groups') %>%
#'    add_css_header(css = list('background-color', 'lightgray'), headers = 1) %>%
#'    add_editable_column(columns = -1:3)
#'
#' @export
add_editable_column <- function(tableHTML,
                                columns) {

 #checks
 if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')

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

 #keep attributes
 attributes <- attributes(tableHTML)

 #creating editable
 for (i in indices) {

  tableHTML <- add_div_editable(tableHTML, i)

 }

 attributes(tableHTML) <- attributes

 tableHTML

}
