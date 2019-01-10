#' Add a theme to a tableHTML with a total row.
#'
#' \code{add_theme_totals} will add an Excel-like theme to tableHTML
#'  and hightlights one or more total-rows.
#'
#' \code{add_theme_totals} will add an Excel-like theme to tableHTML.
#' Column widths are not provided with the theme.
#' Please use the width argument for column widths.
#'
#' @param tableHTML A tableHTML object.
#'
#' @param colour A character vector to specify the desired colour. It can contain
#' at most two colours.
#' Accepts colour names (as listed by \code{\link[=grDevices]{colours()}}),
#' as well as hexadecimal representation of the form "#rrggbb".
#'
#' If two colours are chosen, the first colour will be the dominant one, and row colouring
#' will alternate between the first and second colour.
#'
#' @param total_rows A numeric atomic vector with the indices
#' of the total/subtotal rows. If total_rows is \code{NULL}, the last row
#' will be highlighted.
#'
#' @param  id_column A boolean, if set to \code{TRUE} the first column will
#' be highlighted as an ID column.
#' Default is \code{FALSE}.
#'
#' @return A tableHTML object.
#'
#' @examples
#' # one total row
#' x1 <- sample(1:100, 12)
#' x2 <- sample(1:100, 12)
#' x3 <- sample(1:100, 12)
#'
#' df <- data.frame(Month = month.abb, x1, x2, x3,
#'                  stringsAsFactors = FALSE)
#'
#' df[nrow(df) + 1, ] <- c('Total', sum(x1), sum(x2), sum(x3))
#'
#' df %>%
#'   tableHTML(widths = rep(50, 4), rownames = FALSE) %>%
#'   add_theme_totals(colour = 'darkred')
#'
#' df %>%
#'   tableHTML(widths = rep(50, 4), rownames = FALSE) %>%
#'   add_theme_totals(colour = c('steelblue', 'green'))
#'
#'
#' # multiple subtotal rows
#' df_q <- rbind(
#'   df[1:3, ],
#'   c('Sum1', sum(x1[1:3]), sum(x2[1:3]), sum(x3[1:3])),
#'   df[4:6, ],
#'   c('Sum2', sum(x1[4:6]), sum(x2[4:6]), sum(x3[4:6])),
#'   df[7:9, ],
#'   c('Sum3', sum(x1[7:9]), sum(x2[7:9]), sum(x3[7:9])),
#'   df[10:12, ],
#'   c('Sum4', sum(x1[10:12]), sum(x2[10:12]), sum(x3[10:12])))
#'
#' df_q %>%
#'   tableHTML(widths = rep(50, 5),
#'             rownames = FALSE,
#'             row_groups = list(c(4, 4, 4, 4),
#'                               c('Q1', 'Q2', 'Q3', 'Q4'))) %>%
#'   add_theme_totals(colour = '#009999',
#'                    total_rows = c(4, 8, 12, 16))
#'
#' # Two colours and an id_column
#' df_q %>%
#'   tableHTML(widths = rep(50, 5),
#'             rownames = FALSE,
#'             row_groups = list(c(4, 4, 4, 4),
#'                               c('Q1', 'Q2', 'Q3', 'Q4'))) %>%
#'   add_theme_totals(colour = c('pink3', 'yellow2'),
#'                    total_rows = c(4, 8, 12, 16), id_column = TRUE)
#'
#' @export
add_theme_totals <- function(tableHTML, colour,
                             total_rows=NULL, id_column=FALSE)
{

 # default parameters
 if(missing(colour)){
  colour <- 'darkgreen'
 }

 # checks
 if (!inherits(tableHTML, 'tableHTML'))
  stop('tableHTML needs to be a tableHTML object')

 if (!is.null(total_rows))
  if (!is.numeric(total_rows))
   stop('total_rows should be either NULL or a numeric vector')

 if (length(colour) > 2)
  stop('colour should be a vector of at most two colours')

 if (!is.logical(id_column) || is.na(id_column))
  stop('id_column should be TRUE or FALSE')

 n_rows <- attr(tableHTML, "nrows")
 n_cols <- attr(tableHTML, "ncols")
 second_headers <- attr(tableHTML, "second_headers_data")
 exist_second_header <- !is.null(second_headers)
 rownames <- attr(tableHTML, "rownames")
 row_groups <- attr(tableHTML, "row_groups_data")
 exist_row_groups <- !is.null(row_groups)

 # prepare colours
 if(length(colour) == 1){
  colour <- c(colour, colour)
 }
 # add attributes for testing
 attr(tableHTML, 'theme') <- list('total_rows' = total_rows,
                                  'colours' =  colour,
                                  'id_column' = id_column)

 rgb_col <- col2rgb(colour)
 colour <-  paste0('rgba(', paste0(rgb_col[, 1], collapse = ','), ',1)')
 header_background <- paste0('rgba(', paste0(rgb_col[, 1], collapse = ','), ',0.7)')
 background_colour_1 <- paste0('rgba(', paste0(rgb_col[, 1], collapse = ','), ',0.3)')
 background_colour_2 <- paste0('rgba(', paste0(rgb_col[, 2], collapse = ','), ',0.1)')

 # row indices to style
 x_rows <- 1 + exist_second_header
 rows <- 1:(n_rows + x_rows)
 # separate totals from the rest of the rows
 if(!is.null(total_rows)){
  total_rows <- total_rows + x_rows
 }else{
  total_rows <- n_rows + x_rows
 }
 rows <- setdiff(rows, total_rows)

 # column indices to style
 x_cols <- rownames + exist_row_groups
 cols <- 1:(n_cols + x_cols)

 # style the total rows
 tableHTML <- tableHTML %>%
  add_css_row(list(c('background', 'colour'),
                   c(colour, 'white')),
              rows = total_rows)
 # style the rest of the table
 tableHTML <- tableHTML %>%
  add_css_table(css = list(c('border'), c(paste0('3px solid ', colour)))) %>%
  add_css_header(css = list(c('background', 'colour'),
                            c(header_background, 'white')),
                 headers = cols) %>%
  add_css_row(css = list(c('border-top'), c(paste0('3px solid ', colour))),
              rows = rows) %>%
  add_css_row(css = list('background', background_colour_2), rows = odd(rows)) %>%
  add_css_row(css = list('background', background_colour_1), rows = even(rows)) %>%
  add_css_column(css = list(c('text-align'), c('center')),
                 columns = cols) %>%
  replace_html('border=1', '')

 # special cases
 if(rownames){
  tableHTML <- tableHTML %>%
   add_css_column(css = list(c('background', 'colour'),
                             c(header_background, 'white')), columns = 0)
 }
 if(exist_second_header){

  tableHTML <- tableHTML %>%
   add_css_second_header(css = list(c('border', 'background', 'colour'),
                                    c(paste0('2px solid ', colour), header_background, 'white')),
                         second_headers = 1:length(second_headers[[1]])) %>%
   add_css_column(css = list(c('border-right'),
                             c(paste0('2px solid ', colour))),
                  columns = cumsum(second_headers[[1]])-x_cols)
 }
 if(exist_row_groups){
  tableHTML <- tableHTML %>%
   add_css_column(css = list(c('background', 'colour', 'border-right'),
                             c(header_background, 'white', paste0('2px solid ', colour))),
                  columns = -1)
 }
 if(id_column){
  tableHTML <- tableHTML %>%
   add_css_column(css = list(c('background', 'colour', 'border-right'),
                             c(header_background, 'white', paste0('2px solid ', colour))),
                  columns = 1)
 }
 tableHTML
}


