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
#' @param color A character string to specify the desired color. It can be 
#' either a color name, or a hexadecimal string of the form "#rrggbb".
#'
#' @param total_rows A numeric atomic vector with the indices 
#' of the total/subtotal rows. If total_rows is \code{NULL}, the last row 
#' will be highlighted.
#'
#' @return A tableHTML object.
#'
#' @examples
#' x1 <- sample(1:100, 12)
#' x2 <- sample(1:100, 12)
#' x3 <- sample(1:100, 12)
#' df <- data.frame(Month = month.abb, x1, x2, x3,
#'                  stringsAsFactors = FALSE)
#' df[nrow(df)+1,]<- c('Total', sum(x1), sum(x2), sum(x3))
#' 
#' df %>%
#'   tableHTML(widths = rep(50, 4), rownames = FALSE) %>%
#'   add_theme_totals(color = 'darkred')
#' 
#' df_q <-rbind(df[1:3, ], c('Sum1', sum(x1[1:3]), sum(x2[1:3]), sum(x3[1:3])),
#'       df[4:6, ], c('Sum2', sum(x1[4:6]), sum(x2[4:6]), sum(x3[4:6])), 
#'       df[7:9, ], c('Sum3', sum(x1[7:9]), sum(x2[7:9]), sum(x3[7:9])),
#'       df[10:12, ], c('Sum4', sum(x1[10:12]), sum(x2[10:12]), sum(x3[10:12]))) 
#' 
#' df_q %>% 
#'   tableHTML(widths = rep(50, 5), rownames = FALSE, 
#'             row_groups = list(c(4, 4, 4, 4), 
#'                               c('Q1', 'Q2', 'Q3', 'Q4'))) %>%
#'   add_theme_totals(color = '#009999', c(4, 8, 12, 16))
#'
#' @export
add_theme_totals <- function(tableHTML, color, total_rows=NULL) 
{
  n_rows <- attr(tableHTML, "nrows")
  n_cols <- attr(tableHTML, "ncols")
  second_headers <- attr(tableHTML, "second_headers_data")
  exist_second_header <- !is.null(second_headers)
  rownames <- attr(tableHTML, "rownames")
  row_groups <- attr(tableHTML, "row_groups_data")
  exist_row_groups <- !is.null(row_groups)
  
  background_color <- paste0('rgba(', paste0(col2rgb(color), collapse = ','), ',0.7)')
  background_color_1 <- paste0('rgba(', paste0(col2rgb(color), collapse = ','), ',0.3)')
  background_color_2 <- paste0('rgba(', paste0(col2rgb(color), collapse = ','), ',0.1)')
  
  # row indices to style
  x_rows <- 1 + exist_second_header
  rows <- 1:(n_rows + x_rows)
  # seperate totals form the rest of the rows
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
    add_css_row(list(c('background', 'color'), 
                     c(color, 'white')), 
                rows = total_rows)
  # style the rest of the table
  tableHTML <- tableHTML %>% 
    add_css_table(css = list(c('border'), c(paste0('3px solid ', color)))) %>% 
    add_css_header(css = list(c('background', 'color'), 
                              c(background_color, 'white')),
                   headers = cols) %>% 
    add_css_row(css = list(c('border-top'), c(paste0('3px solid ', color))), 
                rows = rows) %>% 
    add_css_row(css = list('background', background_color_1), rows = odd(rows)) %>% 
    add_css_row(css = list('background', background_color_2), rows = even(rows)) %>% 
    add_css_column(css = list(c('text-align'), c('center')), 
                   columns = cols) %>% 
    replace_html('border=1', '')
  
  if(rownames){
    tableHTML <- tableHTML %>% 
      add_css_column(css = list(c('background', 'color'), 
                                c(background_color, 'white')), columns = 0)
  }
  if(exist_second_header){
    
    tableHTML <- tableHTML %>% 
      add_css_second_header(css = list(c('border'), 
                                       c(paste0('2px solid ', color))),
                            second_headers = 1:length(second_headers[[1]])) %>% 
      add_css_column(css = list(c('border-right'), 
                                c(paste0('2px solid ', color))),
                     columns = cumsum(second_headers[[1]])-x_cols)
  }
  if(exist_row_groups){
    tableHTML <- tableHTML %>% add_css_column(css = list(c('background', 'color', 'border-right'), 
                                                         c(background_color, 'white', paste0('2px solid ', color))),
                                              columns = -1)
  }
  tableHTML 
}


