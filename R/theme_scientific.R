theme_scientific <- function(tableHTML) {

 #attributes
 n_rows <- attr(tableHTML, 'nrows')
 n_cols <- attr(tableHTML, 'ncols')
 second_headers <- attr(tableHTML, 'second_headers_data')
 exist_second_header <- !is.null(second_headers)
 rownames <- attr(tableHTML, 'rownames')
 class <- attr(tableHTML, 'table_class')
 row_groups <- attr(tableHTML, 'row_groups_data')
 exist_row_groups <- !is.null(row_groups)

 #transformation of second headers if theme is scientific
 if (exist_second_header) {

  indices <- which(!second_headers[[2]] %in% '')

  if (rownames & exist_row_groups) {
   extra <- 2
  } else if (!exist_row_groups & exist_row_groups) {
   extra <- 1
  } else if (exist_row_groups & !exist_row_groups) {
   extra <- 1
  } else {
   extra <- 0
  }

  sum_of_column_span <- sum(second_headers[[1]])

  if (n_cols > sum(second_headers[[1]]) + extra) {
   second_headers[[1]] <- c(second_headers[[1]],
                           rep(1, n_cols - sum_of_column_span + extra))
   second_headers[[2]] <- c(second_headers[[2]],
                           rep('', n_cols - sum_of_column_span + extra))
  }

 }

 #theme scientific
 tableHTML <-
  sub(paste0('<td id="tableHTML_row_groups" rowspan="',
             row_groups[[1]][length(row_groups[[1]])],
             '">',
             row_groups[[2]][length(row_groups[[2]])],
             '</td>'),
      paste0('<td id="tableHTML_row_groups"',
             ' style="border-bottom:3px solid black;"',
'              rowspan="',
             row_groups[[1]][length(row_groups[[1]])],
             '">',
             row_groups[[2]][length(row_groups[[2]])],
             '</td>'),
       tableHTML)

 if (exist_second_header) {

  tableHTML <-
   tableHTML %>%
     add_css_row(css = list('border-top', '3px solid black'), rows = 1) %>%
     add_css_row(css = list('border-bottom', '2px solid black'), rows = 2) %>%
     add_css_row(css = list('border-bottom', '3px solid black'), rows = n_rows + 2) %>%
     add_css_column(css = list('text-align', 'center'), columns = 1:n_cols) %>%
     add_css_footer(css = list(c('text-align', 'margin-top'), c('left', '3px'))) %>%
     add_css_second_header(css = list('border-bottom', '3px solid black'),
                           second_headers = indices) %>%
     add_css_second_header(css = list('border-top', '3px solid black'),
                           second_headers =  1:length(second_headers[[2]])) %>%
     add_css_column(css = list('vertical-align', 'top'), columns = 'row_groups')
 } else {
  tableHTML <-
   tableHTML %>%
     add_css_row(css = list('border-top', '3px solid black'), rows = 1) %>%
     add_css_row(css = list('border-bottom', '2px solid black'), rows = 1) %>%
     add_css_row(css = list('border-bottom', '3px solid black'), rows = n_rows + 1) %>%
     add_css_column(css = list('text-align', 'center'), columns = 1:n_cols) %>%
     add_css_footer(css = list(c('text-align', 'margin-top'), c('left', '2px'))) %>%
     add_css_column(css = list('vertical-align', 'top'), columns = 'row_groups')

 }

 #set borders to 0
 tableHTML <- replace_html(tableHTML,
                           paste0('class=', class, ' border=1'),
                           paste0('class=', class, ' border=0'))


 #return
 tableHTML

}
