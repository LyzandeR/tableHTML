theme_rshiny_blue <- function(tableHTML) {

  #attributes
  n_rows <- attr(tableHTML, 'nrows')
  n_cols <- attr(tableHTML, 'ncols')
  second_headers <- attr(tableHTML, 'second_headers_data')
  exist_second_header <- !is.null(second_headers)
  length_second_header <- length(second_headers[[1]])
  class <- attr(tableHTML, 'table_class')

  #if there are second headers
  if (exist_second_header) {

   tableHTML <-
    tableHTML %>%
    add_css_row(css = list('background-color', '#428bca'), rows = 1:2) %>%
    add_css_row(css = list('background-color', '#f2f2f2'), rows = odd(3:(n_rows + 2))) %>%
    add_css_column(css = list('text-align', 'center'), columns = 1:n_cols) %>%
    add_css_column(css = list(c('vertical-align', 'background-color'),
                              c('top', 'white')),
                   columns = 'row_groups') %>%
    add_css_footer(css = list(c('text-align', 'margin-top'), c('left', '2px'))) %>%
    add_css_second_header(css = list(c('font-size', 'height'), c('25px', '30px')),
                          second_headers = 1:length_second_header)

  #if not
  } else {

   tableHTML <-
    tableHTML %>%
    add_css_row(css = list('background-color', '#428bca'), rows = 1) %>%
    add_css_row(css = list('background-color', '#f2f2f2'), rows = odd(2:(n_rows + 1))) %>%
    add_css_column(css = list('text-align', 'center'), columns = 1:n_cols) %>%
    add_css_column(css = list(c('vertical-align', 'background-color'),
                              c('top', 'white')),
                   columns = 'row_groups') %>%
    add_css_footer(css = list(c('text-align', 'margin-top'), c('left', '2px')))

  }

  #set borders to 0
  tableHTML <- replace_html(tableHTML,
                            paste0('class=', class, ' border=1'),
                            paste0('class=', class, ' border=0'))

  #return
  tableHTML

}
