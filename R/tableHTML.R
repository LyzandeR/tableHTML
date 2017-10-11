#' Create an easily css-ible HTML table
#'
#' The purpose of \code{tableHTML} is to create easily css-ible HTML tables
#' that are compatible with R shiny. The exported HTML table will contain separate ids
#' or classes for headers, columns, second headers (if any) and the table itself
#' (in case you have multiple tables) in order to create a
#' complex css file very easily. ids and classes are explained in detail in
#' the details section.
#'
#' \code{tableHTML} will create an HTML table with defined ids and classes for rows and columns.
#' In particular:
#' \itemize{
#'   \item \strong{Table:} Will get the class from the class argument in the function.
#'   \item \strong{Columns:} Will get an id which will be of the form tableHTML_column_x (where x
#'       is the column position). If rownames exist these will get the tableHTML_rownames id. If
#'       row groups exist these will get the tableHTML_row_groups id. Check the \code{add_css_column}
#'       function for examples.
#'   \item \strong{Headers:} Will get an id of the form tableHTML_header_x (where x is the header position).
#'       For example the first header will have the id tableHTML_header_1, the second header
#'       will have tableHTLM_header_2 and so on. If rownames exist these will get the tableHTML_header_0 id.
#'   \item \strong{Second_Header:} Will get an id of the form tableHTML_second_header_x (where x is the
#'       second header position). For example the first second_header will have the id tableHTML_second_header_1,
#'       the second header will have tableHTML_second_header_2 and so on.
#' }
#'
#' Notice that rows do not get a specific id or class.
#'
#' If you would like to use a non-collapsed table i.e. leave spacing between cells, then
#' you would need to use the \code{collapse} argument. Setting it to separate would create a
#' non-collapsed table. However, this choice will not work in shiny. The reason is that shiny
#' uses \code{table {border-collapse: collapse; border-spacing:0;}} in its css by default through
#' bootstrap 3. In order to overcome this problem in shiny, \code{collapse} needs to be set to
#' separate_shiny instead of separate. By setting collapse to separate_shiny tableHTML uses
#' \code{!important} in order to overwrite the standard behaviour of bootstrap 3. \code{!important}
#' needs to be used with caution since it overwrites css styles, so unless you are using shiny
#' (or any other place where the above css is automatically loaded) you should be using
#' \code{collapse = 'separate'}.
#'
#' Printing the table will result in rendering it in R studio's viewer
#' with the print.tableHTML method if using Rstudio otherwise it will use the default
#' browser. Use \code{print(tableHTML(obj), viewer = FALSE)} or \code{str(tableHTML(obj))}
#' to view the actual html code.
#'
#' @param obj Needs to be a data.frame or a matrix or an arbitrary object that has the
#'   data.frame class and can be coersible to a data.frame (e.g data.table, tbl, etc.)
#'
#' @param rownames Can be TRUE or FALSE. Defaults to TRUE. Whether the obj's rownames
#'   will be inlcuded.
#'
#' @param class Character string. Specifies the table's class. Convinient if you have multiple
#'   tables. Default is table_xxxx (random 4-digit number).
#'
#' @param headers character vector. The headers for the HTML table. If not provided the original
#'   data.frame headers will be used.
#'
#' @param widths Needs to be a numeric atomic vector with the column widths. Widths are in pixels.
#'
#' @param second_headers A list of two elements of the same length. The first element will contain
#'   the column spans (i.e. a numeric atomic vector) whereas the second element will contain the
#'   names (i.e. a character atomic vector). See the examples for more info. Defauls to NULL.
#'
#' @param row_groups A list of two elements of the same length. The first element will contain
#'   the row spans (i.e. a numeric atomic vector) whereas the second element will contain the
#'   names (i.e. a character atomic vector). See the examples for more info. Defauls to NULL.
#'
#' @param caption Character string. The table's caption.
#'
#' @param footer Character string. The table's footer. This gets added below the table and it
#'   should not be confused with tfooter.
#'
#' @param border An integer. Specifies the border of the table. Defaults to 1. 0 removes borders
#'   from the table. The higher the number the thicker the table's outside border.
#'
#' @param collapse Whether to collapse the table or not. By default the tables are collapsed. The
#'   choices for this argument are 'collapse', 'separate' and 'separate_shiny'. Check the details
#'   about which one to use.
#'
#' @param spacing Character string. This is only used if collapse is either separate or
#'   separate_shiny and sets the spacing between the table's cells. It defaults to 2px. Can be one
#'   or two length values (provided as a string). If two length values are provided the first one
#'   sets the horizontal spacing whereas the second sets the vertical spacing. See the examples.
#'
#' @param theme Pick one of the provided themes. These can still be modified by extra css. Choices
#'   are: default, scientific, rstudio-blue. Column widths are not provided when you select a theme.
#'   Please use the width argument for column widths. Defaults to 'default' i.e. no css included.
#'
#' @param x A tableHTML object created from the \code{tableHTML} function.
#'
#' @param ... Optional arguments to print.
#'
#' @param viewer TRUE or FALSE. Defaults to TRUE. Whether or not to render the HTML table. If
#'   you are working on Rstudio (interactively) the table will be printed or Rstudio's viewer.
#'   If you are working on Rgui (interactively) the table will be printed on your default browser.
#'   If you set this to FALSE the HTML code will be printed on screen.
#'
#' @return A tableHTML object.
#'
#' @examples
#' tableHTML(mtcars)
#' tableHTML(mtcars, rownames = FALSE)
#' tableHTML(mtcars, class = 'table1')
#' tableHTML(mtcars, second_headers = list(c(3, 4, 5), c('col1', 'col2', 'col3')))
#' tableHTML(mtcars,
#'           widths = c(rep(50, 6), rep(100, 6)),
#'           second_headers = list(c(3, 4, 5), c('col1', 'col2', 'col3')))
#' tableHTML(mtcars, caption = 'This is a caption', footer = 'This is a footer')
#' tableHTML(mtcars,
#'           row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3')),
#'           widths = c(200, rep(50, 5), rep(100, 6)),
#'           rownames = FALSE)
#' tableHTML(mtcars,
#'           rownames = FALSE,
#'           widths = c(140, rep(50, 11)),
#'           row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3')),
#'           second_headers = list(c(3, 4, 5), c('col1', 'col2', 'col3')))
#' tableHTML(mtcars,
#'           rownames = FALSE,
#'           widths = c(140, rep(50, 11)),
#'           row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3')),
#'           second_headers = list(c(3, 4), c('col1', 'col2')),
#'           theme = 'scientific')
#' tableHTML(mtcars, theme = 'rshiny-blue', widths = c(140, rep(50, 11)))
#' tableHTML(mtcars, collapse = 'separate_shiny', spacing = '5px')
#' tableHTML(mtcars, collapse = 'separate', spacing = '5px 2px')
#'
#' @export
tableHTML <- function(obj,
                      rownames = TRUE,
                      class = paste0('table_', sample(1000:9999, 1)),
                      widths = NULL,
                      headers = NULL,
                      second_headers = NULL,
                      row_groups = NULL,
                      caption = NULL,
                      footer = NULL,
                      border = 1,
                      collapse = c('collapse', 'separate', 'separate_shiny'),
                      spacing = '2px',
                      theme = c('default', 'scientific', 'rshiny-blue')) {

  #CHECKS----------------------------------------------------------------------------------------
  #adding checks for obj
  if (is.matrix(obj)) {
   obj <- as.data.frame(obj)
  } else if (inherits(obj, 'data.frame')) {
   obj <- as.data.frame(obj)
  } else {
   stop('obj needs to be either a data.frame or a matrix')
  }

  #checks for second header
  if (!is.null(second_headers)) {
   if (!is.list(second_headers)) {
    stop('second_headers needs to be a list')
   }
   if (length(second_headers) != 2L) {
    stop('second_headers needs to be a list of length two')
   }
   if (!is.numeric(second_headers[[1]])) {
    stop("second_headers\'s first element needs to be numeric")
   }
   if (!is.character(second_headers[[2]])) {
    stop("second_headers\'s second element needs to be character")
   }
   if (length(second_headers[[1]]) != length(second_headers[[2]])) {
    stop("second_headers\'s  elements need to have the same length")
   }
  }

  #checks for widths
  if (rownames == TRUE & !is.null(widths) & is.null(row_groups)) {
   if (length(widths) != ncol(obj) + 1) stop('widths must have the same length as the columns + 1')
  } else if (rownames == FALSE & !is.null(widths) & is.null(row_groups)) {
   if (length(widths) != ncol(obj)) stop('widths must have the same length as the columns')
  } else if (rownames == TRUE & !is.null(widths) & !is.null(row_groups)) {
   if (length(widths) != ncol(obj) + 2) stop('widths must have the same length as the columns + 2')
  } else if (rownames == FALSE & !is.null(widths) & !is.null(row_groups)) {
   if (length(widths) != ncol(obj) + 1) stop('widths must have the same length as the columns + 1')
  }

  #check for the border argument
  if (!is.numeric(border)) stop('border needs to be an integer')
  border <- as.integer(border)

  #check for the theme options
  theme <- match.arg(theme)

  #make sure collapse has the right argument
  collapse <- match.arg(collapse)

  #make sure the first character of spacing is a number
  if (collapse %in% c('separate', 'separate_shiny')) {
   distances <- gsub('\\s+', ' ', spacing)
   distances <- strsplit(distances, ' ')[[1]]
   for (i in distances) {
    if (is.na(as.numeric(sub('[[:alpha:]]+', '', i)))) {
     stop('distances in spacing need to start with a number e.g. "1px 2px"')
    }
   }
  }

  if (!is.null(headers)) {
   if (length(headers) != length(names(obj))) {
    stop('The length of the headers needs to be the same as number of columns of the data.frame')
   }
  } else {
   headers <- names(obj)
  }

  #escape character > and < in the data and headers because it will close or open tags
  obj[sapply(obj, is.fachar)] <- lapply(obj[sapply(obj, is.fachar)], function(x) {
   x <- gsub('>', '&#62;', x)
   x <- gsub('<', '&#60;', x)
   x
  })
  headers <- gsub('>', '&#62;', force(headers))
  headers <- gsub('<', '&#60;', force(headers))

  #make sure headers do not contain empty string
  headers[headers == ''] <- ' '

  #headers to be exported
  headers_exported <- headers

  #HEADERS---------------------------------------------------------------------------------------

  #adding new headers in case the user has provided those
  names(obj) <- headers

  #taking into account rownames
  if (rownames == TRUE) {
    headers <- paste('<tr>',
                     '  <th id="tableHTML_header_1"> </th>',
                     paste(vapply(seq_along(names(obj)) + 1, function(x) {
                              paste0('  <th id="tableHTML_header_', x, '">',
                                    names(obj)[x - 1],
                                    '</th>')
                              },
                              FUN.VALUE = character(1)),
                              collapse = '\n'),
                     '</tr>\n',
                     sep = '\n')
  } else {
    headers <- paste('<tr>',
                     paste(vapply(seq_along(names(obj)), function(x) {
                              paste0('  <th id="tableHTML_header_', x, '">', names(obj)[x], '</th>')
                              },
                              FUN.VALUE = character(1)),
                              collapse = '\n'),
                     '</tr>\n',
                     sep = '\n')
  }

  #SECOND HEADERS--------------------------------------------------------------------------------
  #transformation of second headers if theme is scientific
  if (theme == 'scientific' & !is.null(second_headers)) {
   indices <- which(!second_headers[[2]] %in% '')

   if (!is.null(rownames) & !is.null(row_groups)) {
    extra <- 2
   } else if (is.null(rownames) & !is.null(row_groups)) {
    extra <- 1
   } else if (!is.null(rownames) & is.null(row_groups)) {
    extra <- 1
   } else {
    extra <- 0
   }

   sum_of_column_span <- sum(second_headers[[1]])

   if (ncol(obj) > sum(second_headers[[1]]) + extra) {
    second_headers[[1]] <- c(second_headers[[1]],
                            rep(1, ncol(obj) - sum_of_column_span + extra))
    second_headers[[2]] <- c(second_headers[[2]],
                            rep('', ncol(obj) - sum_of_column_span + extra))
   }
  }

  #adding second headers if available
  if (!is.null(second_headers)) {
    over_header <-
      paste('<tr>',
            paste(vapply(seq_along(second_headers[[1]]), function(x) {
                    paste0('  <th colspan=',
                           second_headers[[1]][x],
                           ' id="tableHTML_second_header_',
                           x,
                           '">',
                    second_headers[[2]][x],
                    '</th>')
                    },
                   FUN.VALUE = character(1)),
                  collapse = '\n'),
           '</tr>\n',
           sep = '\n')
  } else {
    over_header <- NULL
  }

  #TABLE'S BODY----------------------------------------------------------------------------------
  #adding body
  content <- lapply(seq_along(names(obj)), function(x) {
    paste0('  <td id="tableHTML_column_', x, '">', obj[[x]], '</td>\n')
  })

  #adding rownames in the body
  if (rownames == TRUE) {
    content <- c(list(paste0('  <td id="tableHTML_rownames">',
                             row.names(obj),
                             '</td>\n')),
                 content)
  }

  content <- cbind('<tr>\n', do.call(cbind, content), '</tr>')
  content <- paste(apply(content, 1, paste, collapse = ''), collapse = '\n')

  #WIDTHS----------------------------------------------------------------------------------------
  #setting column widths if any
  if (!is.null(widths)) {
   colwidths <- paste(
    vapply(widths, function(x) {
     paste0('<col width="', x, '">')
    }, FUN.VALUE = character(1)),
    collapse = '\n')
   colwidths <- paste0(colwidths, '\n')
  } else {
   colwidths <- NULL
  }

  #CAPTION---------------------------------------------------------------------------------------
  #adding a caption
  if (!is.null(caption)) {
   caption <- paste0('<caption>', caption, '</caption>\n')
  }

  #FOOTER----------------------------------------------------------------------------------------
  #adding a footer
  if (!is.null(footer)) {
   footer <- paste0('<caption id="footer" align="bottom">', footer, '</caption>\n')
  }

  #PUTTING IT ALL TOGETHER-----------------------------------------------------------------------
  #adding all the components in one html table

  #taking into account the theme
  if (theme %in% c('scientific', 'rshiny-blue')) {
   border <- 0
  }

  htmltable <-
    htmltools::HTML(paste0('\n<table style="border-collapse:collapse;" class=',
                           class,
                           ' border=',
                           border,
                           '>\n',
                           caption,
                           footer,
                           colwidths,
                           '<thead>\n',
                           over_header,
                           headers,
                           '</thead>\n',
                           '<tbody>\n',
                           content,
                           '\n',
                           '</tbody>\n',
                           '</table>',
                           collapse = ''))

  #ADDING ROW GROUPS-----------------------------------------------------------------------------
  #adding row groups in table

  if (!is.null(row_groups)) {

   htmltable <-
    sub('<tr>\n  <th id="tableHTML_header_1">',
        '<tr>\n  <th id="tableHTML_header_0"></th>\n  <th id="tableHTML_header_1">',
        htmltable)

   rows <- Reduce('+', row_groups[[1]], accumulate = TRUE)
   rows <- c(1, rows[-length(rows)] + 1)

   if (!is.null(second_headers)) {
    rows <- rows + 3
   } else {
    rows <- rows + 2
   }

   splits <- strsplit(htmltable, '<tr')

   splits[[1]][2:length(splits[[1]])] <-
    vapply(splits[[1]][2:length(splits[[1]])], function(x) paste0('<tr', x),
           FUN.VALUE = character(1))

   splits[[1]][rows] <-
    mapply(function(x, y, z) {
     x <- sub('<tr>\n',
              paste0('<tr>\n  <td id="tableHTML_row_groups" rowspan="',
                     z,
                     '">',
                     y,
                     '</td>\n'
                     ),
              x)},
     splits[[1]][rows],
     row_groups[[2]],
     row_groups[[1]]
    )

   htmltable <- paste(splits[[1]], collapse = '')

   htmltable <- htmltools::HTML(htmltable)
  }

  #ADDING CLASS AND RETURNING--------------------------------------------------------------------
  #add class and attribute and then return htmltable
  class(htmltable) <- c('tableHTML', class(htmltable))
  attr(htmltable, 'headers') <- headers_exported
  attr(htmltable, 'nrows') <- nrow(obj)
  attr(htmltable, 'ncols') <- ncol(obj)
  attr(htmltable, 'col_classes') <- unname(sapply(obj, function(x) class(x)))
  attr(htmltable, 'rownames') <- rownames
  attr(htmltable, 'row_groups') <- ifelse(is.null(row_groups), FALSE, TRUE)
  attr(htmltable, 'second_headers') <- ifelse(is.null(second_headers), FALSE, TRUE)

  #ADDING THEMES---------------------------------------------------------------------------------
  #Will use the add_css family

  #theme scientific
  if (theme == 'scientific') {

   htmltable <-
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
        htmltable)

   if (!is.null(second_headers)) {

    htmltable <-
      htmltable %>%
       add_css_row(css = list('border-top', '3px solid black'), rows = 1) %>%
       add_css_row(css = list('border-bottom', '2px solid black'), rows = 2) %>%
       add_css_row(css = list('border-bottom', '3px solid black'), rows = nrow(obj) + 2) %>%
       add_css_column(css = list('text-align', 'center'), columns = names(obj)) %>%
       add_css_footer(css = list(c('text-align', 'margin-top'), c('left', '3px'))) %>%
       add_css_second_header(css = list('border-bottom', '3px solid black'),
                             second_headers = indices) %>%
       add_css_second_header(css = list('border-top', '3px solid black'),
                             second_headers =  1:length(second_headers[[2]])) %>%
       add_css_column(css = list('vertical-align', 'top'), columns = 'row_groups')
   } else {
    htmltable <-
      htmltable %>%
      add_css_row(css = list('border-top', '3px solid black'), rows = 1) %>%
      add_css_row(css = list('border-bottom', '2px solid black'), rows = 1) %>%
      add_css_row(css = list('border-bottom', '3px solid black'), rows = nrow(obj) + 1) %>%
      add_css_column(css = list('text-align', 'center'), columns = names(obj)) %>%
      add_css_footer(css = list(c('text-align', 'margin-top'), c('left', '2px'))) %>%
      add_css_column(css = list('vertical-align', 'top'), columns = 'row_groups')

   }

  } else if (theme == 'rshiny-blue') {

   if (!is.null(second_headers)) {

    htmltable <-
     htmltable %>%
     add_css_row(css = list('background-color', '#428bca'), rows = 1:2) %>%
     add_css_row(css = list('background-color', '#f2f2f2'), rows = odd(3:(nrow(obj) + 2))) %>%
     add_css_column(css = list('text-align', 'center'), columns = names(obj)) %>%
     add_css_column(css = list(c('vertical-align', 'background-color'), c('top', 'white')),
                    columns = 'row_groups') %>%
     add_css_footer(css = list(c('text-align', 'margin-top'), c('left', '2px'))) %>%
     add_css_second_header(css = list(c('font-size', 'height'), c('25px', '30px')),
                           second_headers = 1:length(second_headers[[2]]))

   } else {

    htmltable <-
     htmltable %>%
     add_css_row(css = list('background-color', '#428bca'), rows = 1) %>%
     add_css_row(css = list('background-color', '#f2f2f2'), rows = odd(2:(nrow(obj) + 1))) %>%
     add_css_column(css = list('text-align', 'center'), columns = names(obj)) %>%
     add_css_column(css = list(c('vertical-align', 'background-color'), c('top', 'white')),
                    columns = 'row_groups') %>%
     add_css_footer(css = list(c('text-align', 'margin-top'), c('left', '2px')))

   }

  }

  #Collapse table according to collapse argument
  if (collapse == 'separate') {
   htmltable <- replace_html(htmltable,
                             'style="border-collapse:collapse;"',
                             paste0('style="border-collapse:separate;border-spacing:',
                                    spacing,
                                    ';"'))
  }
  if (collapse == 'separate_shiny') {
   htmltable <- replace_html(
    htmltable,
    'style="border-collapse:collapse;"',
    paste0('style="border-collapse:separate !important;border-spacing:',
           spacing,
           '; !important"'))
  }

  #return
  htmltable
}

#DECLARING S3 METHOD FOR PRINT-----------------------------------------------------------------

#' @rdname tableHTML
#' @export
print.tableHTML <- function(x, viewer = TRUE, ...) {

 if (interactive() & viewer == TRUE) {

  rstudioviewer <- getOption("viewer")
  file <- tempfile(fileext = ".html")
  htmlfile <- htmltools::HTML(paste('<html>\n<body>',
                                    x,
                                    '</body>\n</html>',
                                    sep = '\n'))
  cat(htmlfile, file = file)

  if (is.function(rstudioviewer)) {
   rstudioviewer(file)
  } else {
   utils::browseURL(file)
  }

 } else if (viewer == FALSE | !interactive()) {
  cat(x, ...)
 }

 invisible(x)

}



