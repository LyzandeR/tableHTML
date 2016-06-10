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
#'       The final class will be of the form table_<class_name>.
#'   \item \strong{Columns:} Will get an id which will be the same as the column name
#'   \item \strong{Headers:} Will get an id of the form header_<header index>. 
#'       For example the first header will have the id header_1, the second header 
#'       will have header_2 and so on.
#'   \item \strong{Second_Header:} Will get an id of the form overheader_<second header index>.  
#'       For example the first second_header will have the id overheader_1, the second header  
#'       will have overheader_2 and so on.
#' }
#' 
#' Notice that rows do not get a specific id or class as they are easily accessible from css 
#' anyway.
#'
#' @param obj Needs to be a data.frame or a matrix or an arbitrary object that has the 
#'   data.frame class and can be coersible to a data.frame (e.g data.table, tbl, etc.)
#'
#' @param rownames Can be TRUE or FALSE. Defaults to TRUE. Whether the obj's rownames
#'   will be inlcuded.
#'   
#' @param class Character string. Specifies the table's class. Convinient if you have multiple
#'   tables. Default is table_<data_frame_name>.
#'   
#' @param widths Needs to be a numeric atomic vector with the column widths. Widths are in pixels.  
#'
#' @param second_header A list of two elements of the same length. The first element will contain
#'   the column spans (i.e. a numeric atomic vector) whereas the second element will contain the
#'   names (i.e. a character atomic vector). See the examples for more info. Defauls to NULL. 
#'   
#' @param caption Character string. The table's caption. 
#' 
#' @param footer Character string. The table's footer. This gets added below the table and it
#'   should not be confused with tfooter. 
#'
#' @return An tableHTML object. Printing the table will result in rendering it in R studio's viewer
#'         with the print.tableHTML method. Use \code{str(tableHTML)} to view the actual html code.
#'         
#' @examples 
#' tableHTML(mtcars)
#' tableHtML(mtcars, rownames = FALSE)
#' tableHTML(mtcars, class = 'table1')
#' tableHTML(mtcars, second_header = list(c(3, 4, 5), c('col1', 'col2', 'col3')))
#' tableHTML(mtcars, 
#'           widths = c(rep(50, 6), rep(100, 6)) , 
#'           second_header = list(c(3, 4, 5), c('col1', 'col2', 'col3')))
#' tableHTML(mtcars, caption = 'This is a caption', footer = 'This is a footer')
#' 
#' @export
tableHTML <- function(obj, 
                      rownames = TRUE,
                      class = paste0('table_', deparse(substitute(obj))),
                      widths = NULL,
                      second_header = NULL,
                      caption = NULL,
                      footer = NULL) {
     
  #CHECKS----------------------------------------------------------------------------------------
  #adding checks for obj
  if (is.matrix(obj)) {
   obj <- as.data.frame(obj)
  } else if (!inherits(obj, 'data.frame')) {
   stop('obj needs to be either a data.frame or a matrix')  
  }

  #checks for second header
  if (!is.null(second_header)) {
   if (!is.list(second_header)) {
    stop('second_header needs to be a list')
   }
   if (length(second_header) != 2L) {
    stop('second_header needs to be a list of length two')
   }
   if (!is.numeric(second_header[[1]])) {
    stop("second_header\'s first element needs to be numeric")
   }
   if (!is.character(second_header[[2]])) {
    stop("second_header\'s second element needs to be character")
   }
   if (length(second_header[[1]]) != length(second_header[[2]])) {
    stop("second_header\'s  elements need to have the same length")
   }
  }
  
  #checks for widths
  if(rownames == TRUE & !is.null(widths)) {
   if (length(widths) != ncol(obj) + 1) stop('widths need to have the same length as the columns')
  } else if(rownames == FALSE & !is.null(widths)) {
   if (length(widths) != ncol(obj)) stop('widths need to have the same length as the columns')
  }
   
  #HEADERS---------------------------------------------------------------------------------------
  #taking into account rownames
  if (rownames == TRUE) {
    headers <- paste('<tr>',
                     '  <th id="header_1"> </th>', 
                     paste(vapply(seq_along(names(obj)) + 1, function(x) {
                              paste0('  <th id="header_', x, '">', 
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
                              paste0('  <th id="header_', x, '">', names(obj)[x], '</th>') 
                              },
                              FUN.VALUE = character(1)),
                              collapse = '\n'),
                     '</tr>\n',
                     sep = '\n')
  }
  
  #SECOND HEADERS--------------------------------------------------------------------------------
  #adding second headers if needed
  if (!is.null(second_header)) {
    over_header <- 
      paste('<tr>', 
            paste(vapply(seq_along(second_header[[1]]), function(x) {
                    paste0('  <th colspan=', 
                           second_header[[1]][x], 
                           ' id="overheader_',  
                           x,
                           '">',
                    second_header[[2]][x],
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
  content <- lapply(names(obj), function(x) {
    paste0('  <td id="', x, '">', obj[[x]], '</td>\n')
  })
  
  #adding rownames in the body
  if (rownames == TRUE) {
    content <- c(list(paste0('  <td id="rownames">', 
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
  
  #FOOTER---------------------------------------------------------------------------------------
  #adding a footer
  if (!is.null(footer)) {
   footer <- paste0('<caption id="footer" align="bottom">', footer, '</caption>\n')
  }
  
  #PUTTING IT ALL TOGETHER-----------------------------------------------------------------------
  #adding all the components in one html table
  htmltable <- 
    htmltools::HTML(paste0('\n<table class=',
                           class, 
                           ' border=1 style="border-collapse: collapse;">\n',
                           caption,
                           footer,
                           over_header, 
                           colwidths,
                           headers, 
                           content, 
                           '\n',
                           '</table>', 
                           collapse = ''))
  
  class(htmltable) <- c('tableHTML', class(htmltable))
  
  htmltable
}

#' @rdname tableHTML
#' @export
print.tableHTML <- function(htmltable) {
 
 if (interactive()) {
  
   viewer <- getOption("viewer")
   file <- tempfile(fileext = ".html")
   htmlfile <- htmltools::HTML(paste('<html>\n<body>',
                               htmltable,
                               '</body>\n</html>',
                               sep = '\n'))
   cat(htmlfile, file = file)
   viewer(file)
   
 }
 
 invisible(htmltable)
 
}







