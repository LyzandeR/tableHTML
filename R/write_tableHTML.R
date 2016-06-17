#' Writes the HTML code to a file 
#'
#' \code{write_tableHTML} will write the HTML code to a file
#' 
#' \code{write_tableHTML} will write the HTML code to a file.
#'
#' @param tableHTML A tableHTML object created by the tableHTML function.
#'
#' @param file A character string. This is the file name. You need to include the extention.
#' 
#' @param complete_html Either TRUE or FALSE. Defaults to FALSE. If TRUE then the <html> and <body>
#'   tags are also added in the file.
#'
#' @return The function itself returns nothing but a file is created. 
#'         
#' @examples
#' write_tableHTML(tableHTML(mtcars), file = 'myhtmlcode.html')
#' 
#' write_tableHTML(tableHTML(mtcars), file = 'myhtmlcode.html', complete_html = TRUE)
#' 
#' 
#' @export
write_tableHTML <- function(tableHTML, file, complete_html = FALSE) {
 
 #checks
 if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
 
 tabHTML <- tableHTML
 
 #create style
 if (complete_html) {
  
  tabHTML <- htmltools::HTML(paste('<!DOCTYPE html>\n<html>\n<body>',
                                   tabHTML,
                                   '</body>\n</html>',
                                   sep = '\n'))
  
 }
 
 cat(tabHTML, file = file)
 
 invisible(NULL)
 
}

