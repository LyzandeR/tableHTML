#' Create a hyperlink to display as a link on a tableHTML
#'
#' \code{make_hyperlink} add the relevant <a> tag on a vector.
#' 
#' \code{make_hyperlink} The standard way to use this (although it can be used
#'   outside a tableHTML) is to convert a column with plain URLs into 
#'   clickable hyperlinks when you render the HTML table. **Make sure 
#'   the escape argument of tableHTML is set to FALSE for this to work as 
#'   expected.**
#'
#' @param vec A vector. Typically the column of a data.frame.
#'
#' @param message The hyperlinks name. If it is omitted, the actual link will
#'   be used as the name
#'
#' @return A character vector which will represent an HTML hyperlink
#' 
#' @examples 
#' #make sure the escape argument is set to FALSE for this to work
#' tableHTML(data.frame(mpg = make_hyperlink(mtcars$mpg)), escape = FALSE)
#' 
#' tableHTML(data.frame(mpg = make_hyperlink(mtcars$mpg, 1:32)), escape = FALSE)
#' 
#' @export
make_hyperlink <- function(vec, message = NULL) {
  
  #if message is not provided then use the actual link
  if (is.null(message)) {
    message <- vec
  }
  
  #construct a hyperlink from a link and a message
  paste0("<a href=", 
         vec, ">", 
         message , 
         "</a>")
  
}
