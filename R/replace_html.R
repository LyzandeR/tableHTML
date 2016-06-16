#' Replaces a tableHTML string with another 
#'
#' \code{replace_html} replaces a tableHTML string with another
#' 
#' \code{replace_html} replaces a tableHTML string with another. The function calls sub and gsub
#'   internally (according to the replace_all argument) to do the replacements but in a safe way in 
#'   order to preserve the class of the tableHTML object. 
#'
#' @param tableHTML A tableHTML object created by the tableHTML function.
#'
#' @param pattern A tableHTML string to be replaced. Regex is allowed.
#' 
#' @param replacement A replacement for the matched pattern.
#' 
#' @param replace_all TRUE or FALSE. If TRUE gsub is used internally and all the pattern occurrances
#'   will be replaced. If FALSE sub is used internally and only the first occurance will be
#'   replaced. Defaults to FALSE.
#'   
#' @param ... Additional arguments passed on to sub or gsub.  
#'
#' @return A tableHTML object.
#' 
#' @family Pattern Matching and Replacement
#' @seealso \code{\link{gsub}} or \code{\link{sub}}     
#'         
#' @examples
#' a <- mtcars %>% 
#'   tableHTML() %>% 
#'   add_css_row(css = list('background-color', 'lightblue'), rows = 1)
#'     
#' a %>% 
#'   replace_html('lightblue', 'green')
#' 
#' 
#' @export
replace_html <- function(tableHTML, 
                         pattern, 
                         replacement, 
                         replace_all = FALSE,
                         ... ) {
 
 #checks
 if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
 
 #check the dots
 args <- list(...)
 
 #make arguments list
 sub_args <- paste(names(formals(sub))[!names(formals(sub)) %in% 
                                       c('pattern', 'replacement', 'x')], collapse = ', ')
 

 if (!all(names(args) %in% names(formals(gsub)))) {
  stop(paste('gsub or sub can have the following arguments:', sub_args, collapse = ' '))
 } 
 
 classes <- class(tableHTML)
 
 #replacing string
 if (replace_all) {
  tableHTML <- gsub(pattern = pattern, replacement = replacement, x = tableHTML, ...)
 } else {
  tableHTML <- sub(pattern = pattern, replacement = replacement, x = tableHTML, ...)
 }
 
 class(tableHTML) <- classes
 
 tableHTML
 
}

