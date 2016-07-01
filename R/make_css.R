#' Create a css file
#'
#' \code{make_css} will create a css file which can also be used in shiny
#' 
#' \code{make_css} will create a css file which can also be used in shiny
#'
#' @param ... css style definitions
#'
#' @return A css file. 
#'         
#' @examples
#' make_css(list('table', c('text-align', 'font-size'), c('center', '20px')),
#'          list('th', c('text-align', 'font-size'), c('center', '20px')))
#' 
#' @export
make_css <- function(...) {
 
 css_defs <- list(...)
 
 all_css <- 
   lapply(css_defs, function(x) {
    
     css_comp <- paste0(x[[2]], ':', x[[3]], ';')
     style <- paste(css_comp, collapse = '\n  ')
     
     paste0(x[[1]], 
            ' {\n  ',
            style,
            '\n}\n')
    
   })
 
 htmltools::HTML(paste(all_css, collapse = '\n'))
}